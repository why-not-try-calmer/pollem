{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Server
    ( startApp
    , app
    ) where

import           Control.Concurrent          (modifyMVar_, putMVar, readMVar,
                                              takeMVar, threadDelay)
import           Control.Monad.Except        (ExceptT, runExceptT)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader
import           Data.Aeson                  (ToJSON (toEncoding), encode,
                                              fromEncoding, fromJSON, toJSON)
import           Data.Aeson.Extra            (encodeStrict)
import qualified Data.ByteString             as B
import qualified Data.HashMap.Strict         as HMS
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Data.Text.Encoding
import           Database
import           Database.Redis              (Connection, PortID (PortNumber))
import qualified ErrorsReplies               as R
import           HandlersDataTypes
import           Mailer
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Scheduler                   (getNow, isoOrCustom, schedule)
import           Servant
import           System.Environment          (getEnvironment)
import           Workers

type API =
    "ask_token" :> ReqBody '[JSON] ReqAskToken :> Post '[JSON] RespAskToken :<|>
    "confirm_token" :> ReqBody '[JSON] ReqConfirmToken :> Post '[JSON] RespConfirmToken :<|>
    "create" :> ReqBody '[JSON] ReqCreate :> Post '[JSON] RespCreate :<|>
    "close":> ReqBody '[JSON] ReqClose :> Post '[JSON] RespClose :<|>
    "get" :> Capture "poll_id" Int :> QueryParam "secret" String :> Get '[JSON] RespGet :<|>
    "myhistory" :> ReqBody '[JSON] ReqMyHistory :> Post '[JSON] RespMyHistory :<|>
    "take" :> ReqBody '[JSON] ReqTake :> Post '[JSON] RespTake :<|>
    "warmup" :> Get '[JSON] RespWarmup

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = ask_token :<|> confirm_token :<|> create :<|> close :<|> get :<|> myhistory :<|> take :<|> warmup
    where
        ask_token :: ReqAskToken -> AppM RespAskToken
        ask_token (ReqAskToken email) = do
            let email_b = encodeUtf8 email
            env <- ask
            let mvar = pollmanager env
                hashed = B.init . B.tail . encodeStrict . hashEmail $ email_b
                asksubmit token = SAsk hashed token
            res <- liftIO $ do
                (n, gen) <- takeMVar mvar
                token <- createToken gen email_b
                let token_b = B.init . B.tail . encodeStrict $ token
                putMVar mvar (n, gen)
                sendEmail (makeSendGridEmail (sendgridconf env) token_b email_b) >>= \case
                    Right _ -> connDo (redisconn env) . submit $ asksubmit token_b
            case res of
                Left err  -> pure . RespAskToken . R.renderError $ err
                Right msg -> pure . RespAskToken . R.renderOk $ msg

        confirm_token :: ReqConfirmToken -> AppM RespConfirmToken
        confirm_token (ReqConfirmToken token fingerprint email) = do
            let token_b = encodeUtf8 token
                email_b = encodeUtf8 email
                fingerprint_b = encodeUtf8 fingerprint
                hashed = B.init . B.tail . encodeStrict . hashEmail $ email_b
            let confirmsubmit = SConfirm token_b fingerprint_b hashed
            env <- ask
            liftIO (connDo (redisconn env) . submit $ confirmsubmit) >>= \case
                Left err  -> pure $ RespConfirmToken (R.renderError err) Nothing Nothing
                Right msg -> pure $ RespConfirmToken (R.renderOk msg) (Just . decodeUtf8 $ hashed) (Just token)

        create :: ReqCreate -> AppM RespCreate
        create (ReqCreate hash token recipe startDate endDate mb_secret) = do
            let hash_b = encodeUtf8 hash
                token_b = encodeUtf8 token
                recipe_b = encodeUtf8 recipe
                startDate_b = encodeUtf8 startDate
                mb_endDate_b = (\x -> case isoOrCustom . T.unpack $ x of
                    Left _  -> Nothing
                    Right _ -> Just . encodeUtf8 $ x) =<< endDate
            env <- ask
            liftIO (connDo (redisconn env) getPollsNb) >>= \case
                Left err -> pure $ RespCreate (R.renderError err) Nothing
                Right nb ->
                    let pollid = encodeStrict (nb+1)
                        takeManager = takeMVar . pollmanager $ env
                    in  do
                        mb_secret' <- liftIO $ do
                            (n,g) <- takeManager
                            secret <- createToken g $ encodeUtf8 startDate
                            putMVar (pollmanager env) (n,g)
                            pure $ fmap (pure . B.init . B.tail . encodeStrict $ secret) mb_secret
                        liftIO $ do
                            case isoOrCustom . T.unpack $ startDate of
                                Left _ -> pure $ RespCreate (R.renderError (R.Err R.DatetimeFormat (mempty :: T.Text))) Nothing
                                Right date -> do
                                    modifyMVar_ (pollmanager env) $ \p -> pure (nb, snd p)
                                    res <- connDo (redisconn env) . submit $ SCreate hash_b token_b pollid recipe_b startDate_b mb_endDate_b mb_secret'
                                    case res of
                                        Left err -> pure $ RespCreate (R.renderError err) Nothing
                                        Right msg -> pure $ RespCreate (R.renderOk msg) (Just nb) 

        close :: ReqClose -> AppM RespClose
        close (ReqClose hash token pollid) = do
            let hash_b = encodeUtf8 hash
                token_b = encodeUtf8 token
                pollid_b = encodeUtf8 pollid
            env <- ask
            liftIO (connDo (redisconn env) . submit $ SClose hash_b token_b pollid_b) >>= \case
                Left err -> pure . RespClose . R.renderError $ err
                Right ok -> pure $ RespClose $ "Poll closed: " `T.append` (T.pack . show $ pollid)

        get :: Int -> Maybe String -> AppM RespGet
        get pollid secret_req = do
            env <- ask
            liftIO $ do
                now <- getNow
                hmap <- readMVar . pollcache $ env
                case HMS.lookup pollid_b hmap of
                    Just (poll, mb_scores, _, mb_secret_stored) ->
                        let valid = fromMaybe False $ (==) <$> mb_secret_req <*> mb_secret_stored
                        in  if not valid && isJust mb_secret_stored then pure $ RespGet stop Nothing Nothing else
                            if poll_visible poll then goFetch pollid_b env now
                            else do
                                updateCache env pollid_b now
                                pure $ finish poll mb_scores
                    Nothing -> goFetch pollid_b env now
            where
                pollid_b = encodeStrict pollid
                mb_secret_req = T.pack <$> secret_req
                isJust (Just x) = True
                isJust Nothing  = False
                goFetch pollid env now = liftIO (connDo (redisconn env) . getPoll $ SGet pollid) >>= \case
                    Left err -> pure $ RespGet (T.pack . show $ err) Nothing Nothing
                    Right (poll, mb_scores, mb_secret) -> do
                        updateCache env pollid now
                        pure $ finish poll mb_scores
                updateCache env pollid now = modifyMVar_ (pollcache env) (pure . HMS.update (\(a, b, _, d) -> Just (a, b, now, d)) pollid)
                err = R.Err R.BadSecret (mempty :: T.Text)
                stop = R.renderError err
                finish poll mb_scores = RespGet "Ok" (Just poll) mb_scores

        myhistory :: ReqMyHistory -> AppM RespMyHistory
        myhistory (ReqMyHistory hash token) =
            let hash_b = encodeUtf8 hash
            in  ask >>= \env -> liftIO (connDo (redisconn env) . getMyPollsData $ hash_b) >>= \case
                Left err -> pure $ RespMyHistory Nothing Nothing Nothing $ R.renderError err
                Right (hmap, taken, created) ->
                    let mb l = if null l then Nothing else Just l
                        finish = RespMyHistory (mb hmap) (mb taken) (mb created) "Here's you history."
                    in  pure finish

        take :: ReqTake -> AppM RespTake
        take (ReqTake hash token finger pollid answers) = do
            env <- ask
            res <- liftIO . connDo (redisconn env) . submit $
                SAnswer (encodeUtf8 hash) (encodeUtf8 token) (encodeUtf8 finger) (encodeStrict . show $ pollid) (map encodeStrict answers)
            case res of
                Left err  -> pure . RespTake . R.renderError $ err
                Right msg -> pure . RespTake . R.renderOk $ msg

        warmup :: AppM RespWarmup
        warmup = pure $ RespWarmup "Server warmed up for your coziness!"

newtype AppM a = AppM { unAppM :: ReaderT Config (ExceptT ServerError IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

data Config = Config {
    sendgridconf :: SendGridConfig,
    redisconn    :: Connection,
    pollmanager  :: PollCreator,
    pollcache    :: PollCache
}

runAppM :: AppM a -> Config -> IO (Either ServerError a)
runAppM app = runExceptT . runReaderT (unAppM app)

injectEnv :: Config -> AppM a -> Servant.Handler a
injectEnv config app = liftIO (runAppM app config) >>= \case
    Left err -> throwError err
    Right v  -> pure v

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
    where
        policy = simpleCorsResourcePolicy
          {
              corsMethods = [ "GET", "POST", "PUT", "OPTIONS" ],
              corsOrigins = Just (["http://localhost:8080", "https://hardcore-hopper-66afd6.netlify.app"], True),
              corsRequestHeaders = [ "authorization", "content-type" ]
          }

app :: Config -> Application
app env = serve api . hoistServer api (injectEnv env) $ server

startApp :: IO ()
startApp = do
    {- discovering port offered by the host -}
    env <- getEnvironment
    let port = maybe 8009 read $ lookup "PORT" env
    {- initializing connection to database, cache -}
    state <- initState
    cache <- initCache
    connector <- initRedisConnection
    {- setting up config -}
    let config = Config initSendgridConfig connector state cache
    {- running -}
    print "Worker started..."
    runSweeperWorker cache connector
    print $ "Server starting on port " ++ show port
    run port $ corsPolicy (app config)
