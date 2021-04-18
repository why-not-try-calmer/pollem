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
import           Cryptog
import           Data.Aeson                  (ToJSON (toEncoding), encode,
                                              fromEncoding, fromJSON, toJSON)
import           Data.Aeson.Extra            (encodeStrict)
import qualified Data.ByteString             as B
import qualified Data.HashMap.Strict         as HMS
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Data.Text.Encoding
import           Database
import           Database.Redis              (Connection, PortID (PortNumber),
                                              disconnect, info, runRedis)
import qualified ErrorsReplies               as R
import           HandlersDataTypes
import           Mailer
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant
import           System.Environment          (getEnvironment)
import           Times                       (fresherThan, getNow, isoOrCustom)
import           Worker

type API =
    "ask_token" :> ReqBody '[JSON] ReqAskToken :> Post '[JSON] RespAskToken :<|>
    "confirm_token" :> ReqBody '[JSON] ReqConfirmToken :> Post '[JSON] RespConfirmToken :<|>
    "create" :> ReqBody '[JSON] ReqCreate :> Post '[JSON] RespCreate :<|>
    "close":> ReqBody '[JSON] ReqClose :> Post '[JSON] RespClose :<|>
    "polls" :> Capture "id" Int :> QueryParam "secret" String :> Get '[JSON] RespGet :<|>
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
                hashed = toCleanB . hashEmail $ email_b
                asksubmit token = SAsk hashed token
            res <- liftIO $ do
                (n, gen) <- takeMVar mvar
                now <- getNow
                token <- createCrypto gen (toCleanB . show $ now)
                let token_b = toCleanB token
                putMVar mvar (n, gen)
                sendEmail (emailToken (sendgridconf env) token_b email_b) >>= \case
                    Right _ -> connDo (redisconn env) . submit $ asksubmit token_b
            case res of
                Left err  -> pure . RespAskToken . R.renderError $ err
                Right msg -> pure . RespAskToken . R.renderOk $ msg

        confirm_token :: ReqConfirmToken -> AppM RespConfirmToken
        confirm_token (ReqConfirmToken token fingerprint email) = do
            let token_b = encodeUtf8 token
                email_b = encodeUtf8 email
                fingerprint_b = encodeUtf8 fingerprint
                hashed = toCleanB . hashEmail $ email_b
            let confirmsubmit = SConfirm token_b fingerprint_b hashed
            env <- ask
            liftIO (connDo (redisconn env) . submit $ confirmsubmit) >>= \case
                Left err  -> pure $ RespConfirmToken (R.renderError err) Nothing Nothing
                Right msg -> pure $ RespConfirmToken (R.renderOk msg) (Just . decodeUtf8 $ hashed) (Just token)

        create :: ReqCreate -> AppM RespCreate
        create (ReqCreate hash token recipe startDate endDate) = do
            let hash_b = encodeUtf8 hash
                token_b = encodeUtf8 token
                recipe_b = encodeUtf8 recipe
                startDate_b = encodeUtf8 startDate
                mb_endDate_b = (\x -> case isoOrCustom . T.unpack $ x of
                    Left _  -> Nothing
                    Right _ -> Just . encodeUtf8 $ x) =<< endDate
            env <- ask
            liftIO $ connDo (redisconn env) getPollsNb >>= \case
                Left err -> stopOn err
                Right total ->
                    let pollid = encodeStrict (total + 1)
                        takeManager = takeMVar . pollmanager $ env
                        produceSecret = do
                            (n,g) <- takeManager
                            secret <- createCrypto g $ encodeUtf8 startDate
                            putMVar (pollmanager env) (n,g)
                            pure . toCleanB $ secret
                    in  do
                        secret <- produceSecret
                        case isoOrCustom . T.unpack $ startDate of
                            Left _ -> pure $ RespCreate (R.renderError (R.Err R.DatetimeFormat (mempty :: T.Text))) Nothing Nothing
                            Right date -> liftIO (connDo (redisconn env) . submit $ SCreate hash_b token_b pollid recipe_b startDate_b mb_endDate_b secret) >>=
                                \case   Left err -> stopOn err
                                        Right msg -> pure $ RespCreate (R.renderOk msg) (Just $ total + 1) (Just . decodeUtf8 $ secret)
            where
                stopOn err = pure $ RespCreate (R.renderError err) Nothing Nothing

        close :: ReqClose -> AppM RespClose
        close (ReqClose hash token pollid) = do
            let hash_b = encodeUtf8 hash
                token_b = encodeUtf8 token
                pollid_b = encodeUtf8 pollid
            env <- ask
            let conn = redisconn env
            liftIO (connDo conn . submit $ SClose hash_b token_b pollid_b) >>= \case
                Left err -> pure . RespClose . R.renderError $ err
                Right _ -> liftIO (connDo conn . notifyOnDisable $ [pollid_b]) >>= \case
                    Left err -> pure . RespClose $ R.renderError err
                    Right _  -> pure $ RespClose "Poll closed"

        get :: Int -> Maybe String -> AppM RespGet
        get pollid secret_req = do
            env <- ask
            liftIO $ do
                now <- getNow
                hmap <- readMVar . pollcache $ env
                case HMS.lookup pollid_b hmap of
                    -- poll, scores, last accessed datetime, secret
                    Just (poll, active, mb_scores, _, mb_secret_stored) ->
                        if runningButMissing now poll mb_secret_stored then stopWith badsecret else
                        if poll_visible poll || not active then goFetchScores pollid_b env now else do
                            -- updating cache
                            updateCache env pollid_b now
                            finish poll mb_scores
                    Nothing -> liftIO (connDo (redisconn env) . getPoll $ SGet pollid_b) >>= \case
                        Left err -> stopWith err
                        Right (poll, active, mb_scores, mb_secret_stored_b) ->
                            let res = (poll, active, mb_scores, now, mb_secret_req)
                            in if runningButMissing now poll (decodeUtf8 <$> mb_secret_stored_b) then stopWith badsecret else do
                                -- adding to cache
                                addToCache env res
                                finish poll mb_scores
            where
                pollid_b = encodeStrict pollid
                mb_secret_req = T.pack <$> secret_req
                runningButMissing now poll mb_secret_stored =
                    let mismatch = fromMaybe False $ (/=) <$> mb_secret_req <*> mb_secret_stored
                        running a Nothing = False
                        running a mb_b = case poll_endDate poll of
                            Just b -> case isoOrCustom . T.unpack $ b of Right b_date -> a < b_date
                            _ -> False
                    in  running now (poll_endDate poll) && mismatch
                goFetchScores pollid env now = liftIO (connDo (redisconn env) . getPoll $ SGet pollid) >>= \case
                    Left err -> pure $ RespGet (T.pack . show $ err) Nothing Nothing
                    Right (poll, active, mb_scores, mb_secret) -> do
                        updateCache env pollid now
                        finish poll mb_scores
                updateCache env pollid now = modifyMVar_ (pollcache env) (pure . HMS.update (\(a, b, c, _, e) -> Just (a, b, c, now, e)) pollid)
                addToCache env (poll, active, mb_scores, now, mb_secret_req) = modifyMVar_ (pollcache env) (pure . HMS.insert pollid_b (poll, active, mb_scores, now, mb_secret_req))
                badsecret = R.Err R.BadSecret (mempty :: T.Text)
                stopWith err = pure $ RespGet (R.renderError err) Nothing Nothing
                finish poll mb_scores = pure $ RespGet "Ok" (Just poll) mb_scores

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
        take (ReqTake hash token finger email pollid answers) = do
            env <- ask
            res <- liftIO . connDo (redisconn env) . submit $
                STake (encodeUtf8 hash) (encodeUtf8 token) (encodeUtf8 finger) (encodeUtf8 email) (encodeUtf8 pollid) (map encodeStrict answers)
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
    connector <- initRedisConnection connInfo
    {- setting up config -}
    let config = Config initSendgridConfig connector state cache
    {- running -}
    print "Worker started..."
    runAutoClose connector cache
    print $ "Server starting on port " ++ show port
    run port $ corsPolicy (app config)
