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
import           Scheduler                   (getNow, schedule)
import           Servant
import           System.Environment          (getEnvironment)
import           Workers

type API =
    "ask_token" :> ReqBody '[JSON] ReqAskToken :> Post '[JSON] RespAskToken :<|>
    "confirm_token" :> ReqBody '[JSON] ReqConfirmToken :> Post '[JSON] RespConfirmToken :<|>
    "create" :> ReqBody '[JSON] ReqCreate :> Post '[JSON] RespCreate :<|>
    "close":> ReqBody '[JSON] ReqClose :> Post '[JSON] RespClose :<|>
    "get" :> Capture "poll_id" Integer :> Get '[JSON] RespGet :<|>
    "take" :> ReqBody '[JSON] ReqTake :> Post '[JSON] RespTake

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = ask_token :<|> confirm_token :<|> create :<|> close :<|> get :<|> take
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
        create (ReqCreate hash token recipe startDate endDate) = do
            let hash_b = encodeUtf8 token
                token_b = encodeUtf8 token
                recipe_b = encodeUtf8 token
                startDate_b = encodeUtf8  startDate
                endDate_b = encodeUtf8 <$> endDate
            env <- ask
            liftIO $ do
                (v, g) <- takeMVar $ pollmanager env
                let pollid = encodeStrict (v+1)
                now <- getNow
                res <- connDo (redisconn env) . submit $
                    SCreate hash_b token_b pollid recipe_b startDate_b endDate_b
                putMVar (pollmanager env) (v+1, g)
                case res of
                    Left err -> pure . RespCreate . R.renderError $ err
                    Right ok -> pure $ RespCreate $
                        "Thanks for creating this poll! You can follow the results as they come using this id"
                            `T.append` (T.pack . show $ v+1)

        close :: ReqClose -> AppM RespClose
        close (ReqClose hash token pollid) = do
            let hash_b = encodeUtf8 hash
                token_b = encodeUtf8 token
                pollid_b = encodeUtf8 pollid
            env <- ask
            liftIO (connDo (redisconn env) . submit $ SClose hash_b token_b pollid_b) >>= \case
                Left err -> pure . RespClose . R.renderError $ err
                Right ok -> pure $ RespClose $ "Poll closed: " `T.append` (T.pack . show $ pollid)

        get :: Integer -> AppM RespGet
        get pollid =
            let pollid_b = encodeStrict pollid
                req = SGet pollid_b
            in  do
                env <- ask
                liftIO $ do
                    now <- getNow
                    hmap <- readMVar . pollcache $ env
                    res <- case HMS.lookup pollid_b hmap of
                        Just (poll, scores, _) -> pure $ Right (poll, scores)
                        Nothing -> connDo (redisconn env) . getPoll $ SGet pollid_b
                    case res of
                        Left _ -> pure $ RespGet "Unable to find a poll with this id." Nothing Nothing
                        Right (poll, scores) -> do
                            modifyMVar_ (pollcache env) (\_ -> pure $ HMS.insert pollid_b (poll, scores, now) hmap)
                            if poll_visible poll then pure $ RespGet "Ok" (Just poll) (Just scores)
                            else pure $ RespGet "Ok" (Just poll) Nothing

        take :: ReqTake -> AppM RespTake
        take (ReqTake hash token finger pollid answers) = do
            env <- ask
            res <- liftIO . connDo (redisconn env) . submit $
                SAnswer (encodeUtf8 hash) (encodeUtf8 token) (encodeUtf8 finger) (encodeStrict . show $ pollid) (map encodeStrict answers)
            case res of
                Left err  -> pure . RespTake . R.renderError $ err
                Right msg -> pure . RespTake . R.renderOk $ msg

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
              corsOrigins = Just (["http://localhost:8080"], True),
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
    if port == 8009
        then run port $ corsPolicy (app config)
        else run port (app config)
