{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Server
    ( startApp
    , app
    ) where

import           Control.Concurrent          (readMVar, putMVar,
                                              takeMVar, threadDelay)
import           Control.Monad.Except        (ExceptT, runExceptT)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader
import           Data.Aeson                  (ToJSON (toEncoding), encode,
                                              fromEncoding, fromJSON, toJSON)
import           Data.Aeson.Extra            (encodeStrict)
import qualified Data.Text                   as T
import           Data.Text.Encoding
import           Database
import           Database.Redis              (Connection)
import qualified ErrorsReplies               as R
import           HandlersDataTypes
import qualified Data.HashMap.Strict as HMS
import           Mailer
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Scheduler                   (getNow, schedule)
import           Servant


type API =
    "ask_token" :> ReqBody '[JSON] ReqAskToken :> Post '[JSON] RespAskToken :<|>
    "confirm_token" :> ReqBody '[JSON] ReqConfirmToken :> Post '[JSON] RespConfirmToken :<|>
    "create" :> ReqBody '[JSON] ReqCreate :> Post '[JSON] RespCreate :<|>
    "close":> ReqBody '[JSON] ReqClose :> Post '[JSON] RespClose :<|>
    "get" :> QueryParam "id" String :> Get '[JSON] RespGet :<|>
    "take" :> ReqBody '[JSON] ReqTake :> Post '[JSON] RespTake

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = ask_token :<|> confirm_token :<|> create :<|> close :<|> get :<|> take
    where
        ask_token :: ReqAskToken -> AppM RespAskToken
        ask_token (ReqAskToken email) = do
            env <- ask
            let mvar = state env
                hashed = hashEmail email
                asksubmit token = SAsk (encodeStrict hashed) token
            res <- liftIO $ do
                (n, gen, h) <- takeMVar mvar
                token <- createToken gen email
                putMVar mvar (n, gen, h)
                sendEmail $ makeSendGridEmail (sendgridconf env) (encodeStrict token) email
                connDo (redisconn env) . submit $ asksubmit (encodeStrict token)
            case res of
                Left err  -> return . RespAskToken . R.renderError $ err
                Right msg -> return . RespAskToken . R.renderOk $ msg

        confirm_token :: ReqConfirmToken -> AppM RespConfirmToken
        confirm_token (ReqConfirmToken token fingerprint email) = do
            let confirmsubmit = SConfirm token fingerprint email
            env <- ask
            res <- liftIO . connDo (redisconn env) . submit $ confirmsubmit
            case res of
                Left err  -> return $ RespConfirmToken (R.renderError err) Nothing Nothing
                Right msg -> return $ RespConfirmToken (R.renderOk msg) (Just $ decodeUtf8 email) (Just $ decodeUtf8 token)

        create :: ReqCreate -> AppM RespCreate
        create (ReqCreate hash token recipe startDate endDate) = do
            env <- ask
            liftIO $ do
                (v, g, h) <- takeMVar $ state env
                let pollid = encodeStrict (v+1)
                now <- getNow
                res <- connDo (redisconn env) . submit $
                    SCreate hash token pollid recipe startDate endDate
                putMVar (state env) (v+1, g, h)
                case res of
                    Left err -> return . RespCreate . R.renderError $ err
                    Right ok -> return $ RespCreate $
                        "Thanks for creating this poll! You can follow the results as they come using this id"
                            `T.append` (T.pack . show $ v+1)

        close :: ReqClose -> AppM RespClose
        close (ReqClose hash token pollid) = do
            env <- ask
            res <- liftIO . connDo (redisconn env) . submit $
                SClose hash token pollid
            case res of
                Left err -> return . RespClose . R.renderError $ err
                Right ok -> return $ RespClose $ "Poll closed: " `T.append` (T.pack . show $ pollid)

        take :: ReqTake -> AppM RespTake
        take (ReqTake hash token finger pollid answers) = do
            env <- ask
            res <- liftIO . connDo (redisconn env) . submit $
                SAnswer hash token finger (encodeStrict . show $ pollid) answers
            case res of
                Left err  -> return . RespTake . R.renderError $ err
                Right msg -> return . RespTake . R.renderOk $ msg

        get :: Maybe String -> AppM RespGet
        get (Just pollid) =
            let pollid_b = encodeStrict pollid
                req = SGet pollid_b
            in  do
                env <- ask
                res <- liftIO $ do
                    (_,_, hmap) <- readMVar . state $ env
                    case HMS.lookup pollid hmap of
                        Just (poll, scores) -> return $ Right (poll, scores)
                        Nothing -> connDo (redisconn env) . getPoll $ SGet pollid_b
                case res of
                    Left _ -> return $ RespGet "Unable to find a poll with this id." Nothing Nothing
                    Right (poll, scores) -> return $ RespGet "Ok" (Just poll) (Just scores)
        get Nothing = return $ RespGet "Missing poll identifier." Nothing Nothing


newtype AppM a = AppM { unAppM :: ReaderT Config (ExceptT ServerError IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

data Config = Config {
    sendgridconf :: SendGridConfig,
    redisconn    :: Connection,
    state        :: State
}

runAppM :: AppM a -> Config -> IO (Either ServerError a)
runAppM app = runExceptT . runReaderT (unAppM app)

injectEnv :: Config -> AppM a -> Servant.Handler a
injectEnv config app = liftIO (runAppM app config) >>= \case
    Left err -> throwError err
    Right v  -> return v

app :: Config -> Application
app env = simpleCors $ serve api $ hoistServer api (injectEnv env) server

startApp :: IO ()
startApp = do
    state <- initState
    connector <- initRedisConnection
    let env = Config initSendgridConfig connector state
    run 8080 (app env)
