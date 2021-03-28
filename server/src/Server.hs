{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Server
    ( startApp
    , app
    ) where

import           AppData
import           AppErrors
import           Control.Concurrent          (newEmptyMVar, newMVar, putMVar,
                                              takeMVar)
import           Control.Concurrent.Async    (async, cancel)
import           Control.Monad.Except        (ExceptT, runExceptT)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader
import qualified Data.ByteString             as B
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           Data.Text.Encoding
import           Database
import           Mailer
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Scheduler                   (schedule)
import           Servant
import Control.Exception (try)

newtype SendGridConfig = SendGridBearer { bearer :: B.ByteString }

initSendgridConfig :: SendGridConfig
initSendgridConfig = SendGridBearer "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"
data RedisConfig = RedisConfig {
    auth :: Maybe B.ByteString,
    port :: Int,
    host :: B.ByteString
}

initRedisConfig :: RedisConfig
initRedisConfig = RedisConfig {
    host ="ec2-108-128-25-66.eu-west-1.compute.amazonaws.com",
    port = 14459,
    auth = Just "p17df6aa47fbc3f8dfcbcbfba00334ecece8b39a921ed91d97f6a9eeefd8d1793"
}
data Config = Config {
    sendgridconf :: SendGridConfig,
    redisconf    :: RedisConfig,
    state        :: State
}

type API =
    "submit_create_request" :> ReqBody '[JSON] SubmitCreateRequest :> Post '[JSON] SubmitPartResponse :<|>
    "submit_close_request":> ReqBody '[JSON] SubmitCloseRequest :> Post '[JSON] SubmitPartResponse :<|>
    "getpoll" :> QueryParam "id" String :> Get '[JSON] GetPollResponse :<|>
    "submit_part_request" :> ReqBody '[JSON] SubmitPartRequest :> Post '[JSON] SubmitPartResponse :<|>
    "ask_token" :> ReqBody '[JSON] AskTokenRequest :> Post '[JSON] AskTokenResponse :<|>
    "confirm_token" :> ReqBody '[JSON] ConfirmTokenRequest :> Post '[JSON] ConfirmTokenResponse

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = submitCreate :<|> submitClose :<|> getPoll :<|> submitPart :<|> ask_token :<|> confirm_token
    where
        submitCreate :: SubmitCreateRequest -> AppM SubmitPartResponse
        submitCreate (SubmitCreateRequest cid fp pid pay) = do
            env <- ask
            let mvar = state env
            liftIO $ do
                (v, g) <- takeMVar mvar
                -- printing current stat left value
                print v
                -- incrementing left value with 'last number'; replacing into state
                putMVar mvar (v+1, g)
                -- scheduling worker runtime thread to prepare callback upon time limit reached
                th <- async $ schedule . T.unpack $ pay
                -- call `cancel` on th if you want to unschedule the event
                return ()
            return (SubmitPartResponse "Thanks for creating this poll. Before we can make it happen, please verify your email using the link sent there.")

        submitClose :: SubmitCloseRequest -> AppM SubmitPartResponse
        submitClose SubmitCloseRequest{} = return $ SubmitPartResponse "Thanks for creating this poll."

        getPoll :: Maybe String -> AppM GetPollResponse
        getPoll (Just i) = return $ GetPollResponse "Thanks for asking. Here is your poll data." initPoll
        getPoll Nothing = return $ GetPollResponse "Unable to find a poll with this id." Nothing

        submitPart payload@SubmitPartRequest{..} = do
            res <- liftIO . connDo  . submit $ AnswerPoll "ok" "ok" [("ok", "ok")]
            return . SubmitPartResponse $ res

        ask_token :: AskTokenRequest -> AppM AskTokenResponse
        ask_token (AskTokenRequest fingerprint email) = do
            let encoded = encodeUtf8 email
                hashed = hashEmail encoded
            env <- ask
            let mvar = state env
            token <- liftIO $ do
                (n, gen) <- takeMVar mvar
                token <- createToken gen encoded
                putMVar mvar (n, gen)
                sendEmail $ makeSendGridEmail token email
                return token
            let asksubmit = AskToken (encodeUtf8 hashed) (encodeUtf8 fingerprint) (encodeUtf8 token)
            verdict <- liftIO . connDo . submit $ asksubmit
            return $ AskTokenResponse hashed verdict

        confirm_token :: ConfirmTokenRequest -> AppM ConfirmTokenResponse
        confirm_token (ConfirmTokenRequest token hash) = do
            let confirmsubmit = ConfirmToken (encodeUtf8 hash) (encodeUtf8 token)
            verdict <- liftIO . connDo . submit $ confirmsubmit
            return $ ConfirmTokenResponse verdict

newtype AppM a = AppM { unAppM :: ReaderT Config (ExceptT ServerError IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

runAppM :: AppM a -> Config -> IO (Either ServerError a)
runAppM app = runExceptT . runReaderT (unAppM app)

injectEnv :: Config -> AppM a -> Handler a
injectEnv config app = liftIO (runAppM app config) >>= \case
    Left err -> throwError err
    Right v  -> return v

app :: Config -> Application
app env = simpleCors $ serve api $ hoistServer api (injectEnv env) server

startApp :: IO ()
startApp = do
    state <- initState
    let env = Config initSendgridConfig initRedisConfig state
    run 8080 (app env)
