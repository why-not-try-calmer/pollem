{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Server
    ( startApp
    , app
    ) where

import           AppData
import           Control.Concurrent          (newEmptyMVar, newMVar, putMVar,
                                              takeMVar)
import           Control.Concurrent.Async    (async, cancel)
import           Control.Exception           (try)
import           Control.Monad.Except        (ExceptT, runExceptT)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader
import           Data.Aeson                  (ToJSON (toEncoding), encode,
                                              fromEncoding, fromJSON, toJSON)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           Data.Text.Encoding
import           Database
import qualified ErrorsReplies               as ER
import           Mailer
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Scheduler                   (schedule)
import           Servant

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
                -- incrementing left value with 'last number'; replacing into state
                putMVar mvar (v+1, g)
                -- scheduling worker runtime thread to prepare callback upon time limit reached
                th <- async . schedule . show $ pay
                -- call `cancel` on th if you want to unschedule the event
                return ()
            return (SubmitPartResponse "Thanks for creating this poll. Before we can make it happen, please verify your email using the link sent there.")

        submitClose :: SubmitCloseRequest -> AppM SubmitPartResponse
        submitClose SubmitCloseRequest{} = return $ SubmitPartResponse "Thanks for creating this poll."

        getPoll :: Maybe String -> AppM GetPollResponse
        getPoll (Just i) = return $ GetPollResponse "Thanks for asking. Here is your poll data." initPoll
        getPoll Nothing = return $ GetPollResponse "Unable to find a poll with this id." Nothing

        submitPart :: SubmitPartRequest -> AppM SubmitPartResponse
        submitPart (SubmitPartRequest hash finger pollid poll) = do
            let poll_encoded = B.concat . BL.toChunks. encode $ poll
            env <- ask
            res <- liftIO . connDo (redisconf env) . submit $
                SPoll $ SubmitPoll (encodeUtf8 hash) (encodeUtf8 finger) (encodeUtf8 . T.pack . show $ pollid) poll_encoded
            case res of
                Left err  -> return . SubmitPartResponse . ER.encodeError $ err
                Right msg -> return . SubmitPartResponse . ER.encodeOk $ msg

        ask_token :: AskTokenRequest -> AppM AskTokenResponse
        ask_token (AskTokenRequest fingerprint email) = do
            let hashed = hashEmail (encodeUtf8 email)
            env <- ask
            let mvar = state env
            token <- liftIO $ do
                (n, gen) <- takeMVar mvar
                token <- createToken gen (encodeUtf8 email)
                putMVar mvar (n, gen)
                sendEmail $ makeSendGridEmail (sendgridconf env) token email
                return token
            let hashed_b = encodeUtf8 hashed
                asksubmit = SAskToken $ SubmitAskToken hashed_b (encodeUtf8 fingerprint) (encodeUtf8 token)
            res <- liftIO . connDo (redisconf env) . submit $ asksubmit
            case res of
                Left err  -> return . AskTokenResponse . ER.encodeError $ err
                Right msg -> return . AskTokenResponse . ER.encodeOk $ msg

        confirm_token :: ConfirmTokenRequest -> AppM ConfirmTokenResponse
        confirm_token (ConfirmTokenRequest token hash) = do
            let confirmsubmit = SConfirmToken $ SubmitConfirmToken (encodeUtf8 hash) (encodeUtf8 token)
            env <- ask
            res <- liftIO . connDo (redisconf env) . submit $ confirmsubmit
            case res of
                Left err -> return . ConfirmTokenResponse . ER.encodeError $ err
                Right msg -> return . ConfirmTokenResponse . ER.encodeOk $ msg


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
