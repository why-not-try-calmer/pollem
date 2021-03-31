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
import           Data.Aeson.Extra            (encodeStrict)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T
import           Data.Text.Encoding
import           Database
import qualified ErrorsReplies               as ER
import           Mailer
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Scheduler                   (getNow, schedule)
import           Servant

type API =
    "ask_token" :> ReqBody '[JSON] ReqAskToken :> Post '[JSON] RespAskToken :<|>
    "confirm_token" :> ReqBody '[JSON] ReqConfirmToken :> Post '[JSON] RespConfirmToken :<|>
    "create" :> ReqBody '[JSON] ReqCreate :> Post '[JSON] RespPart :<|>
    "close":> ReqBody '[JSON] ReqClose :> Post '[JSON] RespPart :<|>
    "get" :> QueryParam "id" String :> Get '[JSON] GetPollResponse :<|>
    "take" :> ReqBody '[JSON] ReqPart :> Post '[JSON] RespPart

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = ask_token :<|> confirm_token :<|> create :<|> close :<|> get :<|> take
    where
        ask_token :: ReqAskToken -> AppM RespAskToken
        ask_token (ReqAskToken fingerprint email) = do
            env <- ask
            let mvar = state env
                hashed = hashEmail (encodeUtf8 email)
                hashed_b = encodeUtf8 hashed
                asksubmit token = SAsk hashed_b (encodeUtf8 fingerprint) (encodeUtf8 token)
            res <- liftIO $ do
                (n, gen) <- takeMVar mvar
                token <- createToken gen (encodeUtf8 email)
                putMVar mvar (n, gen)
                sendEmail $ makeSendGridEmail (sendgridconf env) token email
                connDo (redisconf env) . submit $ asksubmit token
            case res of
                Left err  -> return . RespAskToken . ER.renderError $ err
                Right msg -> return . RespAskToken . ER.renderOk $ msg

        confirm_token :: ReqConfirmToken -> AppM RespConfirmToken
        confirm_token (ReqConfirmToken token hash) = do
            let confirmsubmit = SConfirm (encodeUtf8 hash) (encodeUtf8 token)
            env <- ask
            res <- liftIO . connDo (redisconf env) . submit $ confirmsubmit
            case res of
                Left err  -> return . RespConfirmToken . ER.renderError $ err
                Right msg -> return . RespConfirmToken . ER.renderOk $ msg

        create :: ReqCreate -> AppM RespPart
        create (ReqCreate hash _ recipe) = do
            env <- ask
            liftIO $ do
                (v, g) <- takeMVar $ state env
                let pollid = encodeStrict (v+1)
                now <- getNow
                res <- connDo (redisconf env) . submit $ SPoll pollid (encodeStrict recipe) (encodeStrict . show $ now) "true"
                putMVar (state env) (v+1, g)
                case res of
                    Left err -> return . RespPart . ER.renderError $ err
                    Right ok -> return $ RespPart $
                        "Thanks for creating this poll! You can follow the results as they come using this id"
                            `T.append` (T.pack . show $ v+1)

        close :: ReqClose -> AppM RespPart
        close ReqClose{} = return $ RespPart "Thanks for creating this poll."

        take :: ReqPart -> AppM RespPart
        take (ReqPart hash finger pollid answers) = do
            let answers_encoded = B.concat . BL.toChunks . encode $ answers
            env <- ask
            res <- liftIO . connDo (redisconf env) . submit $
                SPoll (encodeUtf8 hash) (encodeUtf8 finger) (encodeUtf8 . T.pack . show $ pollid) answers_encoded
            case res of
                Left err  -> return . RespPart . ER.renderError $ err
                Right msg -> return . RespPart . ER.renderOk $ msg

        get :: Maybe String -> AppM GetPollResponse
        get (Just pollid) =
            let pollid_b = encodeStrict pollid
                req = SGet pollid_b
            in  do
                env <- ask
                res <- liftIO . connDo (redisconf env) . getPoll $ SGet pollid_b
                case res of
                    Left _ -> return $ GetPollResponse "Unable to find a poll with this id." Nothing Nothing
                    Right (poll, scores) -> return $ GetPollResponse "Ok" (Just poll) (Just scores)
        get Nothing = return $ GetPollResponse "Missing poll identifier." Nothing Nothing


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
