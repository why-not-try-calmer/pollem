{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server
    ( startApp
    , app
    ) where

import AppErrors
import Mailer
import Database
import           AppData
import           Control.Concurrent          (putMVar, takeMVar, newMVar, newEmptyMVar)
import           Control.Concurrent.Async    (async, cancel)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Scheduler                   (schedule)
import           Servant
import Data.Text.Encoding

type API =
    "submit_create_request" :> ReqBody '[JSON] SubmitCreateRequest :> Post '[JSON] SubmitPartResponse :<|>
    "submit_close_request":> ReqBody '[JSON] SubmitCloseRequest :> Post '[JSON] SubmitPartResponse :<|>
    "getpoll" :> QueryParam "id" String :> Get '[JSON] GetPollResponse :<|>
    "submit_part_request" :> ReqBody '[JSON] SubmitPartRequest :> Post '[JSON] SubmitPartResponse :<|>
    "ask_token" :> ReqBody '[JSON] AskTokenRequest :> Post '[JSON] AskTokenResponse :<|>
    "confirm_token" :> ReqBody '[JSON] ConfirmTokenRequest :> Post '[JSON] ConfirmTokenResponse

api :: Proxy API
api = Proxy

server :: State -> Server API
server state = submitCreate state :<|> submitClose :<|> getPoll :<|> submitPart :<|> ask_token state :<|> confirm_token 
    where
        submitPart :: SubmitPartRequest -> Handler SubmitPartResponse
        submitPart SubmitPartRequest{} = return (SubmitPartResponse "Thanks for participating.")

        submitCreate :: State -> SubmitCreateRequest -> Handler SubmitPartResponse
        submitCreate mvar (SubmitCreateRequest cid fp pid pay) = do
            liftIO $ do
                (v, g) <- takeMVar mvar
                -- printing current stat left value
                print v
                -- incrementing left value with 'last number'; replacing into state
                putMVar mvar (v+1, g)
                -- scheduling worker runtime thread to prepare callback upon time limit reached
                th <- async $ schedule . T.unpack $ pay
                -- call `cancel` on th if you want to unschedule the event
                -- cancel th
                return ()
            return (SubmitPartResponse "Thanks for creating this poll. Before we can make it happen, please verify your email using the link sent there.")

        submitClose :: SubmitCloseRequest -> Handler SubmitPartResponse
        submitClose SubmitCloseRequest{} = return (SubmitPartResponse "Thanks for creating this poll.")

        getPoll :: Maybe String -> Handler GetPollResponse
        getPoll (Just i) = return $ GetPollResponse "Thanks for asking. Here is your poll data." initPoll
        getPoll Nothing = return $ GetPollResponse "Unable to find a poll with this id." Nothing

        ask_token :: State -> AskTokenRequest -> Handler AskTokenResponse
        ask_token mvar (AskTokenRequest fingerprint email) = do
            let encoded = encodeUtf8 email
                hashed = hashEmail encoded
            token <- liftIO $ do
                (n, gen) <- takeMVar mvar
                token <- createToken gen encoded
                putMVar mvar (n, gen)
                sendEmail $ makeSendGridEmail token email
                return token
            liftIO . _connDo $ submitUser (encodeUtf8 hashed) (encodeUtf8 fingerprint) (encodeUtf8 token)
            return $ AskTokenResponse hashed "Thanks, please check your email"

        confirm_token :: ConfirmTokenRequest -> Handler ConfirmTokenResponse
        confirm_token (ConfirmTokenRequest token hash) = do
            either_verdict <- liftIO . connDo $ findHashCheckToken (encodeUtf8 hash) (encodeUtf8 token)
            let (ok, notOk) = (ConfirmTokenResponse "ok", ConfirmTokenResponse)
            case either_verdict of
                Left error -> return $ notOk error 
                Right verdict -> if verdict then return ok else return $ notOk "bad token" 

app :: State -> Application
app s = simpleCors (serve api . server $ s)

startApp :: IO ()
startApp = do
    state <- initState
    run 8080 (app state)
