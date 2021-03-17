{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server
    ( startApp
    , app
    ) where

import AppErrors
import           AppData
import           Control.Concurrent          (putMVar, takeMVar)
import           Control.Concurrent.Async    (async, cancel)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Map                    as M
import qualified Data.Text                   as T
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
    "verify_email" :> QueryParam "token" String :> Get '[JSON] VerificationResponse

api :: Proxy API
api = Proxy

server :: State -> Server API
server state = submitCreate state :<|> submitClose :<|> getPoll :<|> submitPart :<|> verify
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
            return (SubmitPartResponse "Thanks for creating this poll.")

        submitClose :: SubmitCloseRequest -> Handler SubmitPartResponse
        submitClose SubmitCloseRequest{} = return (SubmitPartResponse "Thanks for creating this poll.")

        getPoll :: Maybe String -> Handler GetPollResponse
        getPoll (Just i) = return $ GetPollResponse "Thanks for asking. Here is your poll data." initPoll
        getPoll Nothing = return $ GetPollResponse "Unable to find a poll with this id." Nothing

        verify :: Maybe String -> Handler VerificationResponse
        verify (Just s) = 
            if length s > 5 then 
                let err =  Error TokenNotExist s
                in  return $ VerificationResponse $ encodeError err
            else return (VerificationResponse "ok")
        verify Nothing = 
            let err = Error NoEmptyString ""
            in  return $ VerificationResponse $ encodeError err

app :: State -> Application
app s = simpleCors (serve api . server $ s)

startApp :: IO ()
startApp = do
    state <- initState
    run 8080 (app state)
