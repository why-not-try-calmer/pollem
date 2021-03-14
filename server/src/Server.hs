{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server
    ( startApp
    , app
    ) where

import           Control.Concurrent.Async (async, cancel)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.Text                as T
import qualified Data.Map   as M
import           Network.Wai
import Network.Wai.Middleware.Cors
import           Network.Wai.Handler.Warp
import           Requests
import           Scheduler                (schedule)
import           Servant

type API =
    "submit_part_request" :> ReqBody '[JSON] SubmitPartRequest :> Post '[JSON] SubmitPartResponse :<|>
    "submit_create_request" :> ReqBody '[JSON] SubmitCreateRequest :> Post '[JSON] SubmitPartResponse :<|>
    "submit_close_request":> ReqBody '[JSON] SubmitCloseRequest :> Post '[JSON] SubmitPartResponse :<|>
    "getpoll" :> QueryParam "id" String :> Get '[JSON] GetPollResponse

api :: Proxy API
api = Proxy

server :: Server API
server = submitPart :<|> submitCreate :<|> submitClose :<|> getPoll
    where
        submitPart :: SubmitPartRequest -> Handler SubmitPartResponse
        submitPart SubmitPartRequest{} = return (SubmitPartResponse "Thanks for participating.")

        submitCreate :: SubmitCreateRequest -> Handler SubmitPartResponse
        submitCreate (SubmitCreateRequest cid fp pid pay) = do
            liftIO $ do
                th <- async $ schedule . T.unpack $ pay
                -- call `cancel` on th if you want to unschedule the event
                return ()
            return (SubmitPartResponse "Thanks for creating this poll.")

        submitClose :: SubmitCloseRequest -> Handler SubmitPartResponse
        submitClose SubmitCloseRequest{} = return (SubmitPartResponse "Thanks for creating this poll.")

        getPoll :: Maybe String -> Handler GetPollResponse
        getPoll (Just i) = return $ GetPollResponse "Thanks for asking. Here is your poll data." initPoll
        getPoll Nothing = return $ GetPollResponse "Unable to find a poll with this id." Nothing

app :: Application
app = simpleCors $ serve api server

startApp :: IO ()
startApp = run 8080 app