{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server
    ( startApp
    , app
    ) where

import           Control.Concurrent.Async    (async, cancel)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           AppData
import           Scheduler                   (schedule)
import           Servant
import Control.Monad.State (StateT (runStateT))

type API =
    "submit_create_request" :> ReqBody '[JSON] SubmitCreateRequest :> Post '[JSON] SubmitPartResponse :<|>
    "submit_close_request":> ReqBody '[JSON] SubmitCloseRequest :> Post '[JSON] SubmitPartResponse :<|>
    "getpoll" :> QueryParam "id" String :> Get '[JSON] GetPollResponse :<|>
    "submit_part_request" :> ReqBody '[JSON] SubmitPartRequest :> Post '[JSON] SubmitPartResponse

api :: Proxy API
api = Proxy

server :: LastGen -> Server API
server lg = submitCreate :<|> submitClose :<|> getPoll :<|> submitPart 
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

app :: LastGen -> Application
app s = simpleCors $ serve api (server s)

startApp :: IO ()
startApp = do
    state <- initState  
    run 8080 (app state)