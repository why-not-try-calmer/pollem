{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
-- {-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Server
    ( startApp
    , app
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

data SubmitPartRequest = SubmitPartRequest {
    part_clientId            :: Int,
    part_clientFingerPrint :: String,
    part_clientPollId      :: Int,
    part_payload :: String
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''SubmitPartRequest)

data SubmitCreateRequest = SubmitCreateRequest {
    create_clientId            :: Int,
    create_clientFingerPrint :: String,
    create_clientPollId      :: Int,
    create_payload :: String
}
$(deriveJSON defaultOptions ''SubmitCreateRequest)

newtype SubmitPartResponse = SubmitPartResponse { msg :: String } deriving (Eq, Show)
$(deriveJSON defaultOptions ''SubmitPartResponse)

data PollInitData = PollInitData {
    question :: String,
    options :: [String],
    pollid :: Int
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''PollInitData)

data GetPollResponse = GetPollResponse {
    get_poll_msg :: String,
    get_poll_payload :: Maybe PollInitData
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''GetPollResponse)
    
type API = 
    "submit_part_request" :> ReqBody '[JSON] SubmitPartRequest :> Post '[JSON] SubmitPartResponse :<|>
    "submit_create_request" :> ReqBody '[JSON] SubmitCreateRequest :> Post '[JSON] SubmitPartResponse :<|>
    "poll" :> QueryParam "id" String :> Get '[JSON] GetPollResponse

api :: Proxy API
api = Proxy

server :: Server API
server = submitPart :<|> submitCreate :<|> getPoll
    where
        submitPart :: SubmitPartRequest -> Handler SubmitPartResponse
        submitPart SubmitPartRequest{} = return (SubmitPartResponse "Thanks for participating.")
        submitCreate :: SubmitCreateRequest -> Handler SubmitPartResponse
        submitCreate SubmitCreateRequest{} = return (SubmitPartResponse "Thanks for creating this poll.")
        getPoll :: Maybe String -> Handler GetPollResponse
        getPoll (Just s) = return $ GetPollResponse "Thanks for asking. Here is your poll data." (Just (PollInitData "A question" ["some answers"] 42))
        getPoll Nothing = return $ GetPollResponse "Unable to find a poll with this id." Nothing

app :: Application
app = serve api server

startApp :: IO ()
startApp = run 8080 app
