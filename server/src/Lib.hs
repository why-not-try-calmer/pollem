{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Lib
    ( startApp
    , app
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

data SubmitRequest = SubmitRequest {
    clientId            :: Int,
    clientFingerPrint :: String,
    clientPollId      :: Int
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''SubmitRequest)

newtype SubmitResponse = SubmitResponse { msg :: String } deriving (Eq, Show)
$(deriveJSON defaultOptions ''SubmitResponse)

type API = "submit":> ReqBody '[JSON] SubmitRequest :> Post '[JSON] SubmitResponse

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = submit
    where
        submit :: SubmitRequest -> Handler SubmitResponse
        submit req = return (SubmitResponse "OK")