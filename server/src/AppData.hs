{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module AppData where

import           Control.Concurrent         (MVar)
import           Control.Concurrent.MVar
import           Crypto.KDF.BCrypt
import           Crypto.Number.Generate
import           "cryptonite" Crypto.Random
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteArray             (ByteArray)
import qualified Data.ByteString            as B
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Text.Encoding

data Poll = Poll {
    poll_startDate               :: T.Text,
    poll_endDate                 :: Maybe T.Text,
    poll_question                :: T.Text ,
    poll_description             :: T.Text,
    poll_id                      :: Int,
    poll_multiple                :: Bool,
    poll_visible                 :: Bool,
    poll_answers                 :: M.Map T.Text T.Text,
    poll_other_answers           :: Maybe (M.Map T.Text T.Text),
    poll_requires_verified_email :: Bool,
    poll_creator_fingerprint     :: T.Text,
    poll_creator_token           :: T.Text 
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''Poll)

data SubmitPartRequest = SubmitPartRequest {
    part_clientId          :: Int,
    part_clientFingerPrint :: T.Text,
    part_clientPollId      :: Int,
    part_poll              :: Poll
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''SubmitPartRequest)

data SubmitCreateRequest = SubmitCreateRequest {
    create_clientId          :: Int,
    create_clientFingerPrint :: T.Text ,
    create_clientPollId      :: Int,
    create_payload           :: T.Text
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''SubmitCreateRequest)

data SubmitCloseRequest = SubmitCloseRequest {
    close_reason       :: String,
    close_clientPollId :: Int
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''SubmitCloseRequest)

newtype SubmitPartResponse = SubmitPartResponse { msg :: T.Text } deriving (Eq, Show)
$(deriveJSON defaultOptions ''SubmitPartResponse)

data GetPollResponse = GetPollResponse {
    get_poll_msg :: T.Text ,
    get_poll     :: Maybe Poll
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''GetPollResponse)

data AskTokenRequest = AskTokenRequest {
    user_fingerprint :: T.Text,
    user_email :: T.Text
}
$(deriveJSON defaultOptions ''AskTokenRequest)

newtype AskTokenResponse = AskTokenResponse { user_hash :: T.Text }
$(deriveJSON defaultOptions ''AskTokenResponse)

data ConfirmTokenRequest = ConfirmTokenRequest {
    user_confirm_token  :: T.Text,
    user_confirm_hash :: T.Text
}
$(deriveJSON defaultOptions ''ConfirmTokenRequest)

newtype ConfirmTokenResponse = ConfirmTokenResponse T.Text
$(deriveJSON defaultOptions ''ConfirmTokenResponse)

initPoll :: Maybe Poll
initPoll = Just Poll {
        poll_question = "A question",
        poll_description = "A description",
        poll_startDate = "2021-03-14T14:15:14+01:00",
        poll_endDate = Just "2021-03-16T14:15:14+01:00",
        poll_id = 42,
        poll_multiple = True,
        poll_visible = True,
        poll_answers = M.fromList [("1", "First")],
        poll_other_answers = Just . M.fromList $ [("opt1", "First optional")],
        poll_requires_verified_email = False,
        poll_creator_fingerprint = "0x01",
        poll_creator_token = "lksdlksodi" 
    }

type State = MVar (Integer, SystemDRG)

initState :: IO State
initState = do
    drg <- getSystemDRG
    newMVar (0, drg)

createToken drg salt = do
    let (bytes, gen) = randomBytes drg 16 :: (B.ByteString, SystemDRG)
        token = bcrypt 8 bytes (salt :: B.ByteString) :: B.ByteString
    return token
    where
        randomBytes = flip randomBytesGenerate

createPollId :: IO Integer
createPollId = generateBetween 1 100000000
