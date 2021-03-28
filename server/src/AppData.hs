{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module AppData where

import           Control.Concurrent         (MVar)
import           Control.Concurrent.MVar
import           Crypto.Hash
import           Crypto.KDF.BCrypt
import           Crypto.Number.Generate
import           "cryptonite" Crypto.Random
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString            as B
import qualified Data.Map                   as M
import qualified Data.Text                  as T

--


{- Requests -}


--
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
    part_clientId          :: T.Text,
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

data AskTokenRequest = AskTokenRequest {
    user_fingerprint :: T.Text,
    user_email       :: T.Text
}
$(deriveJSON defaultOptions ''AskTokenRequest)

data ConfirmTokenRequest = ConfirmTokenRequest {
    user_confirm_token :: T.Text,
    user_confirm_hash  :: T.Text
}
$(deriveJSON defaultOptions ''ConfirmTokenRequest)
--


{- Responses -}


--
newtype SubmitPartResponse = SubmitPartResponse { msg :: T.Text } deriving (Eq, Show)
$(deriveJSON defaultOptions ''SubmitPartResponse)

data GetPollResponse = GetPollResponse {
    get_poll_msg :: T.Text ,
    get_poll     :: Maybe Poll
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''GetPollResponse)

newtype AskTokenResponse = AskTokenResponse T.Text
$(deriveJSON defaultOptions ''AskTokenResponse)

newtype ConfirmTokenResponse = ConfirmTokenResponse T.Text
$(deriveJSON defaultOptions ''ConfirmTokenResponse)
--


{- App initialization & types -}


--
newtype SendGridConfig = SendGridBearer { bearer :: B.ByteString }

initSendgridConfig :: SendGridConfig
initSendgridConfig = SendGridBearer "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"

data RedisConfig = RedisConfig {
    auth :: Maybe B.ByteString,
    port :: Integer,
    host :: String
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

--


{- Helpers -}


--
createToken :: Monad m => SystemDRG -> B.ByteString -> m T.Text
createToken drg salt = do
    let (bytes, gen) = randomBytes drg 16 :: (B.ByteString, SystemDRG)
        digest = bcrypt 8 bytes (salt :: B.ByteString) :: B.ByteString
    return . T.pack . show . hashWith SHA256 $ digest
    where
        randomBytes = flip randomBytesGenerate

hashEmail email = T.pack . show $ hashWith SHA256 email

createPollId :: IO Integer
createPollId = generateBetween 1 100000000
