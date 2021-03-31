{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module HandlersDataTypes where

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
    poll_startDate           :: T.Text,
    poll_endDate             :: Maybe T.Text,
    poll_question            :: T.Text ,
    poll_description         :: T.Text,
    poll_id                  :: Int,
    poll_multiple            :: Bool,
    poll_visible             :: Bool,
    poll_answers             :: [T.Text],
    poll_other_answers       :: Maybe [T.Text],
    poll_creator_fingerprint :: T.Text,
    poll_creator_token       :: T.Text
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''Poll)

data ReqTake = ReqPart {
    part_clientId          :: T.Text,
    part_clientFingerPrint :: T.Text,
    part_clientPollId      :: Int,
    part_answers           :: [Int]
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''ReqTake)

data ReqCreate = ReqCreate {
    create_hash   :: T.Text,
    create_recipe :: T.Text
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''ReqCreate)

data ReqClose = ReqClose {
    close_reason       :: T.Text,
    close_clientPollId :: Int
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''ReqClose)

newtype ReqAskToken = ReqAskToken {
    user_email       :: T.Text
}
$(deriveJSON defaultOptions ''ReqAskToken)

data ReqConfirmToken = ReqConfirmToken {
    user_confirm_token       :: T.Text,
    user_confirm_fingerprint :: T.Text,
    user_confirm_email       :: T.Text
}
$(deriveJSON defaultOptions ''ReqConfirmToken)
--

{- Responses -}

--
newtype RespAskToken = RespAskToken { ask_token :: T.Text }
$(deriveJSON defaultOptions ''RespAskToken)

data RespConfirmToken = RespConfirmToken { 
    confirm_msg :: T.Text,
    confirm_hash :: Maybe T.Text,
    confirm_token :: Maybe T.Text
}
$(deriveJSON defaultOptions ''RespConfirmToken)

newtype RespCreate = RespCreate { create_msg :: T.Text}
$(deriveJSON defaultOptions ''RespCreate)

newtype RespClose = RespClose { close_msg :: T.Text}
$(deriveJSON defaultOptions ''RespClose)

newtype RespTake = RespTake { take_msg :: T.Text } deriving (Eq, Show)
$(deriveJSON defaultOptions ''RespTake)

data RespGet = RespGet {
    get_poll_msg :: T.Text ,
    get_poll     :: Maybe Poll,
    get_results  :: Maybe [Int]
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''RespGet)
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

mockPoll :: Poll
mockPoll = Poll {
        poll_question = "A question",
        poll_description = "A description",
        poll_startDate = "2021-03-14T14:15:14+01:00",
        poll_endDate = Just "2021-03-16T14:15:14+01:00",
        poll_id = 42,
        poll_multiple = True,
        poll_visible = True,
        poll_answers = ["First", "Second", "Third"],
        poll_other_answers = Just ["OptFourth", "OptFifth"],
        poll_creator_fingerprint = "0x01",
        poll_creator_token = "lksdlksodi"
    }

type State = MVar (Integer, SystemDRG)

initState :: IO State
initState = do
    drg <- getSystemDRG
    newMVar (0, drg)
--

{- Token -}

--
createToken :: Monad m => SystemDRG -> B.ByteString -> m T.Text
createToken drg salt = do
    let (bytes, gen) = randomBytes drg 16 :: (B.ByteString, SystemDRG)
        digest = bcrypt 8 bytes (salt :: B.ByteString) :: B.ByteString
    return . T.pack . show . hashWith SHA256 $ digest
    where
        randomBytes = flip randomBytesGenerate

hashEmail email = show $ hashWith SHA256 email

createPollId :: IO Integer
createPollId = generateBetween 1 100000000
