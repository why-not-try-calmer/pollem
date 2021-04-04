{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module HandlersDataTypes where

import           Control.Concurrent.MVar
import           Control.Monad
import           Crypto.Hash
import           Crypto.KDF.BCrypt
import           Crypto.Number.Generate
import           "cryptonite" Crypto.Random
import           Data.Aeson
import qualified Data.Aeson                 as J
import           Data.Aeson.TH
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as B64
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
--

{- Instantiating JSON as bytestringss to avoid over parsing -}

--
textToByteString :: MonadPlus m =>  T.Text -> m B.ByteString
textToByteString s =
    case B64.decode (encodeUtf8 s) of
        Left _   -> mzero
        Right bs -> pure bs

instance FromJSON B.ByteString where
  parseJSON (String x) = textToByteString x
  parseJSON _          = mzero
--

{- Requests -}

--
data Poll = Poll {
    poll_startDate           :: T.Text,
    poll_endDate             :: Maybe T.Text,
    poll_question            :: T.Text ,
    poll_description         :: T.Text,
    poll_multiple            :: Bool,
    poll_visible             :: Bool,
    poll_answers             :: [T.Text],
    poll_creator_fingerprint :: T.Text,
    poll_creator_token       :: T.Text
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''Poll)

newtype ReqAskToken = ReqAskToken {
    ask_email       :: B.ByteString
}

instance FromJSON ReqAskToken where
    parseJSON = withObject "ReqAskToken" $ \obj -> do
        ask_email <- obj .: "ask_email"
        return (ReqAskToken { ask_email = ask_email })

data ReqConfirmToken = ReqConfirmToken {
    req_confirm_token       :: B.ByteString,
    req_confirm_fingerprint :: B.ByteString,
    req_confirm_email       :: B.ByteString
}

instance FromJSON ReqConfirmToken where
    parseJSON = withObject "ReqConfirmToken" $ \obj -> do
        req_confirm_token <- obj .: "confirm_token"
        req_confirm_fingerprint <- obj .: "confirm_fingerprint"
        req_confirm_email <- obj .: "confirm_email"
        return (ReqConfirmToken { req_confirm_token = req_confirm_token, req_confirm_fingerprint = req_confirm_fingerprint, req_confirm_email = req_confirm_email})

data ReqCreate = ReqCreate {
    req_create_hash      :: B.ByteString,
    req_create_token     :: B.ByteString,
    req_create_recipe    :: B.ByteString,
    req_create_startDate :: B.ByteString,
    req_create_endDate   :: Maybe B.ByteString
} deriving (Eq, Show)

instance FromJSON ReqCreate where
    parseJSON = withObject "ReqCreate" $ \obj -> do
        req_create_hash <- obj .: "req_create_hash"
        req_create_token <- obj .: "req_create_token"
        req_create_recipe <- obj .: "req_create_recipe"
        req_create_startDate <- obj .: "req_create_startDate"
        req_create_endDate <- obj .: "req_create_endDate"

        return (ReqCreate {
            req_create_hash = req_create_hash, req_create_token = req_create_token, req_create_recipe = req_create_recipe,
            req_create_startDate = req_create_startDate, req_create_endDate = req_create_endDate
        })

data ReqClose = ReqClose {
    close_hash   :: B.ByteString,
    close_token  :: B.ByteString,
    close_pollid :: B.ByteString
} deriving (Eq, Show)

instance FromJSON ReqClose where
    parseJSON = withObject "ReqClose" $ \obj -> do
        close_hash <- obj .: "close_hash"
        close_token <- obj .: "close_token"
        close_pollid <- obj .: "close_pollid"
        return (ReqClose { close_hash = close_hash, close_token = close_token, close_pollid = close_pollid})

data ReqTake = ReqTake {
    take_hash        :: B.ByteString,
    take_token       :: B.ByteString,
    take_fingerprint :: B.ByteString,
    take_pollid      :: B.ByteString,
    take_answers     :: [B.ByteString]
} deriving (Eq, Show)

instance FromJSON ReqTake where
    parseJSON = withObject "ReqTake" $ \obj -> do
        take_hash <- obj .: "take_hash"
        take_token <- obj .: "take_token"
        take_fingerprint <- obj .: "take_fingerprint"
        take_pollid <- obj .: "take_pollid"
        take_answers <- obj .: "take_answers"
        return (ReqTake { take_hash = take_hash, take_token = take_token, take_fingerprint = take_fingerprint, take_pollid = take_pollid, take_answers = take_answers })
--

{- Responses -}

--
newtype RespAskToken = RespAskToken { ask_token :: T.Text }
$(deriveJSON defaultOptions ''RespAskToken)

data RespConfirmToken = RespConfirmToken {
    resp_confirm_msg   :: T.Text,
    resp_confirm_hash  :: Maybe T.Text,
    resp_confirm_token :: Maybe T.Text
}
$(deriveJSON defaultOptions ''RespConfirmToken)

newtype RespCreate = RespCreate { create_msg :: T.Text}
$(deriveJSON defaultOptions ''RespCreate)

newtype RespClose = RespClose { close_msg :: T.Text}
$(deriveJSON defaultOptions ''RespClose)

newtype RespTake = RespTake { take_msg :: T.Text }
$(deriveJSON defaultOptions ''RespTake)

data RespGet = RespGet {
    get_poll_msg :: T.Text ,
    get_poll     :: Maybe Poll,
    get_results  :: Maybe [Int]
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''RespGet)
--

{- Stateful types -}

--
mockPoll :: Poll
mockPoll = Poll {
        poll_question = "A question",
        poll_description = "A description",
        poll_startDate = "2021-03-14T14:15:14+01:00",
        poll_endDate = Just "2021-03-16T14:15:14+01:00",
        poll_multiple = True,
        poll_visible = True,
        poll_answers = ["First", "Second", "Third"],
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
createToken :: Monad m => SystemDRG -> B.ByteString -> m String
createToken drg salt = do
    let (bytes, gen) = randomBytes drg 16 :: (B.ByteString, SystemDRG)
        digest = bcrypt 8 bytes (salt :: B.ByteString) :: B.ByteString
    return . show . hashWith SHA256 $ digest
    where
        randomBytes = flip randomBytesGenerate

hashEmail email = show $ hashWith SHA256 email

createPollId :: IO Integer
createPollId = generateBetween 1 100000000
