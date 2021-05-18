{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module AppTypes where

import           Control.Concurrent.MVar
import           Crypto.Random           (SystemDRG, getSystemDRG)
import           Data.Aeson.Extra        (encodeStrict)
import           Data.Aeson.TH
import qualified Data.ByteString         as B
import qualified Data.HashMap.Strict     as HMS
import qualified Data.Text               as T
import           Data.Text.Encoding      (encodeUtf8)
import           Data.Time               (UTCTime (UTCTime))
--

{- Requests -}

--
data Poll = Poll {
    poll_startDate   :: T.Text,
    poll_endDate     :: Maybe T.Text,
    poll_question    :: T.Text ,
    poll_description :: T.Text,
    poll_multiple    :: Bool,
    poll_visible     :: Bool,
    poll_answers     :: [T.Text]
}   deriving (Show, Eq)
$(deriveJSON defaultOptions ''Poll)

newtype ReqAskToken = ReqAskToken {
    ask_email       :: T.Text
}   deriving (Eq, Show)
$(deriveJSON defaultOptions ''ReqAskToken)

data ReqConfirmToken = ReqConfirmToken {
    confirm_token       :: T.Text,
    confirm_fingerprint :: T.Text,
    confirm_email       :: T.Text
}   deriving (Eq, Show)
$(deriveJSON defaultOptions ''ReqConfirmToken)

data ReqCreate = ReqCreate {
    create_hash      :: T.Text,
    create_email     :: T.Text,
    create_token     :: T.Text,
    create_recipe    :: T.Text,
    create_startDate :: T.Text,
    create_endDate   :: Maybe T.Text
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''ReqCreate)

data ReqClose = ReqClose {
    close_hash   :: T.Text,
    close_token  :: T.Text,
    close_pollid :: T.Text
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''ReqClose)

data ReqGet = ReqGet {
    get_int    :: Int,
    get_mb_str :: Maybe String
}  deriving (Eq,Show)

data ReqTake = ReqTake {
    take_hash        :: T.Text,
    take_token       :: T.Text,
    take_fingerprint :: T.Text,
    take_email       :: T.Text,
    take_pollid      :: T.Text,
    take_results     :: [Int]
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''ReqTake)

data ReqMyHistory = ReqMyHistory {
    myhistory_hash  :: T.Text,
    myhistory_token :: T.Text
}  deriving (Eq, Show)
$(deriveJSON defaultOptions ''ReqMyHistory)
--

{- Responses -}

--
newtype RespAskToken = RespAskToken { resp_ask_token :: T.Text }
$(deriveJSON defaultOptions ''RespAskToken)

data RespConfirmToken = RespConfirmToken {
    resp_confirm_msg   :: T.Text,
    resp_confirm_hash  :: Maybe T.Text,
    resp_confirm_token :: Maybe T.Text
}
$(deriveJSON defaultOptions ''RespConfirmToken)

data RespCreate = RespCreate {
    resp_create_msg        :: T.Text,
    resp_create_pollid     :: Maybe Int,
    resp_create_pollsecret :: Maybe T.Text
}
$(deriveJSON defaultOptions ''RespCreate)

newtype RespClose = RespClose { resp_close_msg :: T.Text }
$(deriveJSON defaultOptions ''RespClose)

newtype RespTake = RespTake { resp_take_msg :: T.Text }
$(deriveJSON defaultOptions ''RespTake)

data RespGet = RespGet {
    resp_get_poll_msg    :: T.Text ,
    resp_get_poll        :: Maybe Poll,
    resp_get_poll_scores :: Maybe [Int]
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''RespGet)

newtype RespWarmup = RespWarmup { resp_warmup_msg :: T.Text }
$(deriveJSON defaultOptions ''RespWarmup)

data RespMyHistory = RespMyHistory {
    resp_myhistory_polls   :: Maybe (HMS.HashMap T.Text [(T.Text, T.Text)]),
    resp_myhistory_taken   :: Maybe [T.Text],
    resp_myhistory_created :: Maybe [T.Text],
    resp_myhistory_msg     :: T.Text
}
$(deriveJSON defaultOptions ''RespMyHistory)
--

{- Stateful types -}

--
type PollCreator = MVar (Int, SystemDRG)

data PollInCache = PollInCache {
    _poll       :: Poll,
    _isActive   :: Bool,
    _results    :: Maybe [Int],
    _lastLookUp :: UTCTime,
    _hasSecret  :: Maybe B.ByteString 
}

type PollCache = MVar (HMS.HashMap B.ByteString PollInCache)

initState :: IO PollCreator
initState = do
    drg <- getSystemDRG
    newMVar (0, drg)

initCache :: IO PollCache
initCache = newMVar HMS.empty
