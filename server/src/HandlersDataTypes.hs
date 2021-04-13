{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module HandlersDataTypes where

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson                 as J
import           Data.Aeson.Extra           (encodeStrict)
import           Data.Aeson.TH
import qualified Data.ByteString            as B
import qualified Data.HashMap.Strict        as HMS
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Time                  (UTCTime (UTCTime))
import Crypto.Random (SystemDRG, getSystemDRG)
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
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''Poll)

newtype ReqAskToken = ReqAskToken {
    ask_email       :: T.Text
}
$(deriveJSON defaultOptions ''ReqAskToken)

data ReqConfirmToken = ReqConfirmToken {
    confirm_token       :: T.Text,
    confirm_fingerprint :: T.Text,
    confirm_email       :: T.Text
}
$(deriveJSON defaultOptions ''ReqConfirmToken)

data ReqCreate = ReqCreate {
    create_hash      :: T.Text,
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

data ReqTake = ReqTake {
    take_hash        :: T.Text,
    take_token       :: T.Text,
    take_fingerprint :: T.Text,
    take_pollid      :: T.Text,
    take_results     :: [Int]
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''ReqTake)

data ReqMyHistory = ReqMyHistory {
    myhistory_hash  :: T.Text,
    myhistory_token :: T.Text
}
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
    resp_create_msg    :: T.Text,
    resp_create_pollid :: Maybe Int,
    resp_create_pollsecret :: Maybe T.Text
}
$(deriveJSON defaultOptions ''RespCreate)

newtype RespClose = RespClose { resp_close_msg :: T.Text}
$(deriveJSON defaultOptions ''RespClose)

newtype RespTake = RespTake { resp_take_msg :: T.Text }
$(deriveJSON defaultOptions ''RespTake)

data RespGet = RespGet {
    resp_get_poll_msg     :: T.Text ,
    resp_get_poll         :: Maybe Poll,
    resp_get_poll_results :: Maybe [Int]
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''RespGet)

newtype RespWarmup = RespWarmup T.Text
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
mockPoll :: Poll
mockPoll = Poll {
    poll_question = "A question",
    poll_description = "A description",
    poll_startDate = "2021-03-14T14:15:14+01:00",
    poll_endDate = Just "2021-03-16T14:15:14+01:00",
    poll_multiple = True,
    poll_visible = True,
    poll_answers = ["First", "Second", "Third"]
}

type PollCreator = MVar (Int, SystemDRG)

type PollCache = MVar (HMS.HashMap B.ByteString (Poll, Maybe [Int], UTCTime, Maybe T.Text))

initState :: IO PollCreator
initState = do
    drg <- getSystemDRG
    newMVar (0, drg)

initCache :: IO PollCache
initCache = newMVar HMS.empty