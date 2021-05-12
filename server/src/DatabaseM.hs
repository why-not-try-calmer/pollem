{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module DatabaseM () where

import           AppTypes
import           Control.Exception              (SomeException, throwIO, try)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import qualified Data.Text                      as T
import           Database.MongoDB
import qualified Database.MongoDB.Transport.Tls as DbTLS
{-
    We're saving polls and users to MongoDB. Structure:
    users -- collection
        {
            _id (= user hash in the Haskell code)
            hash
            fingerprint
            email
            token
            verified (Bool)
        }
    polls -- collection with individual polls:
        {
            _id (= author_hash in the Haskell code)
            author_email
            author_token
            recipe (= bytestring containing defining the poll)
            startDate
            endDate (optional)
            secret (= set by default assuming that the creator wants to restrict the poll to only a number of viewers)
        }
    polls.{poll_id} -- collection registering singular actions against the poll, i.e. participation
        {
            (_id not set)
            poll_id
            hash
            token
            fingerprint
            email
            answers (Array of Int)
        }
-}
--

{- Mapping utilities from Redis to MongoDB -}

--
data DbReq =
    SCreate {
        create_poll_hash      :: T.Text,
        create_poll_email     :: T.Text,
        create_poll_token     :: T.Text,
        create_poll_id        :: T.Text,
        create_poll_recipe    :: T.Text,
        create_poll_startDate :: T.Text,
        create_poll_endDate   :: Maybe T.Text,
        create_poll_secret    :: T.Text
    } |
    SClose  {
        close_hash    :: T.Text,
        close_token   :: T.Text,
        close_poll_id :: T.Text
    } |
    SAsk {
        ask_hash  :: T.Text,
        ask_token :: T.Text
    } |
    SConfirm {
        confirm_token       :: T.Text,
        confirm_fingerprint :: T.Text,
        confirm_hash        :: T.Text
    } |
    STake {
        answers_hash        :: T.Text,
        answers_token       :: T.Text,
        answers_fingerprint :: T.Text,
        answers_email       :: T.Text,
        answers_poll_id     :: T.Text,
        answers_answers     :: [T.Text]
    } |
    SGet { get_poll_id :: T.Text }

data BSONable = BSPoll { _poll :: Poll } | BSReq { _req :: DbReq }

toBSON :: BSONable -> [Field]
toBSON (BSPoll (Poll s e q d m v a)) =
    let def = ["startDate" =: val s, "question" =: val q, "description" =: val d, "multiple" =: val m, "visible" =: val v, "answers" =: val a]
    in  case e of
        Nothing      -> def
        Just endDate -> def ++ ["endDate" =: val endDate]
toBSON (BSReq (SCreate h em tok i recipe start end sec)) =
    let def = ["hash" =: val h, "email" =: val em, "token" =: val tok, "recipe" =: val recipe, "startDate" =: val start, "secret" =: sec]
    in  case end of
        Nothing      -> def
        Just endDate -> def ++ ["endDate" =: val endDate]
toBSON (BSReq (SClose h t pid)) = ["hash" =: h, "token" =: t, "poll_id" =: pid]
toBSON (BSReq (SAsk h t)) = ["hash" =: h, "token" =: t]
toBSON (BSReq (SConfirm h fi t)) = ["hash" =: h, "fingerprint" =: fi, "token" =: t]
toBSON (BSReq (STake h t fi em pid ans)) = ["hash" =: h, "token" =: t, "fingerprint" =: fi, "email" =: em, "poll_id" =: pid, "answers" =: ans]
--

{- Auth, Connection -}

--
data MongoCreds = MongoCreds {
    shard :: String,
    user  :: T.Text,
    pwd   ::T.Text
} deriving (Eq, Show)

initMongCreds :: MongoCreds
initMongCreds = MongoCreds "cluster0-shard-00-02.cmocx.mongodb.net" "pollem-app" "FmUCY0OkZVHJ1MUY"

testAccess :: IO ()
{-
    FIX ME: MongoAtlas tends to shuffle around the role of 'primary' versus 'secondary' shard
    Make sure to call selectOK to avoid failing to authenticate
-}
testAccess =
    let creds = initMongCreds
    in  try (DbTLS.connect (shard creds) (PortNumber 27017)) >>= \case
        Left e -> let e'= e :: SomeException in print ("Error while trying to connect: " ++ show e)
        Right pipe -> do
            authorized <- access pipe UnconfirmedWrites "admin" $ auth (user creds) (pwd creds)
            if authorized then access pipe master "pollem" runIt else throwIO $ userError "Failed to authenticate."
            close pipe
    where
        runIt = do
            --createPoll ["_id" =: "007"]
            insertParticipation "007" "myhash" "lelaStar" "finger" [0,1,0]
            insertParticipation "007" "myhash" "vanessLane" "finger" [1,1,0]
            getPollParticipations "007" >>= rest >>= liftIO . print
--

{- Users -}

--
checkIfExistsUser hash = findOne (select ["_id" =: hash] "users") >>= \case
    Just user -> pure True
    Nothing   -> pure False
getIfExistsUser hash = findOne (select ["_id" =: hash] "users")
getIfUserWithToken hash token = findOne (select ["_id" =: hash, "token" =: token] "users")
getIfUserWithFields hash otherfields = findOne (select (("_id" =: hash) : otherfields) "users")
updateUserWith hash otherfields = upsert (select (("_id" =: hash) : otherfields) "users")
createUser hash fingerprint = insert "users" ["_id" =: hash, "fingerprint" =: fingerprint, "verified" =: "true"]
--

{- Polls -}

--
getIfExistsPoll poll_id = findOne (select ["_id" =: poll_id] "polls")
getIfPollWithFields poll_id otherfields = findOne (select (("_id" =: poll_id) : otherfields) "polls")
createPoll poll_id otherfields = insert "polls" (("_id" =: poll_id) : otherfields)
updatePollWith poll_id otherfields = upsert (select (("_id" =: poll_id) : otherfields) "polls")
--

{- Participations -}

--
insertParticipation poll_id hash email fingerprint answers = insert ("polls." `T.append` poll_id) ["poll_id" =: poll_id, "email" =: email, "fingerprint" =: fingerprint, "answers"=: answers]
getPollParticipations poll_id = find (select [] ("polls." `T.append` poll_id))
--

{- Operations -}

--
main :: IO ()
main = testAccess
--

{- Tests -}

--
makeAPoll :: Poll
makeAPoll = Poll "start_date" (Just "end_date") "question" "description" True True ["Answer A", "Answer B"]
