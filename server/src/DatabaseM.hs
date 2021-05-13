{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module DatabaseM where

import           AppTypes
import           Control.Exception              (SomeException, throwIO, try, Exception)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import qualified Data.Text                      as T
import           Database.MongoDB
import qualified Database.MongoDB.Transport.Tls as DbTLS
{-
    We take as input a command, inspect it, and see if we can accomodate using just the cache.
    If we can, we accomodate. If we cannot, for pull all the data we need from MongoDB and return the data
    to the caller, but before that, we dispatch a worker to update the cache after we've returned the requested
    data.

    -- MongoDB structure --
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
            _id (Int)
            author_hash
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

    -- Redis structure --
    polls (set of poll ids)
    poll:{poll_id} (meta data about poll, including poll recipe)
    users (set of user hash)
    user:{hash} (hashmap about users)
        token (ByteString)
        verified (ByteString)
        fingerprint (ByteString)
    participants_hash:{poll_id} (set of hashes of participants to <poll_id>)
    participants_email:{poll_id} (set of emails of participants to <poll_id>)
    participants_fingerprint:{poll_id}  (set of fingerprints of participants 1to <poll_id>)
    answers:{poll_id} (list of answers a Ints, where 0 is then interpreted as false and 1 true)
-}
--

{- Mapping utilities from Redis to MongoDB -}

--
data DbReqM =
    SMCreate {
        create_poll_hash      :: T.Text,
        create_poll_email     :: T.Text,
        create_poll_token     :: T.Text,
        create_poll_id        :: T.Text,
        create_poll_recipe    :: T.Text,
        create_poll_startDate :: T.Text,
        create_poll_endDate   :: Maybe T.Text,
        create_poll_secret    :: T.Text
    } |
    SMClose  {
        close_hash    :: T.Text,
        close_token   :: T.Text,
        close_poll_id :: T.Text
    } |
    SMAsk {
        ask_hash  :: T.Text,
        ask_token :: T.Text
    } |
    SMConfirm {
        confirm_token       :: T.Text,
        confirm_fingerprint :: T.Text,
        confirm_hash        :: T.Text
    } |
    SMTake {
        answers_hash        :: T.Text,
        answers_token       :: T.Text,
        answers_fingerprint :: T.Text,
        answers_email       :: T.Text,
        answers_poll_id     :: T.Text,
        answers_answers     :: [T.Text]
    } |
    SMGet { get_poll_id :: T.Text }

data BSONable = BSPoll { _poll :: Poll } | BSReq { _req :: DbReqM }

toBSON :: BSONable -> [Field]
toBSON (BSPoll (Poll s e q d m v a)) =
    let def = ["startDate" =: val s, "question" =: val q, "description" =: val d, "multiple" =: val m, "visible" =: val v, "answers" =: val a]
    in  case e of
        Nothing      -> def
        Just endDate -> def ++ ["endDate" =: val endDate]
toBSON (BSReq (SMCreate h em tok pid recipe start end sec)) =
    let def = ["_id" =: pid, "author_hash" =: val h, "email" =: val em, "token" =: val tok, "recipe" =: val recipe, "startDate" =: val start, "secret" =: sec]
    in  case end of
        Nothing      -> def
        Just endDate -> def ++ ["endDate" =: val endDate]
toBSON (BSReq (SMClose h t pid)) = ["_id" =: pid, "hash" =: h, "token" =: t]
toBSON (BSReq (SMAsk h t)) = ["_id" =: h, "token" =: t]
toBSON (BSReq (SMConfirm h fi t)) = ["_id" =: h, "fingerprint" =: fi, "token" =: t]
toBSON (BSReq (SMTake h t fi em pid ans)) = ["_id" =: pid, "hash" =: h, "token" =: t, "fingerprint" =: fi, "email" =: em, "answers" =: ans]
--

{- Auth, Connection , Runner -}

--
data MongoCreds = MongoCreds {
    shard :: String,
    user  :: T.Text,
    pwd   ::T.Text
} deriving (Eq, Show)

initMongCreds :: MongoCreds
initMongCreds = MongoCreds "cluster0-shard-00-02.cmocx.mongodb.net" "pollem-app" "FmUCY0OkZVHJ1MUY"

{-
    FIX ME: MongoAtlas tends to shuffle around the role of 'primary' versus 'secondary' shard
    Make sure to call selectOK to avoid failing to authenticate
-}
getAccess :: IO (Maybe Pipe)
getAccess =
    let creds = initMongCreds
    in  try (DbTLS.connect (shard creds) (PortNumber 27017)) >>= \case
        Left e -> let e'= e :: SomeException in do
            print ("Error while trying to connect: " ++ show e)
            pure Nothing
        Right pipe -> do
            authorized <- access pipe UnconfirmedWrites "admin" $ auth (user creds) (pwd creds)
            if authorized then pure $ Just pipe
            else pure Nothing

runMongo pipe = access pipe master "pollem"
--

{- Actions -}

--
checkIfExistsUser hash = findOne (select ["_id" =: hash] "users") >>= \case
    Just user -> pure True
    Nothing   -> pure False
getIfExistsUser hash = findOne (select ["_id" =: hash] "users")
getIfUserWithToken hash token = findOne (select ["_id" =: hash, "token" =: token] "users")
getIfUserWithFields hash fields = findOne (select fields "users")
updateUserWith fields = upsert (select fields "users")
createUser hash fingerprint = insert "users" ["_id" =: hash, "fingerprint" =: fingerprint, "verified" =: "true"]
--

{- Polls -}

--
getIfExistsPoll poll_id = findOne (select ["_id" =: poll_id] "polls")
getIfPollWithFields fields = findOne (select fields "polls")
createPoll :: MonadIO m => Document -> Action m Value
createPoll = insert "polls"
updatePollWith fields = upsert (select fields "polls")
--

{- Participations -}

--
insertParticipation poll_id hash email fingerprint answers = insert ("polls." `T.append` poll_id) ["poll_id" =: poll_id, "email" =: email, "fingerprint" =: fingerprint, "answers"=: answers]
getPollParticipations poll_id = find (select [] ("polls." `T.append` poll_id))
