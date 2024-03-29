{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Database where

import           Computations           (collect)
import           Control.Exception      (SomeException (SomeException), try)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (decodeStrict)
import           Data.Aeson.Extra       (encodeStrict)
import qualified Data.ByteString        as B
import           Data.ByteString.Char8  (readInt, unsnoc)
import           Data.Foldable          (sequenceA_, traverse_)
import qualified Data.HashMap.Strict    as HMS
import           Data.Maybe
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Database.Redis
import           ErrorsReplies
import qualified ErrorsReplies          as R
import           HandlersDataTypes
import           Mailer                 (emailNotifyOnClose, initSendgridConfig,
                                         sendEmail)
import           Times                  (getNow)
--

{-- Requests to db: types --}

--
data DbReq =
    SCreate {
        create_poll_hash      :: B.ByteString,
        create_poll_email     :: B.ByteString,
        create_poll_token     :: B.ByteString,
        create_poll_id        :: B.ByteString,
        create_poll_recipe    :: B.ByteString,
        create_poll_startDate :: B.ByteString,
        create_poll_endDate   :: Maybe B.ByteString,
        create_poll_secret    :: B.ByteString
    } |
    SClose  {
        close_hash    :: B.ByteString,
        close_token   :: B.ByteString,
        close_poll_id :: B.ByteString
    } |
    SAsk {
        ask_hash  :: B.ByteString,
        ask_token :: B.ByteString
    } |
    SConfirm {
        confirm_token       :: B.ByteString,
        confirm_fingerprint :: B.ByteString,
        confirm_hash        :: B.ByteString
    } |
    STake {
        answers_hash        :: B.ByteString,
        answers_token       :: B.ByteString,
        answers_fingerprint :: B.ByteString,
        answers_emai        :: B.ByteString,
        answers_poll_id     :: B.ByteString,
        answers_answers     :: [B.ByteString]
    } |
    SGet { get_poll_id :: B.ByteString }
--

{-- Requests to db: functions --}

--
connInfo :: ConnectInfo
connInfo = defaultConnectInfo {
    connectHost ="redis-18910.c247.eu-west-1-1.ec2.cloud.redislabs.com",
    connectPort = PortNumber 18910,
    connectAuth = Just "PGO5OZQ9M5UFVU2JYfNQUMnnXcMCWtOh"
}

initRedisConnection :: ConnectInfo -> IO Connection
initRedisConnection = checkedConnect

connDo :: Connection -> Redis a -> IO a
connDo = runRedis

_connDo :: Connection  -> Redis a -> IO ()
_connDo conn = void . connDo conn

toCleanB :: String -> B.ByteString
toCleanB = B.init . B.tail . encodeStrict
dbErr = pure . Left . R.Err Database $ mempty
borked = pure . Left . R.Err BorkedData $ mempty
noUser = pure . Left . R.Err UserNotExist $ mempty
noPoll = pure. Left . R.Err PollNotExist $ mempty
missingFrom keyvals tested = not $ all (`elem` tested) keyvals

getPollsNb :: Redis (Either (Err T.Text) Int)
getPollsNb = smembers "polls" >>= \case
    Left _    -> dbErr
    Right res -> pure . Right . length $ res

submit :: DbReq -> Redis (Either (Err T.Text) (Ok T.Text))
{- hash + token generated a first time from the handler -}
submit (SAsk hash token) =
    let key = "user:" `B.append` hash
    in  do
        liftIO . print $ key
        exists key >>= \case
            Left err -> dbErr
            Right verdict ->
                if verdict then do
                    hmset key [("token", token)]
                    pure . Right . R.Ok $ "Bear in mind that this email address is registered already. I will send you a verification email nonetheless. You don't need to use it unless you want to re-authenticate your email address."
                else do
                    hmset key [("token", token),("verified", "false")] -- this structure is not typed! perhaps do it
                    pure . Right . R.Ok $ "Thanks, please check your email. Expect between 30s and 5 minutes latency from the email provider."
submit (SConfirm token fingerprint hash) = -- hash generated from the handler, token sent by the user, which on match with DB proves that the hash is from a confirmed email address.
    let key = "user:" `B.append` hash
    in  exists key >>= \case
        Left err -> pure . Left . R.Err Database $ T.pack . show $ err
        Right found ->
            if not found then noUser
            else hget key "token" >>= \case
                Left _ -> dbErr
                Right mb_saved_token -> case mb_saved_token of
                    Nothing -> noUser
                    Just saved_token ->
                        if token == saved_token then do
                            hmset key [("fingerprint", fingerprint),("verified","true")]
                            sadd "users" [hash]
                            pure . Right . R.Ok $ "Thanks, you've successfully confirmed your email address."
                        else noUser
submit (SCreate hash email token pollid recipe startDate mb_endDate secret) =
    let pollid_txt = decodeUtf8 pollid
        key = "user:" `B.append` hash
        payload =
            let base = [("author_hash",hash),("author_email", email),("secret",secret),("recipe",recipe),("startDate",startDate),("active","true")]
                endDate = (\v -> pure ("endDate" :: B.ByteString , v)) =<< mb_endDate
            in  base ++ catMaybes [endDate]
    in  exists ("poll:" `B.append` pollid) >>= \case
        Left err -> dbErr
        Right verdict ->
            if verdict then pure . Left . R.Err PollExists $ pollid_txt
            else hgetall key >>= \case
                Left _ -> noUser
                Right userdata ->
                    if missingFrom [("token",token), ("verified", "true")] userdata then pure . Left . R.Err UserNotVerified $ mempty
                    else multiExec ( do
                        hmset ("poll:" `B.append` pollid) payload
                        sadd "polls" [pollid]
                    ) >>= \case
                        TxSuccess _ -> pure .  Right . R.Ok $ "Added poll " `T.append` pollid_txt
                        _ -> dbErr
submit (SClose hash token pollid) =
    let userKey = "user:" `B.append` hash
        pollid_txt = decodeUtf8 pollid
    in  multiExec ( do
            polldata <- hgetall ("poll:" `B.append` pollid)
            userdata <- hgetall userKey
            pure $ (,) <$> polldata <*> userdata
        ) >>= \case
            TxSuccess (pdata, udata) ->
                if null pdata then noPoll else
                if null udata then noUser else
                if missingFrom [("active","true"), ("author_hash", hash)] pdata then pure . Left . R.Err Custom
                    $ "Either the poll was closed already, or you don't have closing rights." else
                if missingFrom [("verified", "true")] udata then pure . Left . R.Err UserNotVerified $ mempty
                else do
                hset ("poll:" `B.append` pollid) "active" "false"
                pure . Right . R.Ok $ "This poll was closed: " `T.append` pollid_txt
            _ -> pure . Left . R.Err PollNotExist $ pollid_txt

submit (STake hash token finger email pollid answers) =
    let pollid_txt = decodeUtf8 pollid
        userKey = "user:" `B.append` hash
    in  multiExec ( do
            polldata <- hgetall ("poll:" `B.append` pollid)
            userdata <- hgetall userKey
            ismemberH <- sismember ("participants_hashes:" `B.append` pollid) hash
            ismemberF <- sismember ("participants_fingerprints:" `B.append` pollid) hash
            pure $ (,,,) <$> polldata <*> userdata <*> ismemberH <*> ismemberF
        ) >>= \case TxSuccess (pdata, udata, isH, isF) ->
                        if missingFrom [("active","true")] pdata then pure . Left . R.Err PollInactive $ pollid_txt else
                        if missingFrom [("token", token),("verified", "true")] udata then pure . Left . R.Err UserNotVerified $ decodeUtf8 hash else
                        if isF || isH then pure . Left . R.Err PollTakenAlready $ mempty else
                                multiExec ( do
                                sadd ("participants_hashes:" `B.append` pollid) [hash]
                                sadd ("participants_fingerprints:" `B.append` pollid) [finger]
                                sadd ("participants_email:" `B.append` pollid) [email]
                                lpush ("answers:" `B.append` pollid `B.append` ":" `B.append` hash) answers
                                ) >>= \case TxSuccess _ -> pure . Right . R.Ok $ "Answers submitted successfully!"
                                            _  -> pure . Left . R.Err Database $ "Database error"
                    _   -> pure . Left . R.Err Database $ "User or poll data missingFrom."

getAnswers p pollid =
    let key = "answers:" `B.append` pollid `B.append` ":" `B.append` p
    in  lrange key 0 (-1)

getPollMetaScores :: B.ByteString -> Redis (Either (Err T.Text) ([[B.ByteString]], [(B.ByteString, B.ByteString)]))
getPollMetaScores pollid = smembers ("participants_hashes:" `B.append` pollid) >>= \case
    Left _ -> dbErr
    Right participants ->
        let collectAnswers = sequence <$> traverse (`getAnswers` pollid) participants
        in  multiExec ( do
                answers <- collectAnswers
                poll_meta_data <- hgetall ("poll:" `B.append` pollid)
                pure $ (,) <$> answers <*> poll_meta_data
            ) >>= \case
                TxError _ -> dbErr
                TxSuccess (answers, poll_meta_data) -> pure $ Right (answers, poll_meta_data)

getPoll :: DbReq -> Redis (Either (Err T.Text) (Poll, Bool, Maybe [Int], Maybe B.ByteString))
{- Returns all data on a single poll, as a tuple
<poll contents, whether is active, maybe the scores, maybe the secret> -}
getPoll (SGet pollid) =
    let pollid_txt = decodeUtf8 pollid
        key = ("poll:" `B.append` pollid)
    in  hgetall key >>= \case
        Left _ -> dbErr
        Right poll_raw ->
            if null poll_raw then noPoll else
            smembers ("participants_hashes:" `B.append` pollid) >>= \case
            Right participants ->
                if null participants then finish poll_raw Nothing
                else let collectAnswers = sequence <$> traverse (`getAnswers` pollid) participants
                in  multiExec ( do
                    answers <- collectAnswers
                    poll_meta_data <- hgetall ("poll:" `B.append` pollid)
                    pure $ (,) <$> answers <*> poll_meta_data
                    )
                >>= \case
                TxError _ -> dbErr
                TxSuccess (res, poll_raw) ->
                    let mb_decoded = sequence $ foldr decodeByteList [] res
                    in  case mb_decoded of
                        Just answers ->
                            case collect answers of
                            Left err -> pure . Left $ err
                            Right collected ->
                                let scores = Just $ reverse collected
                                in  finish poll_raw scores
                        Nothing -> borked
    where   decodeByteList :: [B.ByteString] -> [Maybe [Int]] -> [Maybe [Int]]
            decodeByteList val acc = traverse (fmap fst . readInt) val : acc
            finish poll_raw mb_scores =
                let poll_metadata = HMS.fromList poll_raw
                    mb_secret = HMS.lookup "secret" poll_metadata
                    active = Just "true" == HMS.lookup "active" poll_metadata
                in  case HMS.lookup "recipe" poll_metadata of
                        Nothing -> borked
                        Just recipe -> case decodeStrict recipe :: Maybe Poll of
                            Nothing -> borked
                            Just poll -> pure . Right $ (poll, active, mb_scores, mb_secret)

getPollIdEndDate :: Redis (Either (Err T.Text) [(B.ByteString, B.ByteString)])
{- Traverse the entire db and collects the endDate of all polls -}
getPollIdEndDate = smembers "polls" >>= \case
    Left _ -> dbErr
    Right ids -> traverse collectEndifExists ids >>= \res -> pure . Right . catMaybes $ res
    where
        collectEndifExists :: B.ByteString -> Redis (Maybe (B.ByteString, B.ByteString))
        collectEndifExists i =
            let pollid = "poll:" `B.append` i
            in  hget pollid "endDate" >>= \case
                Left err -> pure Nothing
                Right maybeEnd -> case maybeEnd of
                    Nothing -> pure Nothing
                    Just e  -> pure . Just $ (pollid, e)

disableNotifyPolls :: [B.ByteString] -> Redis (Either (Err T.Text) (Ok T.Text))
{- Set all polls in the list as 'active' => 'false' -}
disableNotifyPolls [] = pure . Right . R.Ok $ "No poll to disable"
disableNotifyPolls ls = multiExec (sequence <$> traverse disablePoll ls) >>= \case
    TxError _ -> dbErr
    TxSuccess toNotify -> pure . Right . R.Ok $ "Disabled these outdated polls: " `T.append` (T.concat . map decodeUtf8 $ ls)
    where
        disablePoll l =
            let key = ("poll:" `B.append` l)
            in  hset key "active" "false"

getTakenCreated :: B.ByteString -> Redis (Either (Err T.Text) ([B.ByteString], [B.ByteString]))
{- Get a user's complete polling history as a pair of lists
including the polls taken and created, in that order -}
getTakenCreated hash = smembers "polls" >>= \case
    Left _ -> dbErr
    Right ids ->
        multiExec ( do
        taken <- sequence <$> traverse collectParticipated ids
        created <- sequence <$> traverse collectCreated ids
        return $ (,) <$> taken <*> created ) >>= \case
        TxSuccess (taken, mb_created) ->
            let mytaken = filterOnAuthor ids taken
                mycreated = filterOnAuthor ids mb_created
            in  pure . Right $ (mytaken, mycreated)
        _ -> dbErr
    where
        collectParticipated i =
            let key = "participants_hashes:" `B.append` i
            in  smembers key
        collectCreated i = hget ("poll:" `B.append` i) "author_hash"
        filterOnAuthor ids ls = HMS.keys . HMS.filter (elem hash) . HMS.fromList . zip ids $ ls

getMyPollsData :: B.ByteString -> Redis (Either (Err T.Text) (HMS.HashMap T.Text [(T.Text, T.Text)], [T.Text], [T.Text]))
{- Returns hashmap of all poll ids into pair of (bytestring maps of) polls taken, polls created -}
getMyPollsData hash = getTakenCreated hash >>= \case
    Right (taken, created) ->
        let both = taken ++ created
        in  multiExec (sequence <$> traverse collectPoll both) >>= \case
            TxSuccess res ->
                let both_txt = map decodeUtf8 both
                    bi_map f (a, b) = (f a, f b)
                    res_txt = map (map $ bi_map decodeUtf8) res
                    hmap = HMS.fromList . zip both_txt $ res_txt
                    taken_txt = map decodeUtf8 taken
                    created_txt = map decodeUtf8 created
                in  pure . Right $ (hmap, taken_txt, created_txt)
            TxError err -> pure . Left . R.Err R.Custom $ T.pack err
    where
        collectPoll i = hgetall ("poll:" `B.append` i)

notifyOnDisable :: [B.ByteString] -> Redis (Either (Err T.Text) (Ok T.Text))
{- for each users in the input list, send them a 'poll closed' email, after making sure
they are given the scores and the poll's metadata -}
notifyOnDisable pollids = traverse getMetaScoresNotify pollids >>=
    (\case
        Left _  -> dbErr
        Right _ -> pure . Right . R.Ok $ ""
    ) . sequence
    where
        config = initSendgridConfig
        emailOne pollid meta_data recipients =
            let (scores, poll_meta_data) = meta_data
                hmap = HMS.fromList poll_meta_data
                author = case HMS.lookup "author_email" hmap of Just e -> e
                mb_poll = decodeStrict =<< HMS.lookup "recipe" hmap :: Maybe Poll
                poll = case mb_poll of Just p -> p
            in  sendEmail $ emailNotifyOnClose config (author:recipients) poll author scores
        getMetaScoresNotify pollid = do
            meta <- getPollMetaScores pollid
            recipients <- smembers ("participants_email" `B.append` pollid)
            case meta of
                Left _ -> dbErr
                Right meta' -> case recipients of
                    Left _ -> dbErr
                    Right recipients' -> liftIO $ emailOne pollid meta' recipients'
