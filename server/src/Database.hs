{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Database where

import           Computations           (collect)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (decodeStrict)
import           Data.Aeson.Extra       (encodeStrict)
import qualified Data.ByteString        as B
import           Data.ByteString.Char8  (readInt, unsnoc)
import qualified Data.HashMap.Strict    as HMS
import qualified Data.Map               as M
import           Data.Maybe
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Database.Redis
import           ErrorsReplies
import qualified ErrorsReplies          as R
import           HandlersDataTypes
import           Times                  (getNow)
import Control.Exception (try, SomeException (SomeException))
--

{-- Requests to db: types --}

--
data DbReq =
    SCreate {
        create_poll_hash      :: B.ByteString,
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
missingFrom keyvals tested = not $ all (`elem` tested) keyvals

getPollsNb :: Redis (Either (Err T.Text) Int)
getPollsNb = smembers "polls" >>= \case
    Left _    -> dbErr
    Right res -> pure . Right . length $ res

submit :: DbReq -> Redis (Either (Err T.Text) (Ok T.Text))
submit (SAsk hash token) = -- hash + token generated a first time from the handler
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
submit (SCreate hash token pollid recipe startDate mb_endDate secret) =
    let pollid_txt = decodeUtf8 pollid
        key = "user:" `B.append` hash
        payload =
            let base = [("author_hash",hash),("secret",secret),("recipe",recipe),("startDate",startDate),("active","true")]
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
                if missingFrom [("active","true"), ("<", token)] pdata then pure . Left . R.Err Custom
                    $ "Either the poll was closed already, or you don't have closing rights." else
                if missingFrom [("verified", "true")] udata then pure . Left . R.Err UserNotVerified $ mempty
                else do
                hset pollid "active" "false"
                pure . Right . R.Ok $ "This poll was closed: " `T.append` pollid_txt
            _ -> pure . Left . R.Err PollNotExist $ pollid_txt

submit (STake hash token finger pollid answers) =
    let pollid_txt = decodeUtf8 pollid
        userKey = "user:" `B.append` hash
    in  multiExec ( do
            polldata <- hgetall ("poll:" `B.append` pollid)
            userdata <- hgetall userKey
            ismemberH <- sismember hash ("participants_hashes:" `B.append` pollid)
            ismemberF <- sismember hash ("participants_fingerprints:" `B.append` finger)
            pure $ (,,,) <$> polldata <*> userdata <*> ismemberH <*> ismemberF
        ) >>= \case TxSuccess (pdata, udata, isH, isF) ->
                        if missingFrom [("active","true")] pdata  then pure . Left . R.Err PollInactive $ pollid_txt else
                        if missingFrom [("token", token),("verified", "true")] udata then pure . Left . R.Err UserNotVerified $ decodeUtf8 hash else
                        if isF || isH then pure . Left . R.Err PollTakenAlready $ pollid_txt
                        else multiExec ( do
                            sadd ("participants_hashes:" `B.append` pollid) [hash]
                            sadd ("participants_fingerprints:" `B.append` pollid) [finger]
                            lpush ("answers:" `B.append` pollid `B.append` ":" `B.append` hash) answers
                        ) >>= \case TxSuccess _ -> pure . Right . R.Ok $ "Answers submitted successfully!"
                                    _  -> pure . Left . R.Err Database $ "Database error"
                    _   -> pure . Left . R.Err Database $ "User or poll data missingFrom."

getPoll :: DbReq -> Redis (Either (Err T.Text) (Poll, Maybe [Int], Maybe B.ByteString  ))
getPoll (SGet pollid) =
    let pollid_txt = decodeUtf8 pollid
        key = ("poll:" `B.append` pollid)
    in  exists key >>= \case
        Left err -> dbErr
        Right verdict ->
            if not verdict then pure . Left . R.Err PollNotExist $ pollid_txt
            else smembers ("participants_hashes:" `B.append` pollid) >>= \case
                Right participants ->
                    if null participants then hgetall ("poll:" `B.append` pollid) >>= \case
                        Right poll_raw ->
                            let poll_metadata = M.fromList poll_raw
                                mb_secret = M.lookup "secret" poll_metadata
                            in  case M.lookup "recipe" poll_metadata of
                                    Nothing -> borked
                                    Just recipe -> case decodeStrict recipe :: Maybe Poll of
                                        Nothing -> borked
                                        Just poll -> pure . Right $ (poll, Nothing, mb_secret)
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
                                    let poll_metadata = M.fromList poll_raw
                                    in  case M.lookup "recipe" poll_metadata of
                                        Nothing -> borked
                                        Just recipe -> case decodeStrict recipe :: Maybe Poll of
                                            Nothing -> borked
                                            Just poll -> pure . Right $ (poll, Just $ reverse collected, M.lookup "secret" poll_metadata)
                            Nothing -> borked
    where   decodeByteList :: [B.ByteString] -> [Maybe [Int]] -> [Maybe [Int]]
            decodeByteList val acc = traverse (fmap fst . readInt) val : acc
            getAnswers p pollid =
                let key = "answers:" `B.append` pollid `B.append` ":" `B.append` p
                in  lrange key 0 (-1)

getResults :: Redis (Either (Err T.Text) [(B.ByteString, B.ByteString)])
getResults = smembers "polls" >>= \case
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

disablePolls :: [B.ByteString] -> Redis (Either (Err T.Text) (Ok T.Text))
disablePolls [] = pure . Right . R.Ok $ "No poll to disable"
disablePolls ls = multiExec (sequence_ <$> traverse disablePoll ls) >>= \case
    TxError _ -> dbErr
    TxSuccess _ -> pure . Right . R.Ok $ "Disabled these outdated polls: " `T.append` (T.concat . map decodeUtf8 $ ls)
    where
        disablePoll l = hset ("poll:" `B.append` l) "active" "false"

getTakenCreated :: B.ByteString -> Redis (Either (Err T.Text) ([B.ByteString], [B.ByteString]))
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

{- Tests -}

--
tCreateUser :: Redis (Either Reply Status)
tCreateUser =
    let token = "23232"
        hash = "lklsdklsk"
        fingerprint = "223322"
    in  hmset ("user:" `B.append` hash) [("fingerprint", fingerprint),("token", token),("verified", "true")]

tCreatePoll :: B.ByteString -> B.ByteString -> Redis (Either (Err T.Text) (Ok T.Text))
tCreatePoll hash now =
    let poll = mockPoll
        pollid = "1"
        recipe = encodeStrict poll
        token = "token1"
        date_now d = encodeStrict . show $ d
    in  submit $ SCreate hash token pollid recipe now Nothing ""

tSubmitAnswers :: Redis (Either (Err T.Text) (Ok T.Text))
tSubmitAnswers =
    let hash = "lklsdklsk"
        fingerprint = "223322"
        pollid = "1"
        token= "token1"
        answers = ["0","0","1","1","1"]
    in submit $ STake hash token fingerprint pollid answers

tGetPoll :: Redis (Either (Err T.Text) (Poll, Maybe [Int], Maybe B.ByteString))
tGetPoll = let pollid = "1" in getPoll . SGet $ pollid

mockSetStageGetPoll :: Connection -> IO ()
mockSetStageGetPoll conn =
    let pollid = "1"
        hash = "testUser"
        actions now = do
            tCreateUser
            tCreatePoll hash now
            tSubmitAnswers
            getPoll . SGet $ pollid
    in  getNow >>= \now -> connDo conn $ actions (encodeStrict . show $ now) >>= \case
            Left err  -> liftIO . print . renderError $ err
            Right res -> liftIO . print . show $ res