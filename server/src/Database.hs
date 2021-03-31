{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Database where

import           Compute
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson.Extra       (decodeStrict', encodeStrict)
import qualified Data.ByteString        as B
import           Data.ByteString.Char8  (readInt)
import           Data.Foldable          (foldl', traverse_)
import qualified Data.Map               as M
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Database.Redis
import           ErrorsReplies
import qualified ErrorsReplies          as R
import           HandlersDataTypes
import           Scheduler              (getNow)
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
        create_poll_active    :: B.ByteString
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
        confirm_hash        :: B.ByteString,
        confirm_fingerprint :: B.ByteString,
        confirm_token       :: B.ByteString
    } |
    SAnswer {
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
openConnection :: RedisConfig -> ConnectInfo
openConnection conf = defaultConnectInfo {
    connectHost = host conf,
    connectPort = PortNumber . fromInteger . port $ conf,
    connectAuth = HandlersDataTypes.auth conf
}

connDo :: RedisConfig -> Redis a -> IO a
connDo config action = withConnect (openConnection config) (`runRedis` action)

_connDo :: RedisConfig  -> Redis a -> IO ()
_connDo config = void . connDo config

dbErr = return . Left . R.Err Database $ mempty
borked = return . Left . R.Err BorkedData $ mempty
noUser = return . Left . R.Err UserNotExist $ mempty
notIn keyvals tested = not $ all (`elem` tested) keyvals

submit :: DbReq -> Redis (Either (Err T.Text) (Ok T.Text))
submit (SAsk hash token) = -- hash + token generated a first time from the handler
    let key = "user:" `B.append` hash
    in  exists key >>= \case
        Left err -> dbErr
        Right verdict ->
            if verdict then do
                hmset key [("token", token)]
                return . Right . R.Ok $ "Bear in mind that this email address is registered already. I will send you a verification email nonetheless. You don't need to use it unless you want to re-authenticate your email address."
            else do
                hmset key [("token", token),("verified", "false")] -- this structure is not typed! perhaps do it
                return . Right . R.Ok $ "Thanks, please check your email"
submit (SConfirm hash fingerprint token) = -- hash generated from the handler, token sent by the user, which on match with DB proves that the hash is from a confirmed email address.
    let key = "user:" `B.append` hash
    in  exists key >>= \case
        Left err -> return . Left . R.Err Database $ T.pack . show $ err
        Right found ->
            if not found then noUser
            else hget key "token" >>= \case
                Left _ -> dbErr
                Right mb_saved_token -> case mb_saved_token of
                    Nothing -> noUser
                    Just saved_token ->
                        if token == saved_token then do
                            hmset key [("fingerprint", fingerprint),("verified","true")]
                            return . Right . R.Ok $ "Thanks, you've successfully confirmed your email address."
                        else noUser
submit (SCreate hash token pollid recipe created_date isactive) =
    let pollid_txt = decodeUtf8 pollid
        userKey = "user:" `B.append` hash
    in  exists ("poll:" `B.append` pollid) >>= \case
        Left err -> dbErr
        Right verdict ->
            if verdict then return . Left . R.Err PollExists $ pollid_txt
            else hgetall userKey >>= \case
                Left _ -> noUser
                Right userdata ->
                    if notIn [("token",token), ("active", "true")] userdata then return . Left . R.Err UserNotVerified $ mempty
                    else do
                        hmset ("poll:" `B.append` pollid) [("author_token", hash),("recipe",recipe),("created_date",created_date),("active",isactive)] -- this structure is not typed! perhaps do it
                        return .  Right . R.Ok $ "Added poll " `T.append` pollid_txt
submit (SClose hash token pollid) =
    let userKey = "user:" `B.append` hash
        pollid_txt = decodeUtf8 pollid
    in  multiExec ( do
            polldata <- hgetall ("poll:" `B.append` pollid)
            userdata <- hgetall userKey
            return $ (,) <$> polldata <*> userdata
        ) >>= \case
            TxSuccess (pdata, udata) ->
                if notIn [("active","true"), ("author_token", token)] pdata then return . Left . R.Err Custom
                    $ "Either the poll was closed already, or you don't have closing rights." else
                if notIn [("verified", "true")] udata then return . Left . R.Err UserNotVerified $ mempty
                else do
                hset pollid "active" "false"
                return . Right . R.Ok $ "This poll was closed: " `T.append` pollid_txt
            _ -> return . Left . R.Err PollNotExist $ pollid_txt

submit (SAnswer hash token finger pollid answers) =
    let pollid_txt = decodeUtf8 pollid
        userKey = "user:" `B.append` hash
    in  multiExec ( do
            polldata <- hgetall ("poll:" `B.append` pollid)
            userdata <- hgetall userKey
            ismemberH <- sismember hash ("participants_hashes:" `B.append` pollid)
            ismemberF <- sismember hash ("participants_fingerprints:" `B.append` finger)
            return $ (,,,) <$> polldata <*> userdata <*> ismemberH <*> ismemberF
        ) >>= \case TxSuccess (pdata, udata, isH, isF) ->
                        if notIn [("active","true")] pdata  then return . Left . R.Err PollInactive $ pollid_txt else
                        if notIn [("verified", "true")] udata then return . Left . R.Err UserNotVerified $ decodeUtf8 hash else
                        if isF || isH then return . Left . R.Err PollTakenAlready $ pollid_txt
                        else multiExec ( do
                            sadd ("participants_hashes:" `B.append` pollid) [hash]
                            sadd ("participants_fingerprints:" `B.append` pollid) [finger]
                            lpush ("answers:" `B.append` pollid `B.append` ":" `B.append` hash) answers
                        ) >>= \case TxSuccess _ -> return . Right . R.Ok $ "Answers submitted successfully!"
                                    _  -> return . Left . R.Err Database $ "Database error"
                    _   -> return . Left . R.Err Database $ "User or poll data missing."

getPoll :: DbReq -> Redis (Either (Err T.Text) (Poll, [Int]))
getPoll (SGet pollid) =
    let pollid_txt = decodeUtf8 pollid
    in  exists ("poll:" `B.append` pollid) >>= \case
        Left err -> dbErr
        Right verdict ->
            if not verdict then return . Left . R.Err PollNotExist $ pollid_txt
            else smembers ("participants_hashes:" `B.append` pollid) >>= \case
                Right participants ->
                    let collectAnswers = sequence <$> traverse (`getAnswers` pollid) participants
                    in  multiExec ( do
                            answers <- collectAnswers
                            poll_meta_data <- hgetall ("poll:" `B.append` pollid)
                            return $ (,) <$> answers <*> poll_meta_data
                        )
                    >>= \case
                    TxError _ -> dbErr
                    TxSuccess (res, poll_raw) ->
                        let mb_decoded = sequence $ foldr decodeByteList [] res
                        in  case mb_decoded of
                            Just answers ->
                                case collect answers of
                                Left err -> return . Left $ err
                                Right collected ->
                                    let poll_metadata = M.fromList poll_raw
                                    in  case M.lookup "recipe" poll_metadata of
                                        Nothing -> borked
                                        Just recipe -> case decodeStrict' recipe :: Maybe Poll of
                                            Nothing -> borked
                                            Just poll -> return . Right $ (poll, reverse collected)
                            Nothing -> borked
    where   decodeByteList :: [B.ByteString] -> [Maybe [Int]] -> [Maybe [Int]]
            decodeByteList val acc = traverse (fmap fst . readInt) val : acc
            getAnswers p pollid =
                let key = "answers:" `B.append` pollid `B.append` ":" `B.append` p
                in  lrange key 0 (-1)
--

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
        isactive = "true"
        token = "token1"
        date_now d = encodeStrict . show $ d
    in  submit $ SCreate hash token pollid recipe now isactive

tSubmitAnswers :: Redis (Either (Err T.Text) (Ok T.Text))
tSubmitAnswers =
    let hash = "lklsdklsk"
        fingerprint = "223322"
        pollid = "1"
        token= "token1"
        answers = ["0","0","1","1","1"]
    in submit $ SAnswer hash token fingerprint pollid answers

tGetPoll :: Redis (Either (Err T.Text) (Poll, [Int]))
tGetPoll = let pollid = "1" in getPoll . SGet $ pollid

mockSetStageGetPoll :: IO ()
mockSetStageGetPoll =
    let pollid = "1"
        hash = "testUser"
        actions now = do
            tCreateUser
            tCreatePoll hash now
            tSubmitAnswers
            getPoll . SGet $ pollid
    in  getNow >>= \now -> connDo initRedisConfig $ actions (encodeStrict . show $ now) >>= \case
            Left err  -> liftIO . print . renderError $ err
            Right res -> liftIO . print . show $ res
