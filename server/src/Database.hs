{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import           AppData
import           Compute
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson.Extra       (decodeStrict', encodeStrict, decodeStrict)
import qualified Data.ByteString        as B
import           Data.Foldable          (foldl', traverse_)
import qualified Data.Map               as M
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Database.Redis
import           ErrorsReplies
import qualified ErrorsReplies          as ER
import Scheduler (getNow)
--

{-- Requests to db: types --}

--
data DbReq =
    SPoll {
        create_poll_id        :: B.ByteString,
        create_poll_recipe    ::  B.ByteString,
        create_poll_startDate :: B.ByteString,
        create_poll_active    :: B.ByteString
        } |
    SClose  { close_poll_id :: B.ByteString } |
    SAsk {
        ask_has         :: B.ByteString,
        ask_fingerprint :: B.ByteString,
        ask_token       :: B.ByteString
    } |
    SConfirm {
        confirm_hash  :: B.ByteString,
        confirm_token :: B.ByteString
    } |
    SAnswer {
        answers_hash        :: B.ByteString,
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
    connectAuth = AppData.auth conf
}

connDo :: RedisConfig -> Redis a -> IO a
connDo config action = withConnect (openConnection config) (`runRedis` action)

_connDo :: RedisConfig  -> Redis a -> IO ()
_connDo config = void . connDo config

submit :: DbReq -> Redis (Either (Err T.Text) (Ok T.Text))
submit (SPoll pollid recipe startdate isactive) =
    let pollid_txt = decodeUtf8 pollid
    in  exists ("poll:" `B.append` pollid) >>= \case
        Left err -> return . Left . ER.Err Database $ mempty
        Right verdict ->
            if verdict then return . Left . ER.Err PollExists $ pollid_txt
            else do
                hmset ("poll:" `B.append` pollid) [("recipe",recipe),("startDate",startdate),("active",isactive)] -- this structure is not typed! perhaps do it
                return .  Right . ER.Ok $ "Added poll " `T.append` pollid_txt

submit (SClose pollid) = return . Right . ER.Ok $ "ok"
submit (SAsk hash fingerprint token) =
    let key = "user:" `B.append` hash
    in  exists key >>= \case
        Left err -> return . Left . ER.Err UserNotExist $ T.pack . show $ err
        Right verdict ->
            if verdict then return .  Left .  ER.Err EmailTaken $ decodeUtf8 hash
            else do
                hmset key [("fingerprint", fingerprint),("token", token),("verified", "false")] -- this structure is not typed! perhaps do it
                return . Right . ER.Ok $ "Thanks, please check your email"
submit (SConfirm hash token) =
    let key = "user:" `B.append` hash
    in  exists key >>= \case
        Left err -> return . Left . ER.Err Database $ T.pack . show $ err
        Right found ->
            if not found then return . Left . ER.Err UserNotExist $ decodeUtf8 hash
            else hget key "token" >>= \case
                Left _ -> return . Left . ER.Err Database $ decodeUtf8 key
                Right mb_saved_token -> case mb_saved_token of
                    Nothing -> return . Left . ER.Err UserNotExist $ decodeUtf8 hash
                    Just saved_token ->
                        if token == saved_token then do
                            hmset key [("verified","true")]
                            return . Right . ER.Ok $ "Thanks, you've successfully confirmed your email address."
                        else return . Left . ER.Err UserNotExist $ decodeUtf8 hash
submit (SAnswer hash finger pollid answers) =
    let pollid_txt = decodeUtf8 pollid
        userKey = "user:" `B.append` hash
    in  multiExec ( do
            polldata <- hgetall ("poll:" `B.append` pollid)
            userdata <- hgetall userKey
            ismemberH <- sismember hash ("participants_hashes:" `B.append` pollid)
            ismemberF <- sismember hash ("participants_fingerprints:" `B.append` finger)
            return $ (,,,) <$> polldata <*> userdata <*> ismemberH <*> ismemberF
        ) >>= \case TxSuccess (pdata, udata, isH, isF) ->
                        if not $ meetConditions pdata [("active","true")] then do
                            liftIO . print $ pdata
                            return . Left . ER.Err PollInactive $ pollid_txt else
                        if not $ meetConditions udata [("verified", "true")] then do
                            liftIO . print $ udata
                            return . Left . ER.Err UserUnverified $ decodeUtf8 hash else
                        if isF || isH then return . Left . ER.Err PollTakenAlready $ pollid_txt
                        else multiExec ( do
                            sadd ("participants_hashes:" `B.append` pollid) [hash]
                            sadd ("participants_fingerprints:" `B.append` pollid) [finger]
                            lpush ("answers:" `B.append` pollid `B.append` ":" `B.append` hash) answers
                        ) >>= \case TxSuccess _ -> return . Right . ER.Ok $ "Answers submitted successfully!"
                                    _  -> return . Left . ER.Err Database $ "Database error"
                    _   -> return . Left . ER.Err Database $ "User or poll data missing."
        where   meetConditions keyvals = all (`elem` keyvals)

getPoll :: DbReq -> Redis (Either (Err T.Text) Poll)
getPoll (SGet pollid) =
    let pollid_txt = decodeUtf8 pollid
    in  exists ("poll:" `B.append` pollid) >>= \case
        Left err -> dbErr
        Right verdict ->
            if not verdict then return . Left . ER.Err PollNotExist $ pollid_txt
            else smembers ("participants_hashes:" `B.append` pollid) >>= \case
                Right participants -> do
                    let collectAnswers = sequence <$> traverse (`getAnswers` pollid) participants
                    multiExec ( do
                        answers <- collectAnswers
                        poll_meta_data <- hgetall ("poll:" `B.append` pollid)
                        return $ (,) <$> answers <*> poll_meta_data
                        ) >>= \case
                        TxError _ -> dbErr
                        TxSuccess (res, poll_raw)  ->
                            let mb_decoded = sequence . foldr decodeByteList [] $ res :: Maybe [[Int]]
                            -- mb_decoded = sequence $ foldr decodeByteList [] res
                                x = map (map show) res :: [[String]]
                            in  case mb_decoded of
                                Just answers -> case collect answers of
                                    Left err -> return . Left $ err
                                    Right collected ->
                                        let poll_metadata = M.fromList poll_raw
                                        in  case M.lookup "recipe" poll_metadata of
                                            Nothing -> borked
                                            Just recipe -> case decodeStrict' recipe :: Maybe Poll of
                                                Nothing -> borked
                                                Just poll -> return . Right $ poll
                                Nothing -> do
                                    liftIO . print . show $ x
                                    borked
    where   dbErr = return . Left . ER.Err Database $ mempty
            borked = return . Left . ER.Err BorkedData $ mempty 
            getAnswers p pollid =
                let key = "answers:" `B.append` pollid `B.append` ":" `B.append` p
                in  lrange key 0 (-1)
            decodeByteList val acc = acc <> map decodeStrict' val

mockSetStageGetPoll =
    let token = "23232"
        hash = "lklsdklsk"
        fingerprint = "223322"
        poll = mockPoll
        answers = ["0","1","1","0","0"]
        pollid = "1"
        recipe = encodeStrict poll
        isactive = "true"
        date_now d = encodeUtf8 . T.pack . show $ d
        actions now = do
            -- hmset ("user:" `B.append` hash) [("fingerprint", fingerprint),("token", token),("verified", "true")]
            -- submit $ SPoll pollid recipe now isactive
            -- submit $ SAnswer hash fingerprint pollid answers
            getPoll . SGet $ pollid
    in  getNow >>= \now -> connDo initRedisConfig $ actions (date_now now) >>= \case
            Left err -> liftIO . print . renderError $ err
            Right res -> liftIO . print . show $ res

main = mockSetStageGetPoll