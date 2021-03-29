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
import           Data.Aeson.Extra       (encodeStrict)
import qualified Data.ByteString        as B
import           Data.Foldable          (foldl', traverse_)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Database.Redis
import           ErrorsReplies
import qualified ErrorsReplies          as ER

-- Data types

data SubmitPoll = SubmitPoll {
    create_poll_id        :: B.ByteString,
    create_poll_recipe    ::  B.ByteString,
    create_poll_startDate :: B.ByteString,
    create_poll_active    :: B.ByteString
}

newtype SubmitClosePoll = SubmitClosePoll { close_poll_id :: B.ByteString }

data SubmitAskToken = SubmitAskToken {
    ask_has         :: B.ByteString,
    ask_fingerprint :: B.ByteString,
    ask_token       :: B.ByteString
}

data SubmitConfirmToken = SubmitConfirmToken {
    confirm_hash  :: B.ByteString,
    confirm_token :: B.ByteString
}

data SubmitAnswers = SubmitAnswers {
    answers_hash        :: B.ByteString,
    answers_fingerprint :: B.ByteString,
    answers_poll_id     :: B.ByteString,
    answers_answers     :: B.ByteString
}

data Submit a where
    SPoll :: SubmitPoll -> Submit a
    SClosePoll :: SubmitClosePoll -> Submit a
    SAskToken :: SubmitAskToken -> Submit a
    SConfirmToken :: SubmitConfirmToken -> Submit a
    SAnswerPoll :: SubmitAnswers -> Submit a

-- Requests to database

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

submit :: Submit a -> Redis (Either (Err T.Text) (Ok T.Text))
submit (SPoll (SubmitPoll pollid recipe startdate isactive)) =
    let pollid_txt = decodeUtf8 pollid
    in  exists ("poll:" `B.append` pollid) >>= \case
        Left err -> return . Left . ER.Err Database $ mempty
        Right verdict ->
            if verdict then return . Left . ER.Err PollExists $ pollid_txt
            else do
                hmset pollid [("recipe",recipe),("startDate",startdate),("active",isactive)]
                return .  Right . ER.Ok $ "Added poll " `T.append` pollid_txt

submit (SClosePoll (SubmitClosePoll pollid)) = return . Right . ER.Ok $ "ok"
submit (SAskToken (SubmitAskToken hash fingerprint token)) =
    let key = "user:" `B.append` hash
    in  exists key >>= \case
        Left err -> return . Left . ER.Err UserNotExist $ T.pack . show $ err
        Right verdict ->
            if verdict then return .  Left .  ER.Err EmailTaken $ decodeUtf8 hash
            else do
                hmset key [("fingerprint", fingerprint),("token", token),("verified", "false")]
                return . Right . ER.Ok $ "Thanks, please check your email"
submit (SConfirmToken (SubmitConfirmToken hash token)) =
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
submit (SAnswerPoll (SubmitAnswers hash finger pollid answers)) =
    let pollid_txt = decodeUtf8 pollid
    in  hgetall ("poll:" `B.append` pollid) >>= \case
            Left err -> return . Left .ER.Err PollNotExist $ pollid_txt
            Right keys_values -> do
                let userKey = "user:" `B.append` hash
                if not $ meetConditions [("active","true")] keys_values then return . Left . ER.Err PollInactive $ pollid_txt
                else hgetall userKey >>= \case
                    Left err -> return . Left . ER.Err UserNotExist $ decodeUtf8 hash
                    Right user_keys ->
                        if not $ meetConditions [("verified", "true")] user_keys then return . Left . ER.Err UserUnverified $ decodeUtf8 hash
                        else sismember hash ("participants_hashes:" `B.append` pollid) >>= \case
                            Left err -> return . Left . ER.Err Database $ mempty
                            Right verdict ->
                                if not verdict then return . Left . ER.Err PollTakenAlready $ pollid_txt
                                else sismember hash ("participants_fingerprints:" `B.append` finger) >>= \case
                                    Left err -> return . Left . ER.Err Database $ mempty
                                    Right verdict ->
                                        if not verdict then return . Left . ER.Err PollTakenAlready $ pollid_txt
                                        else multiExec ( do
                                            sadd ("participants_hashes:" `B.append` pollid) [hash]
                                            sadd ("participants_fingerprints" `B.append` pollid) [finger]
                                            set ("answers:" `B.append` pollid `B.append` hash) answers
                                        ) >>= \case TxSuccess _ -> return . Right . ER.Ok $ "Answers submitted successfully!"
                                                    _  -> return . Left . ER.Err Database $ "Database error"
    where   meetConditions keyvals = all (`elem` keyvals)


getPoll :: B.ByteString -> Redis (Either T.Text [[B.ByteString]])
getPoll pollid =
    let pollid_txt = decodeUtf8 pollid
    in  exists ("poll:" `B.append` pollid) >>= \case
        Left err -> return . Left $ T.pack . show $ err
        Right verdict ->
            if not verdict then return . Left $ "Sorry, you cannot participate to a poll that doesn't exists:" `T.append` pollid_txt
            else smembers ("participants_hashes:" `B.append` pollid) >>= \case
                Right participants -> do
                    -- must do something about empty participants
                    let collectAnswers = sequence <$> traverse (`getAnswers` pollid) participants
                    multiExec collectAnswers >>=
                        \case
                            TxError _ -> return . Left $ "An error happened, please try again."
                            TxSuccess res  -> return $ Right res
    where   getAnswers p pollid = do
                let key = "answers:" `B.append` pollid `B.append` ":" `B.append` p
                lrange key 0 (-1)
