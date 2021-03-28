{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
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

data Submit (a :: *) where
    CreatePoll :: B.ByteString  -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> Submit a
    ClosePoll :: B.ByteString -> Submit a
    AskToken :: B.ByteString  -> B.ByteString -> B.ByteString -> Submit a
    ConfirmToken :: B.ByteString -> B.ByteString -> Submit a
    AnswerPoll :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> Submit a
    deriving (Show, Eq)

-- Actions

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

submit (CreatePoll pollid recipe startdate isactive authenticateonly) =
    let pollid_txt = decodeUtf8 pollid
    in  exists ("poll:" `B.append` pollid) >>= \case
        Left err -> return . Left . ER.Err UserNotExist $ T.pack . show $ err
        Right verdict ->
            if verdict then return . Left . ER.Err PollExists $ "This poll already exist " `T.append` pollid_txt
            else do
                hmset pollid [("recipe",recipe),("startDate",startdate),("isActive",isactive),("authenticateOnly",authenticateonly)]
                return .  Right . ER.Ok $ "Added poll " `T.append` pollid_txt

submit (ClosePoll pollid) = return . Right . ER.Ok $ "ok"
submit (AskToken hash fingerprint token) =
    let key = "user:" `B.append` hash
    in  exists key >>= \case
        Left err -> return . Left . ER.Err UserNotExist $ T.pack . show $ err
        Right verdict ->
            if verdict then return .  Left .  ER.Err EmailTaken $ decodeUtf8 hash
            else do
                hmset key [("fingerprint", fingerprint),("token", token),("verified", "false")]
                return . Right . ER.Ok $ "Thanks, please check your email"
submit (ConfirmToken hash token) =
    let key = "user:" `B.append` hash
    in  exists key >>= \case
        Left err -> return . Left . ER.Err Database $ T.pack . show $ err
        Right found ->
            if not found then return . Left . ER.Err UserNotExist $ "Not email found. Please authenticate again (get a new token) in order to clarify the situation."
            else hget key "token" >>= \case
                Left _ -> return . Left . ER.Err Database $ "Sorry and error occured"
                Right mb_saved_token -> case mb_saved_token of
                    Nothing -> return . Left . ER.Err UserNotExist $ "Apparently there was no token here. Please authenticate again (ask for a new token). You can use any email address."
                    Just saved_token ->
                        if token == saved_token then do
                            hmset key [("verified","true")]
                            return . Right . ER.Ok $ "Thanks, you've successfully confirmed your email address."
                        else return . Left . ER.Err UserNotExist $ "Sorry, but your token doesn't match our record. Please ask for a new token (authenticate)."
submit (AnswerPoll hash finger pollid answers) =
    let pollid_txt = decodeUtf8 pollid
    in  hgetall ("poll:" `B.append` pollid) >>= \case
            Left err -> return . Left .ER.Err UserNotExist $ "Sorry, you cannot participate to a poll that doesn't exists:" `T.append` pollid_txt
            Right keys_values -> do
                let userKey = "user:" `B.append` hash
                exists userKey >>= \case
                    Right verdict ->
                        if not verdict then return . Left . ER.Err UserNotExist $ decodeUtf8 hash
                        else multiExec ( do
                            sadd ("participants:" `B.append` pollid) [hash]
                            set ("answers:" `B.append` pollid `B.append` hash) answers
                        ) >>= \case TxSuccess _ -> return . Right . ER.Ok $ "Answers submitted successfully!"
                                    _  -> return . Left . ER.Err Database $ "Unable to insert your answers, as a database error occurred. Please try again (later)."

getPoll :: B.ByteString -> Redis (Either T.Text [[B.ByteString]])
getPoll pollid =
    let pollid_txt = decodeUtf8 pollid
    in  exists ("poll:" `B.append` pollid) >>= \case
        Left err -> return . Left $ T.pack . show $ err
        Right verdict ->
            if not verdict then return . Left $ "Sorry, you cannot participate to a poll that doesn't exists:" `T.append` pollid_txt
            else smembers ("participants:" `B.append` pollid) >>= \case
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
