{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import           AppData
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson.Extra       (encodeStrict)
import qualified Data.ByteString        as B
import           Data.Foldable          (traverse_)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Database.Redis

-- Data types

data Submit (a :: *) where
    CreatePoll :: B.ByteString  -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> Submit a
    ClosePoll :: B.ByteString -> Submit a
    AskToken :: B.ByteString  -> B.ByteString -> B.ByteString -> Submit a
    ConfirmToken :: B.ByteString -> B.ByteString -> Submit a
    AnswerPoll :: B.ByteString -> B.ByteString -> [(B.ByteString,B.ByteString)] -> Submit a
    deriving (Show, Eq)

-- Actions

openConnection :: ConnectInfo
openConnection = defaultConnectInfo {
    connectHost = "ec2-108-128-25-66.eu-west-1.compute.amazonaws.com",
    connectPort = PortNumber 14459,
    connectAuth = Just "p17df6aa47fbc3f8dfcbcbfba00334ecece8b39a921ed91d97f6a9eeefd8d1793"
}

connDo :: Redis a -> IO a
connDo action = withConnect openConnection (`runRedis` action)

_connDo :: Redis a -> IO ()
_connDo = void . connDo

submit (CreatePoll pollid recipe startdate isactive authenticateonly) =
    let pollid_txt = decodeUtf8 pollid
    in  exists ("poll:" `B.append` pollid) >>= \case
        Left err -> return (T.pack . show $ err)
        Right verdict ->
            if verdict then return $ "This poll already exist " `T.append` pollid_txt
            else do
                hmset pollid [("recipe",recipe),("startDate",startdate),("isActive",isactive),("authenticateOnly",authenticateonly)]
                return $ "Added poll " `T.append` pollid_txt

submit (ClosePoll pollid) = return "ok"
submit (AskToken hash fingerprint token) =
    let key = "user:" `B.append` hash
    in  exists key >>= \case
        Left err -> return (T.pack . show $ err)
        Right verdict ->
            if verdict then return "Sorry, email already exists. Please ask for a new token to clarify the situation."
            else do
                hmset key [("fingerprint", fingerprint),("token", token),("verified", "false")]
                return "Thanks, please check your email"
submit (ConfirmToken hash token) =
    let key = "user:" `B.append` hash
    in  exists key >>= \case
        Left err -> return (T.pack . show $ err)
        Right found ->
            if not found then return "Not email found. Please authenticate again (get a new token) in order to clarify the situation."
            else hget key "token" >>= \case
                Left _ -> return "Sorry and error occured"
                Right mb_saved_token -> case mb_saved_token of
                    Nothing -> return "Apparently there was no token here. Please authenticate again (ask for a new token). You can use any email address."
                    Just saved_token ->
                        if token == saved_token then do
                            hmset key [("verified","true")]
                            return "Thanks, you've successfully confirmed your email address."
                        else return "Sorry, but your token doesn't match our record. Please ask for a new token (authenticate)."
submit (AnswerPoll hash pollid answers) =
    let pollid_txt = decodeUtf8 pollid
    in  exists ("poll:" `B.append` pollid) >>= \case
        Left err -> return (T.pack . show $ err)
        Right verdict ->
            if not verdict then return $ "Sorry, you cannot participate to a poll that doesn't exists:" `T.append` pollid_txt
            else multiExec ( do
                sadd ("participants:" `B.append` pollid) [hash]
                hmset ("answers:" `B.append` pollid `B.append` hash) answers
            ) >>= \case TxSuccess _ -> return "Ok"
                        _  -> return "Unable to insert your answers, as a database error occurred. Please try again (later)."

--getPoll :: B.ByteString -> Redis (Either T.Text a)
--getPoll :: RedisCtx Queued f => B.ByteString -> Redis (Either T.Text [f [B.ByteString]])
getPoll pollid =
    let pollid_txt = decodeUtf8 pollid
    in  exists ("poll:" `B.append` pollid) >>= \case
        Left err -> return . Left $ T.pack . show $ err
        Right verdict ->
            if not verdict then return . Left $ "Sorry, you cannot participate to a poll that doesn't exists:" `T.append` pollid_txt
            else smembers ("participants:" `B.append` pollid) >>= \case
                Right participants ->
                    let collectAnswers = sequence <$> traverse (`getAnswers` pollid) participants
                    --let collectAnswers = sequence $ foldr (\p acc -> acc ++ p `getAnswers` pollid) [] participants
                    in  multiExec collectAnswers >>=
                    \case
                        TxError _ -> return . Left $ "An error happened, please try again."
                        TxSuccess res  -> return . Right $ res
    where   getAnswers p pollid = do
                let key = "answers:" `B.append` "12" `B.append` p
                lrange key 0 (-1)

main = do
    p <- connDo . getPoll $ "12"
    print "ok"
