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
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import           Database.Redis

-- Data types

data Submit (a :: *) where
    CreatePoll :: Integer -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> [B.ByteString] -> [Int] -> Submit a
    ClosePoll :: Integer -> Submit a
    AskToken :: B.ByteString  -> B.ByteString -> B.ByteString -> Submit a
    ConfirmToken :: B.ByteString -> B.ByteString -> Submit a
    AnswerPoll :: B.ByteString -> Integer -> [B.ByteString] -> Submit a
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

submit (CreatePoll pollid recipe startdate isactive authenticateonly participants results) = return "ok"
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
        Left _ -> return "Sorry and error occured"
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
submit (AnswerPoll hash pollid answers) = return "ok"

createIfExist poll pid = exists ("poll:" `B.append` pid) >>= \case
    Left s -> liftIO . print $ "Sorry, an error occurred"
    Right verdict ->
        if verdict then liftIO . print $ "already exist"
        else do
            set pid (encodeStrict poll)
            liftIO . print $ "added"
