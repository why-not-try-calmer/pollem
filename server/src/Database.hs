{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-} -- needed for either monad in hedis function signatures

module Database where

import AppData
import Database.Redis
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (void)
import qualified Data.ByteString as B
import           Data.Aeson
import qualified Data.Text as T

connDo action = withConnect openConnection (`runRedis` action) 

_connDo action = void $ withConnect openConnection (`runRedis` action)

openConnection :: ConnectInfo
openConnection = defaultConnectInfo {
    connectHost = "ec2-108-128-25-66.eu-west-1.compute.amazonaws.com",
    connectPort = PortNumber 14459,
    connectAuth = Just "p17df6aa47fbc3f8dfcbcbfba00334ecece8b39a921ed91d97f6a9eeefd8d1793"
}

createIfExist :: (RedisCtx m (Either a), MonadIO m) => Poll -> B.ByteString -> m ()
createIfExist poll pid = exists ("poll:" `B.append` pid) >>= \case
    Left s -> liftIO . print $ "Sorry, an error occurred"
    Right verdict -> 
        if verdict then liftIO . print $ "already exist"
        else do
            set pid (encode poll) 
            liftIO . print $ "added"
                        
main = case initPoll of 
    Just poll -> _connDo $ createIfExist poll "adrien"
    Nothing -> print "That's not really a poll you"