{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.Redis
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

connDo action = withConnect openConnection (`runRedis` action) 

_connDo action = void $ withConnect openConnection (`runRedis` action)

openConnection :: ConnectInfo
openConnection = defaultConnectInfo {
    connectHost = "ec2-108-128-25-66.eu-west-1.compute.amazonaws.com",
    connectPort = PortNumber 14459,
    connectAuth = Just "p17df6aa47fbc3f8dfcbcbfba00334ecece8b39a921ed91d97f6a9eeefd8d1793"
}

addAndCheck :: p -> IO ()
addAndCheck k = connDo $ do
    hmset "users" [("Adrien", "Glauser")]
    exists "users" >>= \case
        Left e -> liftIO $ print e
        Right res -> liftIO $ print res

main = addAndCheck "adrien"