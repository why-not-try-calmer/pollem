{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module DatabaseM () where

import           Control.Exception              (SomeException, throwIO, try)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import qualified Data.Text                      as T
import           Database.MongoDB
import qualified Database.MongoDB.Transport.Tls as DbTLS

data MongoCreds = MongoCreds {
    shard :: String,
    user  :: T.Text,
    pwd   ::T.Text
} deriving (Eq, Show)

initMongCreds :: MongoCreds
initMongCreds = MongoCreds "cluster0-shard-00-01.cmocx.mongodb.net" "pollem-app" "FmUCY0OkZVHJ1MUY"

testAccess :: IO ()
testAccess =
    let creds = initMongCreds
    in  try (DbTLS.connect (shard creds) (PortNumber 27017)) >>= \case
        Left e -> let e'= e :: SomeException in print ("Error while trying to connect: " ++ show e)
        Right pipe -> do
            authorized <- access pipe UnconfirmedWrites "admin" $ auth (user creds) (pwd creds)
            if authorized then access pipe master "pollem" runIt else throwIO $ userError "Failed to authenticate."
            close pipe
    where
        runIt = do
            insert "test" ["name" =: "Samuel"]
            findOne (select ["name" =: "Samuel"] "test") >>= liftIO . print
