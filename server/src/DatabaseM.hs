{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module DatabaseM () where

import           Control.Exception              (SomeException, throwIO, try)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import qualified Data.Text                      as T
import           Database.MongoDB
import qualified Database.MongoDB.Transport.Tls as DbTLS
import           HandlersDataTypes

data MongoCreds = MongoCreds {
    shard :: String,
    user  :: T.Text,
    pwd   ::T.Text
} deriving (Eq, Show)

initMongCreds :: MongoCreds
initMongCreds = MongoCreds "cluster0-shard-00-02.cmocx.mongodb.net" "pollem-app" "FmUCY0OkZVHJ1MUY"

-- FIX ME: MongoAtlas tends to shuffle around the role of 'primary' versus 'secondary' shard
-- Make sure to call selectOK to avoid failing to authenticate

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
            insert "polls" $ pollToBSON aPoll
            findOne (select ["name" =: "Samuel"] "polls") >>= liftIO . print

aPoll :: Poll
aPoll = Poll "start_date" (Just "end_date") "question" "description" True True ["Answer A", "Answer B"]

data Meta = Meta Int Int Int Int Int (Maybe Int)

pollToBSON :: Poll -> [Field]
pollToBSON (Poll s e q d m v a) = 
    let def = ["startDate" =: val s, "question" =: val q, "description" =: val d, "multiple" =: val m, "visible" =: val v, "answers" =: val a]
    in  case e of
        Nothing -> def
        Just endDate -> def ++ ["endDate" =: val endDate]

metaToBSON (Meta h em t r s en) =
    let def = ["hash" =: val h, "email" =: val em, "token" =: val t, "recipe" =: val r, "startDate" =: val s]
    in  case en of
        Nothing -> def
        Just enDate -> def ++ ["endDate" =: val en]

main = try testAccess >>= \case
    Left e -> let e' = e :: SomeException in print ("Failed to run testAccess: " ++ show e)
    Right _ -> print "Success!"