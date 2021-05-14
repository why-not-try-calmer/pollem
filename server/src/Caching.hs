{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Caching where

import           AppTypes               (Poll (..))
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Reader   (MonadReader (ask),
                                         ReaderT (runReaderT), withReaderT)
import qualified Data.ByteString        as B
import qualified Data.HashMap.Strict    as HMS
import qualified Data.Text              as T
import           Data.Time              (UTCTime)
import           Database.MongoDB       (Pipe)
import           DatabaseM
import           DatabaseR
{-
    The Request Manager takes a Request for input and yields a result to the caller (a servant handler).
    The logic is simple: it looks up the PollCache if the Request can be satisfied, then looks up Redis, and if the request still
    cannot be satisfied, looks up MongoDb. It yields the Result to the caller as soon as possible, and does not
    write anything before it's returned. However, along the way between call and return, it pushes into a Queue the
    command which, if it has been fulfilled, would have prevented from looking further. Downstream of the channel, a worker
    works to repopulate either Redis or the in-memory cache even after the Request Manager has returned the requested
    result.
-}
data PollInCache = PollInCache {
    _poll       :: Poll,
    _isActive   :: Bool,
    _results    :: Maybe [Int],
    _lastLookUp :: UTCTime,
    _hasSecret  :: Maybe T.Text
}

type PollCache = MVar (HMS.HashMap B.ByteString PollInCache)

data Request = Redis { _redis :: DbReqR } | Mongo { _mongo :: DbReqM }

type Retry = (T.Text, Request)

type Retries = [Retry]

data Stores = Stores { _in_memory :: PollCache, _retries :: MVar Retries, _queue :: Chan Request, _dbpool :: Pipe }

data RequestManagerErrors = MongoConn T.Text | SomethingElse T.Text deriving (Show)

newtype RequestManager a = RequestManager { unManager :: ExceptT RequestManagerErrors (ReaderT Stores IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError RequestManagerErrors, MonadReader Stores)

initStores :: IO (Either RequestManagerErrors Stores)
initStores = do
    q <- newChan
    l <- newMVar []
    mem <- newMVar HMS.empty
    getAccess >>= \case
        Nothing   -> return . Left $ MongoConn "Failed to acquire pipe."
        Just pipe -> return . Right $ Stores mem l q pipe

addRetry :: Retry -> RequestManager ()
addRetry retry = ask >>= \env -> do
    let retries = _retries env
    liftIO $ modifyMVar_ retries (\ls -> return (retry:ls))

showRetries :: RequestManager ()
showRetries = ask >>= \env -> do
    let logs = _retries env
    read <- liftIO $ readMVar logs
    liftIO $ print . map fst $ read

exec :: Pipe -> Request -> IO ()
exec pipe (Mongo create_req@SMCreate{..}) =
    let doc = toBSON $ BSReq create_req
    in  runMongo pipe (createPoll doc)
exec pipe (Mongo close_req@SMClose{..}) = runMongo pipe (closePoll close_poll_id)
exec pipe (Mongo ask_req@SMAsk{..}) = runMongo pipe (userAsk ask_hash ask_token)
exec pipe (Mongo confirm_req@SMConfirm{..}) = runMongo pipe (userConfirm confirm_hash confirm_token confirm_fingerprint)
exec pipe (Mongo take_req@SMTake{..}) =
    let doc = toBSON $ BSReq take_req
    in  runMongo pipe (insertParticipation answers_poll_id doc)

enqueue :: Request -> RequestManager ()
enqueue req = ask >>= \env -> do
    let q = _queue env
    liftIO $ writeChan q req

dequeue :: RequestManager ()
dequeue = ask >>= \env -> do
    let q = _queue env
        pool = _dbpool env
        retries = _retries env
    nextReq <- liftIO $ readChan q
    liftIO (try $ exec pool nextReq) >>= \case
        -- catching all 'natural' MongoDB errors and rethrowing inside the RequestManager monad
        Left err -> let e = err :: SomeException in throwError $ MongoConn (T.pack . show $ err)
        Right _ -> pure ()

runRequestManager :: IO ()
runRequestManager =
    initStores >>= \case
    Right stores ->
        let req = ("Creating poll", Mongo $ SMCreate "ad" "ri" "en" "ET" "td" "er" (Just "et") "our")
            operations = do
                addRetry req
                showRetries
                addRetry req
                showRetries
                enqueue (snd req)
                dequeue
        in  runReaderT (runExceptT (unManager operations)) stores >>= \case
                Left err -> print $ "runReaderT: caught this error: " ++ show err
                Right _ -> print "Ok"

main = runRequestManager