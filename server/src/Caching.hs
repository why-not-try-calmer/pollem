{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Caching where

import           AppTypes               (Poll)
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

data Stores = Stores { _in_memory :: PollCache, _retries :: MVar Retries, _queue :: Chan Request }

data ManagerErrors = A | B

newtype RequestManagerM a = RequestManagerM { unManager :: ExceptT ManagerErrors (ReaderT Stores IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Stores, MonadError ManagerErrors)

addRetryM :: Retry -> RequestManagerM ()
addRetryM retry = ask >>= \env -> do
    let retries = _retries env
    liftIO $ modifyMVar_ retries (\ls -> return (retry:ls))

runRequestManagerM :: IO (Either ManagerErrors ())
runRequestManagerM = do
    stores <- initStores
    let req = ("Creating poll", Mongo $ SMCreate "a" "b" "c" "d" "e" "f" (Just "g") "h")
        operations = addRetryM req
    runReaderT (runExceptT (unManager operations)) stores

initManager = do
    stores <- initStores
    let req = ("Creating poll", Mongo $ SMCreate "a" "b" "c" "d" "e" "f" (Just "g") "h")
        actions = addRetry req
    runReaderT actions stores
    print "ok"

initStores :: IO Stores
initStores = do
    q <- liftIO newChan
    l <- liftIO $ newMVar []
    mem <- liftIO $ newMVar HMS.empty
    return $ Stores mem l q

showRetries :: ReaderT Stores IO ()
showRetries = ask >>= \env -> do
    let logs = _retries env
    read <- liftIO $ readMVar logs
    liftIO $ print . map fst $ read

addRetry :: Retry -> ReaderT Stores IO ()
addRetry retry = ask >>= \env -> do
    let retries = _retries env
    liftIO $ modifyMVar_ retries (\ls -> return (retry:ls))

exec :: Exception e => Request -> IO (Either e ())
exec (Mongo create_req@SMCreate{..}) =
    let doc = toBSON $ BSReq create_req
    in  getAccess >>= \case
        Just pipe -> try $ runMongo pipe (createPoll doc) >> print "RunMongo: OK"
        Nothing   -> throwIO $ userError "Failed to connect"

enqueue :: Request -> ReaderT Stores IO ()
enqueue req = ask >>= \env -> do
    let q = _queue env
    liftIO $ writeChan q req

dequeue :: ReaderT Stores IO ()
dequeue = ask >>= \env -> do
    let q = _queue env
        retries = _retries env
    nextReq <- liftIO $ readChan q
    liftIO (exec nextReq) >>= \case
        Left e ->
            let e' = e :: SomeException
            in  do
                liftIO $ print "Failed to dequeue"
                addRetry (T.pack . show $ e', nextReq)
        Right _ -> liftIO $ print "Dequeued!"

main' = do
    env <- initStores
    let req = ("Creating poll", Mongo $ SMCreate "a" "b" "c" "d" "e" "f" (Just "g") "h")
        operations = do
            addRetry req
            showRetries
            enqueue (snd req)
            dequeue
    runReaderT operations env
