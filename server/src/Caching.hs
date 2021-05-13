{-# LANGUAGE OverloadedStrings #-}

module Caching where

import           AppTypes             (Poll)
import           Control.Concurrent
import           Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), withReaderT)
import qualified Data.ByteString      as B
import qualified Data.HashMap.Strict  as HMS
import qualified Data.Text            as T
import           Data.Time            (UTCTime)
import           DatabaseM
import           DatabaseR
import Control.Monad.IO.Class (MonadIO(liftIO))
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

type Logs = [(T.Text, T.Text)]

data RequestManager = RequestManager { _in_memory :: PollCache, _logs :: MVar Logs, _queue :: Chan Request }

initRequestManager :: IO RequestManager
initRequestManager = do
    q <- liftIO newChan
    l <- liftIO $ newMVar []
    mem <- liftIO $ newMVar HMS.empty  
    return $ RequestManager mem l q

showLogs :: ReaderT RequestManager IO ()
showLogs = ask >>= \env -> do
    let logs = _logs env
    read <- liftIO $ readMVar logs
    liftIO $ print read

addLogs :: ReaderT RequestManager IO ()
addLogs = ask >>= \env -> do
    let logs = _logs env
    liftIO $ modifyMVar_ logs (\ls -> return (("1","ok"):ls))

main :: IO ()
main = do
    env <- initRequestManager
    runReaderT (addLogs >> showLogs) env