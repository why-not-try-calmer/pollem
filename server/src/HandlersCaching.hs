{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
--{-# LANGUAGE RecordWildCards            #-}

module HandlersCaching where

import           AppTypes
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
import           Database.Redis         (Connection)
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
--

{- Requests Manager -}

--
data Connectors = Connectors { _mongopipe :: Pipe, _redisconnection :: Connection }

data Stores = Stores { _inmem :: PollCache, _retries :: MVar Retries, _postqueue :: Chan Target, _connectors :: Connectors }

data Target = Redis { _redis :: DbReqR } | Mongo { _mongo :: DbReqM }

type Retry = (T.Text, Target)

type Retries = [Retry]

data CmdManErrors = MongoErr T.Text | RedisErr T.Text | CacheErr T.Text | RequestManagerErr T.Text
    deriving(Show)

newtype RequestManager a = RequestManager { unManager :: ExceptT CmdManErrors (ReaderT Stores IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError CmdManErrors, MonadReader Stores)

initStores :: PollCache -> IO (Either CmdManErrors Stores)
initStores cache = do
    q <- newChan
    l <- newMVar []
    connector <- initRedisConnection connInfo
    getAccess >>= \case
        Nothing   -> return . Left $ MongoErr "Failed to acquire pipe."
        Just pipe -> return . Right $ Stores cache l q (Connectors pipe connector)

initRequestManager :: PollCache -> IO ()
initRequestManager cache =
    initStores cache >>= \case
    Left _ -> throwIO $ userError "Failed to initialize Cache."
    Right stores -> runReaderT (runExceptT (unManager (pure ()))) stores >>= \case
        Left err -> print $ "runReaderT: caught this error: " ++ show err
        Right _  -> print "runReaderT: Request manager initialized."

runRequestManagerWith :: Stores -> RequestManager a -> IO ()
runRequestManagerWith stores operations =
    runReaderT (runExceptT (unManager operations)) stores >>= \case
        Left err -> print $ "runReaderT: caught this error: " ++ show err
        Right _  -> pure ()

addRetry :: Retry -> RequestManager ()
addRetry retry = ask >>= \env -> do
    let retries = _retries env
    liftIO $ modifyMVar_ retries (\ls -> return (retry:ls))

showRetries :: RequestManager ()
showRetries = ask >>= \env -> do
    let logs = _retries env
    read <- liftIO $ readMVar logs
    liftIO $ print . map fst $ read
--

{- Requests -}

--
exec :: DbReqR -> RequestManager ()
exec req = ask >>= \env -> do
    let redisconn = _redisconnection . _connectors $ env
    res <- liftIO . connDo redisconn . submitR $ req
    case res of
        Left _ -> throwError . RequestManagerErr $ 
            "Unable to perform " `T.append` (T.pack . show $ req) `T.append` "against Redis."
        --Right _ -> 

handleRequests :: Stores -> DbReqR -> IO ()
handleRequests stores req =
    let redisconn = _redisconnection . _connectors $ stores
    in  runRequestManagerWith stores $ exec req

main :: IO ()
main = do
    cache <- initCache
    initStores cache >>= \case
        Right stores -> handleRequests stores (SAsk "1" "2")

