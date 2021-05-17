{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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

data Stores = Stores { _inmem :: PollCache, _retries :: MVar Retries, _queue :: Chan Target, _connectors :: Connectors }

data Target = Redis { _redis :: DbReqR } | Mongo { _mongo :: DbReqM }

type Retry = (T.Text, Target)

type Retries = [Retry]

data CmdManErrors = MongoErr T.Text | RedisErr T.Text | CacheErr T.Text deriving (Show)

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

exec :: Connectors -> Target -> IO ()
exec (Connectors pipe _) (Mongo create_req@SMCreate{..}) =
    let doc = toBSON $ BSReq create_req
    in  runMongo pipe (createPoll doc)
exec (Connectors pipe _) (Mongo close_req@SMClose{..}) = runMongo pipe (closePoll close_poll_id)
exec (Connectors pipe _) (Mongo ask_req@SMAsk{..}) = runMongo pipe (userAsk ask_hash ask_token)
exec (Connectors pipe _) (Mongo confirm_req@SMConfirm{..}) = runMongo pipe (userConfirm confirm_hash confirm_token confirm_fingerprint)
exec (Connectors pipe _) (Mongo take_req@SMTake{..}) =
    let doc = toBSON $ BSReq take_req
    in  runMongo pipe (insertParticipation answers_poll_id doc)

enqueue :: Target -> RequestManager ()
enqueue req = ask >>= \env -> do
    let q = _queue env
    liftIO $ writeChan q req

dequeue :: RequestManager ()
dequeue = ask >>= \env -> do
    let q = _queue env
        connectors = _connectors env
        retries = _retries env
    nextReq <- liftIO $ readChan q
    liftIO (try $ exec connectors nextReq) >>= \case
        -- catching all 'natural' MongoDB errors and rethrowing inside the RequestManager monad
        Left err -> let e = err :: SomeException in throwError $ CacheErr (T.pack . show $ err)
        Right _ -> pure ()
--

{- Requests -}

--
handleRequests :: Stores -> DbReqR -> IO ()
handleRequests stores (SAsk hash token) =
    let newreq = ("Creating poll", Mongo $ SMCreate "ad" "ri" "en" "ET" "td" "er" (Just "et") "our")
    in  do
        runRequestManagerWith stores $ do
            addRetry newreq
            showRetries
        print "ok"

main :: IO ()
main = do
    cache <- initCache
    initStores cache >>= \case
        Right stores -> handleRequests stores (SAsk "1" "2")
