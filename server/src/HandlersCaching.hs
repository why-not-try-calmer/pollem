{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

module HandlersCaching where

import           AppTypes
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Reader   (MonadReader (ask),
                                         ReaderT (runReaderT), withReaderT)
import           Data.Aeson       (decodeStrict)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import           Data.Functor           (($>), (<&>))
import qualified Data.HashMap.Strict    as HMS
import           Data.Maybe             (catMaybes, fromMaybe)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.Time              (UTCTime)
import           Database.MongoDB       (Document, Pipe, Val (cast'), fetch,
                                         valueAt)
import           Database.Redis         (Connection)
import           DatabaseM
import           DatabaseR
import           ErrorsReplies          (Ok (Ok))
import           Times                  (getNow, isoOrCustom)

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
--

{- Operations -}

--
addRetry :: Retry -> RequestManager ()
addRetry retry = ask >>= \env -> do
    let retries = _retries env
    liftIO $ modifyMVar_ retries (\ls -> return (retry:ls))

showRetries :: RequestManager ()
showRetries = ask >>= \env -> do
    let logs = _retries env
    read <- liftIO $ readMVar logs
    liftIO $ print . map fst $ read

setTo :: DbReqR -> RequestManager ()
setTo req = ask >>= \env -> do
    let redisconn = _redisconnection . _connectors $ env
        mongopipe = _mongopipe . _connectors $ env
    redis_res <- liftIO . connDo redisconn . submitR $ req
    case redis_res of
        Left _ -> throwError . RequestManagerErr $ "Unable to perform " `T.append` (T.pack . show $ req) `T.append` "against Redis."
        Right _ -> liftIO ( do
            print "Successfully saved to Redis"
            runMongo mongopipe . redisToMongo $ req
            ) >>= \case
                Left _ -> throwError . RequestManagerErr $ "Unable to perform " `T.append` (T.pack . show $ req) `T.append` "against MongoDB."
                Right (Ok res) -> liftIO . print $ res
    where
        redisToMongo (SCreate a b c d e f g h) = submitM $
            SMCreate (decodeUtf8 a) (decodeUtf8 b) (decodeUtf8 c) (decodeUtf8 d) (decodeUtf8 e) (decodeUtf8 f) (fmap decodeUtf8 g) (decodeUtf8 h)
        redisToMongo (SClose a b c) = submitM $ SMClose (decodeUtf8 a) (decodeUtf8 b) (decodeUtf8 c)
        redisToMongo (SAsk x y) = submitM $ SMAsk (decodeUtf8 x) (decodeUtf8 y)
        redisToMongo (SConfirm a b c) = submitM $ SMConfirm (decodeUtf8 a) (decodeUtf8 b) (decodeUtf8 c)
        redisToMongo (STake a b c d e f) = submitM $
            SMTake (decodeUtf8 a) (decodeUtf8 b) (decodeUtf8 c) (decodeUtf8 d) (decodeUtf8 e) (map decodeUtf8 f)

getFrom :: DbReqR -> UTCTime -> RequestManager (Maybe PollInCache)
getFrom req@SGet{..} now = ask >>= \env -> do
    let redisconn = _redisconnection . _connectors $ env
        mongopipe = _mongopipe . _connectors $ env
    redis_res <- liftIO . connDo redisconn . getPoll $ req
    case redis_res of
        Left _ -> do
            mongo_res <- runMongo mongopipe . redisToMongo $ req
            case mongo_res of
                Nothing  -> throwError . MongoErr $ "Unable to find document."
                Just doc -> pure . Just . prepareDoc $ doc
        Right (p, ac, res, sec) -> pure . Just $ PollInCache p ac res now sec
    where
        redisToMongo (SGet pollid mb_secret) = getIfExistsPoll (decodeUtf8 pollid)
        prepareDoc doc =
            let isActive = cast' . valueAt "active" $ doc :: Maybe Bool
                poll = (cast' . valueAt "recipe" $ doc :: Maybe String) >>= decodeStrict . B8.pack :: Maybe Poll
                results = Nothing
                hasSecret = cast' . valueAt "secret" $ doc :: Maybe T.Text
                unwrap x = case x of Just res -> res
            in  PollInCache (unwrap poll) (unwrap isActive) results now (encodeUtf8 <$> hasSecret)

handleRequests :: DbReqR -> RequestManager (Maybe PollInCache )
handleRequests getReq@SGet{..} = ask >>= \stores -> do
    let mvar = _inmem stores
    (now, mmap) <- liftIO $ do
        now <- getNow
        mmap <- readMVar mvar
        pure (now, mmap)
    case HMS.lookup get_poll_id mmap of
        -- not in cache, trying DBs
        Nothing -> getFrom getReq now >>= \case
            -- nowhere to be found
            Nothing -> throwError . RequestManagerErr $ "Poll does not exist in cache or in database."
            -- found in DBs
            Just pic@PollInCache{..} ->
                if runningButMissing now _poll _hasSecret then throwError . RequestManagerErr $ "Bad secret." else
                    let cache = PollInCache _poll _isActive _results now _hasSecret in do
                    liftIO $ addToCache mvar cache
                    pure . Just $ cache
        -- found in cache
        Just pic@PollInCache{..} ->
            if runningButMissing now _poll _hasSecret then throwError . RequestManagerErr $ "Bad secret." else
            if poll_visible _poll || not _isActive then getFrom getReq now else do
                liftIO $ updateCache mvar get_poll_id now
                pure . Just $ PollInCache _poll _isActive _results now _hasSecret
    where
        runningButMissing now p sec =
            let mismatch = fromMaybe False $ (/=) <$> sec <*> get_mb_secret
                running a Nothing = False
                running a mb_b = case poll_endDate p of
                    Just b -> case isoOrCustom . T.unpack $ b of Right b_date -> a < b_date
                    _ -> False
            in  running now (poll_endDate p) && mismatch
        updateCache mvar pid now = modifyMVar_ mvar (pure . HMS.update (\poll_in_cache -> Just (poll_in_cache { _lastLookUp = now })) pid)
        addToCache mvar (PollInCache p active mb_scores now mb_secret_b) = modifyMVar_ mvar (pure . HMS.insert get_poll_id (PollInCache p active mb_scores now mb_secret_b))
handleRequests req = setTo req $> Nothing

initRequestManager :: PollCache -> IO ()
initRequestManager cache =
    initStores cache >>= \case
    Left _ -> throwIO $ userError "Failed to initialize Cache."
    Right stores -> runReaderT (runExceptT (unManager (pure ()))) stores >>= \case
        Left err -> print $ "runReaderT: caught this error: " ++ show err
        Right _  -> print "runReaderT: Request manager initialized."

runRequestManagerWith :: Stores -> RequestManager a -> IO (Either CmdManErrors a)
runRequestManagerWith stores operations = flip runReaderT stores $ runExceptT (unManager operations) >>= \case
    Left err  -> pure . Left . RequestManagerErr $ "Failed to run request"

main :: IO ()
main = do
    cache <- initCache
    initRequestManager cache
    initStores cache >>= \case
        Right stores -> do
            res <- runRequestManagerWith stores . handleRequests $ SGet "123" (Just "1234")
            print "ok"
