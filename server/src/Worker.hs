{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Worker where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class   (liftIO)
import           Data.Foldable            (foldl')
import qualified Data.HashMap.Strict      as HMS
import           Database                 (_connDo, connDo, disableNotifyPolls,
                                           getPollIdEndDate,
                                           initRedisConnection, notifyOnDisable)
import           Database.Redis           (Connection, info, keys, runRedis)
import qualified ErrorsReplies            as R
import           HandlersDataTypes        (PollCache, initCache)
import           Mailer                   (SendGridConfig, SendGridEmail)
import           Times                    (fresherThanOneMonth, getNow,
                                           isoOrCustom)

autoClose :: Connection -> PollCache -> IO ()
autoClose conn mvar = do
    print "Attempting to sweep..."
    now <- getNow
    res <- connDo conn $ getPollIdEndDate >>= \case
        Right pollidDate ->
            let accOutdated acc (i, d) = case isoOrCustom . show $ d of
                    Left notParsed -> acc
                    Right valid_date -> if valid_date > now then i : acc else acc
                collectedActiveOutdated = foldl' accOutdated [] pollidDate
            in  do
                {- disable & notifiy on disabled every poll whose endDate is in the past -}
                disabled <- disableNotifyPolls collectedActiveOutdated
                {- notifies all participants -}
                notified <- notifyOnDisable collectedActiveOutdated
                pure $ sequenceA [disabled, notified]
    case res of
        Left err -> print . R.renderError $ err
        Right _  -> print "Disabled and notified"
    {- purges cache from every entry that is more than 1-month old -}
    modifyMVar_ mvar $ pure . HMS.filter (\(_,_,_,date, _) -> fresherThanOneMonth now date)

runAutoClose :: Connection -> PollCache -> IO (Async())
runAutoClose conn pollcache =
    let sweep = autoClose conn pollcache
    in  async . forever $ do
        sweep
        print "Swept once and now sleeping for one hour."
        threadDelay $ 1000000 * 3600
        `catch` \e ->
            let e' = e :: SomeException
            in  do
                print $ "Caught exception, restarting in three hours " ++ show e'
                threadDelay $ 1000000 * 10800
