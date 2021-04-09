{-# LANGUAGE LambdaCase #-}
module Workers where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.HashMap.Strict      as HMS
import           Data.Time                (UTCTime (UTCTime))
import           Data.Time.Clock.System   (getSystemTime)
import           Database                 (_connDo, connDo, disablePolls,
                                           getResults, initRedisConnection)
import           Database.Redis           (Connection)
import qualified ErrorsReplies            as R
import           HandlersDataTypes        (PollCache)
import           Scheduler                (fresherThanOneMonth, getNow,
                                           isoOrCustom)

sweeperWorker :: Connection -> PollCache -> IO ()
sweeperWorker conn mvar = do
    now <- getNow
    now_system <- getSystemTime
    res <- connDo conn $ getResults >>= \case
        Right pollidDate ->
            let accOutdated (i, d) acc = case isoOrCustom . show $ d of
                    Left notParsed -> acc
                    Right valid_date -> if valid_date > now then i : acc else acc
                collectedActiveOutdated = foldr accOutdated [] pollidDate
            {- disable every poll whose endDate is in the past -}
            in  disablePolls collectedActiveOutdated
    case res of
        Left err  -> print . R.renderError $ err
        Right msg -> print . R.renderOk $ msg
    {- purges cache from every entry that is more than 1-month old -}
    modifyMVar_ mvar $ pure . HMS.filter (\(_,_,date) -> fresherThanOneMonth date now_system)

runSweeperWorker :: PollCache -> Connection -> IO (Async())
runSweeperWorker mvar conn =
    let sweep = sweeperWorker conn mvar
    in  async . forever $ do
        sweep
        print "Swept once and now sleeping for one hour."
        threadDelay $ 1000000 * 3600
        `catch` \e ->
            let e' = e :: SomeException
            in  print "caught exception, restarting..."
