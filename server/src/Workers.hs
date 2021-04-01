{-# LANGUAGE LambdaCase #-}
module Workers where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class   (liftIO)
import           Database                 (_connDo, connDo, disablePolls,
                                           initRedisConnection, sweeper)
import           Database.Redis           (Connection)
import qualified ErrorsReplies            as R
import           Scheduler                (getNow, isoOrCustom)

runSweeper :: Connection -> IO ()
runSweeper conn = 
    let toDo = withAsync ( do
            print "Sweeping once and then sleeping for one hour..."
            now <- getNow
            res <- connDo conn $ sweeper >>= \case
                Right pollidDate ->
                    let filtered (i, d) acc = case isoOrCustom . show $ d of
                            Left notParsed -> acc
                            Right valid_date -> if valid_date > now then i : acc else acc
                        outdated = foldr filtered [] pollidDate
                    in  disablePolls outdated
            case res of
                Left err  -> print . R.renderError $ err
                Right msg -> print . R.renderOk $ msg
            threadDelay $ 1000000 * 3600
            )
        while th = waitCatch th >>= \case
            Left e ->
                let e' = e :: SomeException
                in  do
                    print . show $ e
                    runSweeper conn
            Right _ -> runSweeper conn
    in  toDo while

main :: IO ()
main = initRedisConnection >>= runSweeper
