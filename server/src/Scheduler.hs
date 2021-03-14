module Scheduler where

import           Data.Time                (ZonedTime, defaultTimeLocale,
                                           parseTimeM, zonedTimeToUTC)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX    (systemToPOSIXTime,
                                           utcTimeToPOSIXSeconds)
import           Data.Time.Clock.System   (getSystemTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import Control.Concurrent(threadDelay)
import Control.Concurrent.Async


isoOrCustom :: String -> Either String UTCTime
isoOrCustom s =
    let utc = iso8601ParseM s :: Maybe UTCTime
        zoned = iso8601ParseM s :: Maybe ZonedTime
    in  case utc of
        Just parsed -> Right parsed
        Nothing -> case zoned of
            Nothing -> toUTC s possibleFormats
            Just r  -> Right . zonedTimeToUTC $ r
    where
        possibleFormats = ["%a, %d %b %Y T %z", "%Y-%b-%dT%z","%a, %d %b %Y T %EZ", "%Y-%m-%d", "%FT%T", "%Y-%b-%dT%H:%M:%S", "%Y-%b-%dT%H:%M"] :: [String]
        toUTC s [] = Left s
        toUTC s (f:fs) = case parseTimeM True defaultTimeLocale f s of
            Nothing     -> toUTC s fs
            Just parsed -> Right parsed

schedule :: String -> IO ()
schedule s = case isoOrCustom s of
    Left s -> print $ "Error, couldn't match the given time " ++ s
    Right date -> getSystemTime >>= \now -> do
        let diff = utcTimeToPOSIXSeconds date - systemToPOSIXTime now
            (s,_) = properFraction (nominalDiffTimeToSeconds diff)
            micro = s * 1000000
        withAsync (waitFor micro) (butFirst s)

    where
        waitFor m = threadDelay m >> print "finished"
        butFirst s th = do
            print $ "Scheduled for in " ++ show s ++ " seconds"
            {- do more things here -}
            {- waiting for runtime thread from main to keep the show running if we want to test -}
            wait th
        
main = schedule "2021-03-14T12:45:14+01:00"
