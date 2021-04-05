module Scheduler where

import           Control.Concurrent       (threadDelay)
import           Data.Time                (ZonedTime, defaultTimeLocale,
                                           parseTimeM, zonedTimeToUTC)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX    (systemToPOSIXTime,
                                           utcTimeToPOSIXSeconds)
import           Data.Time.Clock.System   (getSystemTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)


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
    Right date -> getSystemTime >>= \now ->
        let diff = utcTimeToPOSIXSeconds date - systemToPOSIXTime now
            secs = fst . properFraction . nominalDiffTimeToSeconds $ diff
            micros = secs * 1000000
        in  if secs <= 0 then print "Error, cannot schedule past events."
            else do
                print $ "Scheduled for in " ++ show secs ++ " seconds"
                threadDelay micros
                print "Finished."

olderThanOneMonth d now =
    let diff = utcTimeToPOSIXSeconds d - systemToPOSIXTime now
        secs = fst . properFraction . nominalDiffTimeToSeconds $ diff
        oneMonth = 2592000
    in secs > oneMonth


getNow :: IO UTCTime
getNow = getCurrentTime

-- main = schedule "2021-03-14T14:15:14+01:00"
