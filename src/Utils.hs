module Utils where

import Data.Maybe (fromJust)
import Data.Time

toToday :: ZonedTime -> ZonedTime
toToday (ZonedTime lt tz) = mkZonedTime tz ld midnight
    where ld = localDay lt

toNext :: ZonedTime -> Integer -> ZonedTime
toNext (ZonedTime lt' tz) dt = mkZonedTime tz ld midnight
    where ld = addDays dt $ localDay lt'

mkZonedTime :: TimeZone -> Day -> TimeOfDay -> ZonedTime
mkZonedTime tz d t = ZonedTime lt tz
    where lt = LocalTime d t

getTimePeriod :: ZonedTime -> String -> String
              -> Maybe (ZonedTime, ZonedTime)
getTimePeriod now "" "" =
    Just (toToday now, toNext now 1)
getTimePeriod now "today" "" =
    Just (toToday now, toNext now 1)
getTimePeriod now "thisweek" "" =
    Just (toToday now, toNext now 8)
getTimePeriod now "lastweek" "" =
    Just (toNext now (-8), toToday now)
getTimePeriod now f "" = do
    d <- parseDay f
    let tz = zonedTimeZone now
        from = mkZonedTime tz d midnight
        to = toNext from 1
    return (from, to)
getTimePeriod now f t = do
    d1 <- parseDay f
    d2 <- parseDay t
    let tz = zonedTimeZone now
        from = mkZonedTime tz d1 midnight
        to = mkZonedTime tz d2 midnight
    return (from, to)

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%F"

parseTimeOfDay :: String -> Maybe TimeOfDay
parseTimeOfDay = parseTimeM True defaultTimeLocale "%R"

parseDateTime :: TimeZone -> String -> Maybe ZonedTime
parseDateTime tz str = Just $ ZonedTime lt tz
    where
        parse :: ParseTime t => String -> t
        parse = fromJust . parseTimeM True defaultTimeLocale "%FT%R"
        lt = LocalTime (parse str) (parse str)

parseZonedTime :: String -> Maybe ZonedTime
parseZonedTime = parseTimeM True defaultTimeLocale "%FT%T%z"

showTime :: ZonedTime -> String
showTime = formatTime defaultTimeLocale "%FT%T%z"

formatTimePeriod :: ZonedTime -> ZonedTime -> String
formatTimePeriod szt ezt = unwords [showTime' szt, "-", showTime' ezt]
    where showTime' = formatTime defaultTimeLocale "%F %R"
