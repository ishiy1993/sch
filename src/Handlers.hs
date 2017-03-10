{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Lens ((^.), (^?), (<&>), ix, IxValue, Index, Ixed)
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Lens
import Data.Time
import Data.String (IsString)
import Data.Vector (Vector)
import Network.HTTP.Req

getEvents :: ByteString -> ZonedTime -> ZonedTime -> Bool -> IO ()
getEvents token f t pretty = do
    es <- queryEvents token f t
    mapM_ (putStrLn . formatEvent pretty) es

queryEvents :: ByteString -> ZonedTime -> ZonedTime -> IO Events
queryEvents token f t = do
    let showTime = formatTime defaultTimeLocale "%FT%T%z"
        opts = oAuth2Bearer token
            <> "singleEvents" =: ("true" :: String)
            <> "orderBy" =: ("startTime" :: String)
            <> "timeMin" =: showTime f
            <> "timeMax" =: showTime t
    res <- req GET eventUrl NoReqBody bsResponse opts
    let b = responseBody res
    return $ b ^. key "items" . _Array <&> (toEvent . (^. _Object))

createEvent :: ByteString
            -> String -> String -> String -> String -> String -> IO ()
createEvent token sm ds lc st en = do
    let opts = oAuth2Bearer token
        f = st ++ ":00+09:00"
        t = en ++ ":00+09:00"
        ev = EventSource sm ds lc f t
    res <- req POST eventUrl (ReqBodyJson ev) bsResponse opts
    putStrLn . formatEvent False . toEvent $ responseBody res ^. _Object

eventUrl :: Url 'Https
eventUrl = https "www.googleapis.com" /: "calendar" /: "v3"
             /: "calendars" /: "primary" /: "events"

data Event = Event
    { summary :: String
    , description :: Maybe String
    , location :: Maybe String
    , start :: String
    , end :: String
    }

toEvent :: (AsValue (IxValue t), IsString (Index t), Ixed t)
        => t -> Event
toEvent o = Event sm ds lc (fromJust $ st <|> st') (fromJust $ en <|> en')
    where
        sm = o ^. ix "summary" . _String . unpacked
        ds = o ^? ix "description" . _String . unpacked
        lc = o ^? ix "location" . _String . unpacked
        st = o ^? ix "start" . _Object . ix "dateTime" . _String . unpacked
        en = o ^? ix "end" . _Object . ix "dateTime" . _String . unpacked
        st' = o ^? ix "start" . _Object . ix "date" . _String . unpacked
        en' = o ^? ix "end" . _Object . ix "date" . _String . unpacked

formatEvent :: Bool -> Event -> String
formatEvent pretty (Event sm ds lc st en)
    | pretty = unlines [ "概要: " ++ sm
                       , "日時: " ++ t
                       , "場所: " ++ l
                       , "詳細: " ++ d
                       ]
    | otherwise = unwords [ t, sm, l, d]
    where
        szt = fromJust $ parseZonedTime st
        ezt = fromJust $ parseZonedTime en
        t = formatTimePeriod szt ezt
        l = fromMaybe "None" lc
        d = fromMaybe "None" ds

type Events = Vector Event

data EventSource = EventSource
    { summary :: String
    , description :: String
    , location :: String
    , start :: String
    , end :: String
    }

instance ToJSON EventSource where
    toJSON (EventSource sm ds lc st en) =
        object [ "summary" .= sm
               , "description" .= ds
               , "location" .= lc
               , "start" .= object ["dateTime" .= st]
               , "end" .= object ["dateTime" .= en]
               ]

toToday :: ZonedTime -> ZonedTime
toToday (ZonedTime lt tz) = mkZonedTime tz ld
    where ld = localDay lt

toNext :: ZonedTime -> Integer -> ZonedTime
toNext (ZonedTime lt' tz) dt = mkZonedTime tz ld
    where ld = addDays dt $ localDay lt'

mkZonedTime :: TimeZone -> Day -> ZonedTime
mkZonedTime tz d = ZonedTime lt tz
    where lt = LocalTime d midnight

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
        from = mkZonedTime tz d
        to = toNext from 1
    return (from, to)
getTimePeriod now f t = do
    d1 <- parseDay f
    d2 <- parseDay t
    let tz = zonedTimeZone now
        from = mkZonedTime tz d1
        to = mkZonedTime tz d2
    return (from, to)

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%F"

parseZonedTime :: String -> Maybe ZonedTime
parseZonedTime = parseTimeM True defaultTimeLocale "%FT%T%z"

formatTimePeriod :: ZonedTime -> ZonedTime -> String
formatTimePeriod szt ezt = unwords [showTime szt, "-", showTime ezt]
    where showTime = formatTime defaultTimeLocale "%F %R"

instance MonadHttp IO where
    handleHttpException = throwIO
