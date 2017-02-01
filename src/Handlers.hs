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
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text.Lens
import Data.Time
import Data.String (IsString)
import Network.HTTP.Req
import System.Exit

getEvents :: ByteString -> String -> String -> Bool -> IO ()
getEvents token f t pretty = do
    let opts = oAuth2Bearer token
            <> "singleEvents" =: ("true" :: String)
            <> "orderBy" =: ("startTime" :: String)
            <> "timeMin" =: (f ++ "T00:00:00+09:00")
            <> "timeMax" =: (t ++ "T00:00:00+09:00")
    res <- req GET eventUrl NoReqBody bsResponse opts
    let b = responseBody res
        es = b ^. key "items" . _Array <&> (toEvent . (^. _Object))
    mapM_ (putStrLn . formatEvent pretty) es

createEvent :: ByteString
            -> String -> String -> String -> String -> String -> IO ()
createEvent token sm ds lc st en = do
    let opts = oAuth2Bearer token
        f = st ++ ":00+09:00"
        t = en ++ ":00+09:00"
        ev = EventSource sm ds lc f t
    res <- req POST eventUrl (ReqBodyJson ev) bsResponse opts
    putStrLn . formatEvent False . toEvent $ responseBody res ^. _Object

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
        sd = take 10 st
        ed = take 10 en
        s = take 5 $ drop 11 st
        e = take 5 $ drop 11 en
        t = sd ++ " " ++ s ++ " - " ++ ed ++ " " ++ e
        l = fromJust $ lc <|> Just "None"
        d = fromJust $ ds <|> Just "None"

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

getToday :: IO String
getToday = formatTime defaultTimeLocale "%F" <$> getZonedTime

getNext :: Integer -> IO String
getNext dt = do
    today <- localDay . zonedTimeToLocalTime <$> getZonedTime
    let next = addDays dt today
    return $ formatTime defaultTimeLocale "%F" next

getTimePeriod :: String -> String -> IO (String, String)
getTimePeriod "" "" = (,) <$> getToday <*> getNext 1
getTimePeriod "today" "" = (,) <$> getToday <*> getNext 1
getTimePeriod "thisweek" "" = (,) <$> getToday <*> getNext 8
getTimePeriod "lastweek" "" = (,) <$> getNext (-8) <*> getToday
getTimePeriod f "" = case parseDay f of
    Just d -> let d' = addDays 1 d in return (show d, show d')
    Nothing -> die "Unable to parse args"
getTimePeriod f t = case (parseDay f, parseDay t) of
    (Just _, Just t') -> return (f, show (addDays 1 t'))
    _ -> die "Unable to parse args"

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%F"

instance MonadHttp IO where
    handleHttpException = throwIO
