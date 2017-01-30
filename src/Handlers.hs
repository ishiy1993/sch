{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Handlers where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Lens
import Data.Aeson.Lens
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text.Lens
import Data.Time
import Data.String (IsString)
import Network.HTTP.Req

getEvents :: ByteString -> String -> String -> IO ()
getEvents token f t = do
    let opts = oAuth2Bearer token
            <> "singleEvents" =: ("true" :: String)
            <> "orderBy" =: ("startTime" :: String)
            <> "timeMin" =: (f ++ "T00:00:00+09:00")
            <> "timeMax" =: (t ++ "T00:00:00+09:00")
    res <- req GET eventUrl NoReqBody bsResponse opts
    let b = responseBody res
        es = b ^. key "items" . _Array <&> (toEvent . (^. _Object))
    mapM_ (putStrLn . formatEvent False) es

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

getToday :: IO String
getToday = formatTime defaultTimeLocale "%F" <$> getZonedTime

getNext :: Integer -> IO String
getNext dt = do
    today <- localDay . zonedTimeToLocalTime <$> getZonedTime
    let next = addDays dt today
    return $ formatTime defaultTimeLocale "%F" next

instance MonadHttp IO where
    handleHttpException = throwIO
