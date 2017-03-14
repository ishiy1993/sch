{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers where

import Control.Exception (throwIO)
import Control.Lens ((^.), (<&>))
import Data.Aeson.Lens
import Data.ByteString.Char8 (ByteString)
import Data.Monoid ((<>))
import qualified Data.Text.IO as T
import Data.Time (ZonedTime)
import Network.HTTP.Req

import Events
import Utils

getEvents :: ByteString -> ZonedTime -> ZonedTime -> Bool -> IO ()
getEvents token f t pretty = do
    es <- queryEvents token f t
    mapM_ (T.putStrLn . formatEvent pretty) es

queryEvents :: ByteString -> ZonedTime -> ZonedTime -> IO Events
queryEvents token f t = do
    let opts = oAuth2Bearer token
            <> "singleEvents" =: ("true" :: String)
            <> "orderBy" =: ("startTime" :: String)
            <> "timeMin" =: showTime f
            <> "timeMax" =: showTime t
    res <- req GET eventUrl NoReqBody bsResponse opts
    let b = responseBody res
    return $ b ^. key "items" . _Array <&> readEvent

createEvent :: ByteString -> String -> String -> String
            -> ZonedTime -> ZonedTime -> IO ()
createEvent token sm ds lc st en = do
    let ev = EventSource sm ds lc (showTime st) (showTime en)
    e <- postEvent token ev
    T.putStrLn . formatEvent False $ e

postEvent :: ByteString -> EventSource -> IO Event
postEvent token event = do
    let opts = oAuth2Bearer token
    res <- req POST eventUrl (ReqBodyJson event) bsResponse opts
    return . readEvent $ responseBody res

eventUrl :: Url 'Https
eventUrl = https "www.googleapis.com" /: "calendar" /: "v3"
             /: "calendars" /: "primary" /: "events"

instance MonadHttp IO where
    handleHttpException = throwIO
