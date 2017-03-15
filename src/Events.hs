{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Events where

import Control.Applicative ((<|>))
import Control.Lens ((^.), (^?), ix, view)
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)

import Utils

data Event = Event
    { summary :: Text
    , description :: Maybe Text
    , location :: Maybe Text
    , start :: Text
    , end :: Text
    }

type Events = Vector Event

readEvent :: AsValue s => s -> Event
readEvent = toEvent . view _Object
    where
        toEvent o = Event sm ds lc s e
            where
                sm = o ^. ix "summary" . _String
                ds = o ^? ix "description" . _String
                lc = o ^? ix "location" . _String
                st = o ^? ix "start" . _Object . ix "dateTime" . _String
                en = o ^? ix "end" . _Object . ix "dateTime" . _String
                st' = o ^? ix "start" . _Object . ix "date" . _String
                en' = o ^? ix "end" . _Object . ix "date" . _String
                s = fromJust $ st <|> st'
                e = fromJust $ en <|> en'

formatEvent :: Bool -> Event -> Text
formatEvent pretty (Event sm ds lc st en)
    | pretty = T.unlines [ "概要: " <> sm
                         , "日時: " <> t
                         , "場所: " <> l
                         , "詳細: " <> d
                         ]
    | otherwise = T.unwords [ t, sm, l, d]
    where
        szt = fromJust . parseZonedTime $ T.unpack st
        ezt = fromJust . parseZonedTime $ T.unpack en
        t = T.pack $ formatTimePeriod szt ezt
        l = fromMaybe "None" lc
        d = fromMaybe "None" ds

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
