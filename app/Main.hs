module Main where

import Data.Monoid ((<>))
import Data.Time (getZonedTime)
import Network.Google.OAuth2
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.Environment
import System.Exit (die)
import System.FilePath ((</>))

import Handlers

main :: IO ()
main = do
    ci <- getEnv "SCH_CLIENT_ID"
    cs <- getEnv "SCH_CLIENT_SECRET"
    homeDir <- getHomeDirectory
    let c = OAuth2Client ci cs
        tokenFile = homeDir </> ".sch" </> "token.info"
        scope = "https://www.googleapis.com/auth/calendar"
    token <- getToken c tokenFile [scope]
    opts <- execParser optsParser
    case optCommand opts of
         List from to pretty -> do
             now <- getZonedTime
             case getTimePeriod now from to of
                  Just (f, t) -> getEvents token f t pretty
                  Nothing -> die "Unable to parse args"
         New sm ds lc "" st en -> createEvent token sm ds lc st en
         New sm ds lc dt st en -> do
             let st' = dt ++ "T" ++ st
                 en' = dt ++ "T" ++ en
             createEvent token sm ds lc st' en'

    where
        optsParser =
            info (helper <*> programOptions)
                 (fullDesc <>
                     header "sch - A CLI client of Google Calendar")
        programOptions = 
            Opts <$> hsubparser (listCommand <> newCommand)
        listCommand =
            command "ls" (info listOptions (progDesc "Show schedule"))
        listOptions =
            List <$> strArgument (metavar "date (e.g. today, thisweek, lastweek, YYYY-MM-DD)" <> help "From" <> value "")
                 <*> strArgument (metavar "date (e.g. YYYY-MM-DD)" <> help "To" <> value "")
                 <*> switch (help "Pretty Print" <> short 'p' <> long "pretty")
        newCommand =
            command "new" (info newOptions (progDesc "Create schedule"))
        newOptions =
            New <$> strArgument (metavar "Summary")
                <*> strOption (metavar "Description"
                              <> long "des"
                              <> value ""
                              )
                <*> strOption (metavar "Location"
                              <> long "loc"
                              <> value ""
                              )
                <*> strArgument (metavar "Date" <> value "")
                <*> strOption (metavar "Start"
                              <> short 'f'
                              <> long "from"
                              <> value "00:00"
                              )
                <*> strOption (metavar "End"
                              <> short 't'
                              <> long "to"
                              <> value "23:59"
                              )

data Opts = Opts { optCommand :: !Command }

data Command = List String String Bool
             | New String String String String String String
