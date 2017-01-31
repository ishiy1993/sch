module Main where

import Data.Monoid ((<>))
import Network.Google.OAuth2
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.Environment
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
             (f, t) <- getTimePeriod from to
             getEvents token f t pretty

    where
        optsParser =
            info (helper <*> programOptions)
                 (fullDesc <>
                     header "sch - A CLI client of Google Calendar")
        programOptions = 
            Opts <$> hsubparser listCommand
        listCommand =
            command "ls" (info listOptions (progDesc "Show schedule"))
        listOptions =
            List <$> strArgument (metavar "date" <> help "From" <> value "")
                 <*> strArgument (metavar "date" <> help "To" <> value "")
                 <*> switch (help "Pretty Print" <> short 'p' <> long "pretty")

data Opts = Opts { optCommand :: !Command }

data Command = List String String Bool
