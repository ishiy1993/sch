module Main where

import Network.Google.OAuth2
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
    as <- getArgs
    case as of
        ["ls"] -> do
            f <- getToday
            t <- getNext 7
            getEvents token f t
        _ -> return ()
