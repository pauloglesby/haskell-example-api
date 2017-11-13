module Main where

import Network.Wai.Handler.Warp (run)
import Data.Time.Clock (getCurrentTime)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)

import Wiggly.Server (app)
import Wiggly.Db.Connection (locateDbConnectionInfo)
import Wiggly.Data.Application (Config(..))

configuration :: IO Config
configuration = do
  t <- getCurrentTime
  dbInfo <- locateDbConnectionInfo
  return $ Config {
    environmentName = "development"
    , startTime = t
    , dbConnectInfo = dbInfo
    }

main :: IO ()
main = do
  args <- getArgs
  config <- configuration
  let port = maybe 8080 (\x -> read x :: Int) (listToMaybe args)
  putStrLn $ "Running server on localhost:" ++ (show port)
  run port (app config)
