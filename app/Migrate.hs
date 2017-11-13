module Main where

import Data.Time.Clock (getCurrentTime)
import System.Environment (getArgs)
import Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.Migration as M

import Wiggly.Data.Application (Config(..))
import Wiggly.Db.Connection (locateDbConnectionInfo)

configuration :: IO Config
configuration = do
  t <- getCurrentTime
  dbInfo <- locateDbConnectionInfo
  return $ Config {
    environmentName = "development"
    , startTime = t
    , dbConnectInfo = dbInfo
    }

initDb :: Config -> IO ()
initDb config = do
  conn <- PGS.connect (dbConnectInfo config)
  putStrLn "initialising database"
  result <- PGS.withTransaction conn $ M.runMigration $
    MigrationContext MigrationInitialization True conn
  putStrLn $ show result

main :: IO ()
main = do
  args <- getArgs
  config <- configuration
  conn <- PGS.connect (dbConnectInfo config)
  let dir = "db/migrations"
  putStrLn "migrating database"
  initDb config
  result <- PGS.withTransaction conn $ M.runMigration $
    MigrationContext (MigrationDirectory dir) True conn
  putStrLn $ show result
