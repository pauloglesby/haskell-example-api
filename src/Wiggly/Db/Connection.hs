module Wiggly.Db.Connection
       (
         locateDbConnectionInfo
         , herokuDbConnectionInfo
         , staticDbConnectionInfo
       ) where

import qualified Database.PostgreSQL.Simple as PGS
import System.Environment (lookupEnv)
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)

locateDbConnectionInfo :: IO PGS.ConnectInfo
locateDbConnectionInfo = do
  heroku <- herokuDbConnectionInfo
  return $ maybe staticDbConnectionInfo id heroku

herokuDbConnectionInfo :: IO (Maybe PGS.ConnectInfo)
herokuDbConnectionInfo = do
  uri <- lookupEnv "DATABASE_URL"
  return $ uri >>= parseDatabaseUrl

staticDbConnectionInfo :: PGS.ConnectInfo
staticDbConnectionInfo = let str = "haskell"
                         in PGS.defaultConnectInfo {
  PGS.connectUser = str
  , PGS.connectPassword = str
  , PGS.connectDatabase = str
  }
