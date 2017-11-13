{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wiggly.Server.Account
       (
         accountServer
       ) where

import Prelude()
import Prelude.Compat hiding (log)

import Control.Arrow (returnA, (<<<), (>>>))
import Control.Exception.Safe as X
import Control.Lens
import Control.Monad.Reader
import Data.Either (either)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Profunctor.Product.Default (Default)
import Servant
import qualified Data.Aeson as J
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Simple as PGS
import qualified Opaleye as DB

import Wiggly.Api.Account
import Wiggly.Data.Account
import Wiggly.Data.Application
import Wiggly.Db.Util(showSql)

makeFields ''AccountT -- TODO: lens import

log :: String -> AppM ()
log s = liftIO $ putStrLn s

logQuery :: Default DB.Unpackspec a a => DB.Query a -> AppM ()
logQuery q = log $ showSql q

accounts :: AppM [Account]
accounts = do
  log "accounts"
  connInfo <- asks dbConnectInfo
  conn <- liftIO $ PGS.connect connInfo
  result <- liftIO $ X.try $ runAccountQuery conn accountQuery
  either ioErrorsToServantErrors return result

createAccount :: CreateAccount -> AppM Account
createAccount ca = do
  log "createAccount"
  connInfo <- asks dbConnectInfo
  conn <- liftIO $ PGS.connect connInfo
  result <- liftIO $ X.try $ DB.runInsertManyReturning conn accountTable [(createAccountData ca)] id
  either ioErrorsToServantErrors createdOrServerError result
  where createdOrServerError :: [Account] -> AppM Account
        createdOrServerError xs = do
          let body = ErrorResponse "could not create account"
          if null xs
            then throwError err500 { errBody = (J.encode body) }
            else return (head xs)

showAccount :: Int -> AppM Account
showAccount objectId = do
  log "showAccount"
  connInfo <- asks dbConnectInfo
  conn <- liftIO $ PGS.connect connInfo
  result <- liftIO $ X.try $ runAccountQuery conn ((restrictId objectId) <<< accountQuery)
  either ioErrorsToServantErrors (retrievedOrNotFound objectId) result

retrievedOrNotFound :: Int -> [Account] -> AppM Account
retrievedOrNotFound objectId xs = do
  let body = ErrorResponse $ T.concat [(T.pack "could not find record with id: "), T.pack (show objectId)]
  if null xs
    then throwError err404 { errBody = (J.encode body) }
    else return (head xs)

-- TODO: this should return 404 if the user does not exist
updateAccount :: Int -> UpdateAccount -> AppM NoContent
updateAccount objectId updates = do
  log "updateAccount"
  let sql = DB.arrangeUpdateSql accountTable updater (updateThese objectId)
  log $ "SQL: " ++ sql
  connInfo <- asks dbConnectInfo
  conn <- liftIO $ PGS.connect connInfo
  result <- liftIO $ X.try $ DB.runUpdate conn accountTable updater (updateThese objectId)
  either ioErrorsToServantErrors (updatedOrNotFound objectId) result
  where updater = updateAccountData updates
        updateThese n = (\row -> (_accountId row) DB..== (DB.pgInt4 n))

updatedOrNotFound :: Int -> Int64 -> AppM NoContent
updatedOrNotFound objectId updatedRowsCount = do
  let body = ErrorResponse $ T.concat [(T.pack "could not find record with id: "), T.pack (show objectId)]
  if updatedRowsCount == 0
    then throwError err404 { errBody = (J.encode body) }
    else return NoContent

filterAccounts :: AccountFilter -> AppM [Account]
filterAccounts filterSpec = do
  log "filterAccounts"
  log $ "filters: " ++ (show filterSpec)
  connInfo <- asks dbConnectInfo
  conn <- liftIO $ PGS.connect connInfo
  let query = filterAccountQuery filterSpec
  logQuery query
  result <- liftIO $ X.try $ runAccountQuery conn query
  either ioErrorsToServantErrors return result

accountServer :: ServerT AccountAPI AppM
accountServer = accounts :<|> createAccount :<|> showAccount :<|> updateAccount :<|> filterAccounts

accountTable :: DB.Table AccountTableWrite AccountTableRead
accountTable = DB.Table "accounts" (pAccount
                                 Account {
                                    _accountId = (DB.optional "id")
                                    , _accountName = (DB.required "name")
                                    , _accountEmail = (DB.required "email")
                                    , _accountPostCode = (DB.required "post_code")
                                    , _accountCreatedAt = (DB.optional "created_at")
                                    , _accountUpdatedAt = (DB.optional "updated_at")
                                    }
                                )

accountQuery :: DB.Query AccountTableRead
accountQuery = DB.queryTable accountTable

runAccountQuery :: PGS.Connection
                  -> DB.Query AccountTableRead
                  -> IO [Account]
runAccountQuery = DB.runQuery

restrictId :: Int -> DB.QueryArr AccountTableRead AccountTableRead
restrictId s = proc row -> do
  DB.restrict -< (_accountId row) DB..== (DB.pgInt4 s)
  returnA -< row

restrictName :: T.Text -> DB.QueryArr AccountTableRead AccountTableRead
restrictName s = proc row -> do
  DB.restrict -< (_accountName row) DB..== (DB.pgStrictText s)
  returnA -< row

restrictEmail :: T.Text -> DB.QueryArr AccountTableRead AccountTableRead
restrictEmail s = proc row -> do
  DB.restrict -< (_accountEmail row) DB..== (DB.pgStrictText s)
  returnA -< row

restrictPostCode :: T.Text -> DB.QueryArr AccountTableRead AccountTableRead
restrictPostCode s = proc row -> do
  DB.restrict -< (_accountPostCode row) DB..== (DB.pgStrictText s)
  returnA -< row

filterToQueries :: AccountFilter -> [DB.QueryArr AccountTableRead AccountTableRead]
filterToQueries f = let nf = fmap restrictName (_accountfilterName f)
                        ef = fmap restrictEmail (_accountfilterEmail f)
                        pf = fmap restrictPostCode (_accountfilterPostCode f)
                    in catMaybes [nf, ef, pf]

filterAccountQuery :: AccountFilter -> DB.Query AccountTableRead
filterAccountQuery f = (combineQueries $ filterToQueries f) <<< accountQuery

combineQueries :: [DB.QueryArr c c] -> DB.QueryArr c c
combineQueries xs = foldr go returnA xs
  where go a b = a >>> b

-- TODO: can we generalise this to all excpetion types?
ioErrorsToServantErrors :: PGS.SqlError -> AppM a
ioErrorsToServantErrors e = do
  log "converting SQL error to ServantErr"
  throwError err500 { errBody = (J.encode $ sqlErrorToJson e) }

sqlErrorToJson :: PGS.SqlError -> ErrorResponse
sqlErrorToJson e = let builder = B.byteString "SQL Error - "
                       message = mconcat [builder, (B.byteString $ PGS.sqlErrorMsg e), (B.byteString " - detail: "), (B.byteString $ PGS.sqlErrorDetail e)]
                   in ErrorResponse $ TE.decodeUtf8 $ LBS.toStrict $ (B.toLazyByteString message)
