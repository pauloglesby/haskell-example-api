{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wiggly.Server.Car
       (
         carServer
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

import Wiggly.Api.Car
import Wiggly.Data.Car
import Wiggly.Data.Application
import Wiggly.Db.Util(showSql)

makeFields ''CarT -- TODO: lens import

-- TODO: factor out into util
log :: String -> AppM ()
log s = liftIO $ putStrLn s

-- TODO: factor out into util
logQuery :: Default DB.Unpackspec a a => DB.Query a -> AppM ()
logQuery q = log $ showSql q

cars :: AppM [Car]
cars = do
  log "cars"
  connInfo <- asks dbConnectInfo
  conn <- liftIO $ PGS.connect connInfo
  result <- liftIO $ X.try $ runCarQuery conn carQuery
  either ioErrorsToServantErrors return result

createCar :: CreateCar -> AppM Car
createCar ca = do
  log "createCar"
  connInfo <- asks dbConnectInfo
  conn <- liftIO $ PGS.connect connInfo
  result <- liftIO $ X.try $ DB.runInsertManyReturning conn carTable [(createCarData ca)] id
  either ioErrorsToServantErrors createdOrServerError result
  where createdOrServerError :: [Car] -> AppM Car
        createdOrServerError xs = do
          let body = ErrorResponse "could not create car"
          if null xs
            then throwError err500 { errBody = (J.encode body) }
            else return (head xs)

showCar :: Int -> AppM Car
showCar objectId = do
  log "showCar"
  connInfo <- asks dbConnectInfo
  conn <- liftIO $ PGS.connect connInfo
  result <- liftIO $ X.try $ runCarQuery conn ((restrictId objectId) <<< carQuery)
  either ioErrorsToServantErrors (retrievedOrNotFound objectId) result

retrievedOrNotFound :: Int -> [Car] -> AppM Car
retrievedOrNotFound objectId xs = do
  let body = ErrorResponse $ T.concat [(T.pack "could not find record with id: "), T.pack (show objectId)]
  if null xs
    then throwError err404 { errBody = (J.encode body) }
    else return (head xs)

-- TODO: this should return 404 if the user does not exist
updateCar :: Int -> UpdateCar -> AppM NoContent
updateCar objectId updates = do
  log "updateCar"
  let sql = DB.arrangeUpdateSql carTable updater (updateThese objectId)
  log $ "SQL: " ++ sql
  connInfo <- asks dbConnectInfo
  conn <- liftIO $ PGS.connect connInfo
  result <- liftIO $ X.try $ DB.runUpdate conn carTable updater (updateThese objectId)
  either ioErrorsToServantErrors (updatedOrNotFound objectId) result
  where updater = updateCarData updates
        updateThese n = (\row -> (_carId row) DB..== (DB.pgInt4 n))

updatedOrNotFound :: Int -> Int64 -> AppM NoContent
updatedOrNotFound objectId updatedRowsCount = do
  let body = ErrorResponse $ T.concat [(T.pack "could not find record with id: "), T.pack (show objectId)]
  if updatedRowsCount == 0
    then throwError err404 { errBody = (J.encode body) }
    else return NoContent

filterCars :: CarFilter -> AppM [Car]
filterCars filterSpec = do
  log "filterCars"
  log $ "filters: " ++ (show filterSpec)
  connInfo <- asks dbConnectInfo
  conn <- liftIO $ PGS.connect connInfo
  let query = filterCarQuery filterSpec
  logQuery query
  result <- liftIO $ X.try $ runCarQuery conn query
  either ioErrorsToServantErrors return result

carServer :: ServerT CarAPI AppM
carServer = cars :<|> createCar :<|> showCar :<|> updateCar :<|> filterCars

carTable :: DB.Table CarTableWrite CarTableRead
carTable = DB.Table "cars" (pCar
                            Car {
                               _carId = (DB.optional "id")
                               , _carOwnerId = (DB.required "owner_id")
                               , _carMake = (DB.required "make")
                               , _carModel = (DB.required "model")
                               , _carColour = (DB.required "colour")
                               , _carFuel = (DB.required "fuel")
                               , _carRange = (DB.required "range")
                               , _carCreatedAt = (DB.optional "created_at")
                               , _carUpdatedAt = (DB.optional "updated_at")
                               }
                           )

carQuery :: DB.Query CarTableRead
carQuery = DB.queryTable carTable

runCarQuery :: PGS.Connection
                  -> DB.Query CarTableRead
                  -> IO [Car]
runCarQuery = DB.runQuery

restrictId :: Int -> DB.QueryArr CarTableRead CarTableRead
restrictId s = proc row -> do
  DB.restrict -< (_carId row) DB..== (DB.pgInt4 s)
  returnA -< row

restrictMake :: T.Text -> DB.QueryArr CarTableRead CarTableRead
restrictMake s = proc row -> do
  DB.restrict -< (_carMake row) DB..== (DB.pgStrictText s)
  returnA -< row

restrictModel :: T.Text -> DB.QueryArr CarTableRead CarTableRead
restrictModel s = proc row -> do
  DB.restrict -< (_carModel row) DB..== (DB.pgStrictText s)
  returnA -< row

restrictColour :: T.Text -> DB.QueryArr CarTableRead CarTableRead
restrictColour s = proc row -> do
  DB.restrict -< (_carColour row) DB..== (DB.pgStrictText s)
  returnA -< row

restrictFuel :: T.Text -> DB.QueryArr CarTableRead CarTableRead
restrictFuel s = proc row -> do
  DB.restrict -< (_carFuel row) DB..== (DB.pgStrictText s)
  returnA -< row

restrictMinRange :: Int -> DB.QueryArr CarTableRead CarTableRead
restrictMinRange s = proc row -> do
  DB.restrict -< (_carRange row) DB..>= (DB.pgInt4 s)
  returnA -< row

restrictMaxRange :: Int -> DB.QueryArr CarTableRead CarTableRead
restrictMaxRange s = proc row -> do
  DB.restrict -< (_carRange row) DB..<= (DB.pgInt4 s)
  returnA -< row

restrictMakes :: [T.Text] -> DB.QueryArr CarTableRead CarTableRead
restrictMakes s = proc row -> do
  let restrictions = fmap (\value -> (_carMake row) DB..== (DB.pgStrictText value)) s
  DB.restrict -< DB.ors restrictions
  returnA -< row

restrictModels :: [T.Text] -> DB.QueryArr CarTableRead CarTableRead
restrictModels s = proc row -> do
  let restrictions = fmap (\value -> (_carModel row) DB..== (DB.pgStrictText value)) s
  DB.restrict -< DB.ors restrictions
  returnA -< row

restrictColours :: [T.Text] -> DB.QueryArr CarTableRead CarTableRead
restrictColours s = proc row -> do
  let restrictions = fmap (\value -> (_carColour row) DB..== (DB.pgStrictText value)) s
  DB.restrict -< DB.ors restrictions
  returnA -< row

restrictFuels :: [T.Text] -> DB.QueryArr CarTableRead CarTableRead
restrictFuels s = proc row -> do
  let restrictions = fmap (\value -> (_carFuel row) DB..== (DB.pgStrictText value)) s
  DB.restrict -< DB.ors restrictions
  returnA -< row

filterToQueries :: CarFilter -> [DB.QueryArr CarTableRead CarTableRead]
filterToQueries f = catMaybes [
  fmap restrictMakes (_carfilterMakes f)
  , fmap restrictModels (_carfilterModels f)
  , fmap restrictColours (_carfilterColours f)
  , fmap restrictFuels (_carfilterFuels f)
  , fmap restrictMinRange (_carfilterMinRange f)
  , fmap restrictMaxRange (_carfilterMaxRange f)
  ]

filterCarQuery :: CarFilter -> DB.Query CarTableRead
filterCarQuery f = (combineQueries $ filterToQueries f) <<< carQuery

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
