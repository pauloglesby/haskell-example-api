{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Wiggly.Data.Car
       (
       CarT(..)
       , Car
       , CarTableRead
       , CarTableWrite
       , pCar

       , CreateCar(..)
       , createCarData

       , UpdateCar(..)
       , updateCarData

       , CarFilter(..)
       ) where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Aeson.TH
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import GHC.Generics

import qualified Data.Text as T
import qualified Data.Time as Time

import Opaleye (Column, Nullable, matchNullable, isNull,
                Table(Table), required, optional, queryTable,
                Query, QueryArr, restrict, keepWhen, (.==), (.<=), (.>=), (.&&), (.<),
                (.===),
                (.++), ifThenElse, ors, pgString, aggregate, groupBy,
                count, avg, sum, leftJoin, runQuery,
                showSql, Unpackspec,
                PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool, PGTimestamptz, PGTimestamp,
                pgInt4, pgInt8, pgString, pgStrictText)

import Wiggly.Util.Json (recordNameToJsonName)

data CarT id owner_id make model colour fuel range  created_at updated_at = Car {
  _carId :: id
  , _carOwnerId :: owner_id
  , _carMake :: make
  , _carModel :: model
  , _carColour :: colour
  , _carFuel :: fuel
  , _carRange :: range
  , _carCreatedAt :: created_at
  , _carUpdatedAt :: updated_at
  } deriving (Show)

$(makeAdaptorAndInstance "pCar" ''CarT)

type Car = CarT Int Int T.Text T.Text T.Text T.Text Int Time.UTCTime Time.UTCTime

type CarTableWrite = CarT
     (Maybe (Column PGInt4))
     (Column PGInt4)
     (Column PGText)
     (Column PGText)
     (Column PGText)
     (Column PGText)
     (Column PGInt4)
     (Maybe (Column PGTimestamptz))
     (Maybe (Column PGTimestamptz))

type CarTableRead = CarT
     (Column PGInt4)
     (Column PGInt4)
     (Column PGText)
     (Column PGText)
     (Column PGText)
     (Column PGText)
     (Column PGInt4)
     (Column PGTimestamptz)
     (Column PGTimestamptz)

$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''CarT)

--makeFields ''CarT -- TODO: lens exports

data CreateCar = CreateCar { _createcarOwnerId :: Int
                           , _createcarMake :: T.Text
                           , _createcarModel :: T.Text
                           , _createcarColour :: T.Text
                           , _createcarFuel :: T.Text
                           , _createcarRange :: Int
                           } deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''CreateCar)

data UpdateCar = UpdateCar { _updatecarOwnerId :: Maybe Int
                           , _updatecarColour :: Maybe T.Text
                           , _updatecarFuel :: Maybe T.Text
                           , _updatecarRange :: Maybe Int
                           } deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''UpdateCar)

createCarData :: CreateCar -> CarTableWrite
createCarData d = Car {
  _carId = Nothing
  , _carOwnerId = pgInt4 $ _createcarOwnerId d
  , _carMake = pgStrictText $ _createcarMake d
  , _carModel = pgStrictText $ _createcarModel d
  , _carColour = pgStrictText $ _createcarColour d
  , _carFuel = pgStrictText $ _createcarFuel d
  , _carRange = pgInt4 $ _createcarRange d
  , _carCreatedAt = Nothing
  , _carUpdatedAt = Nothing
  }

updateCarData :: UpdateCar -> CarTableRead -> CarTableWrite
updateCarData d old = Car {
  _carId = Just $ _carId old
  , _carOwnerId = maybe (_carOwnerId old) pgInt4 $ _updatecarOwnerId d
  , _carMake = _carMake old
  , _carModel = _carModel old
  , _carColour = maybe (_carColour old) pgStrictText $ _updatecarColour d
  , _carFuel = maybe (_carFuel old) pgStrictText $ _updatecarFuel d
  , _carRange = maybe (_carRange old) pgInt4 $ _updatecarRange d
  , _carCreatedAt = Just $ _carCreatedAt old
  , _carUpdatedAt = Nothing
  }

data CarFilter = CarFilter {
  _carfilterMakes :: Maybe [T.Text]
  , _carfilterModels :: Maybe [T.Text]
  , _carfilterColours :: Maybe [T.Text]
  , _carfilterFuels :: Maybe [T.Text]
  , _carfilterMinRange :: Maybe Int
  , _carfilterMaxRange :: Maybe Int
  } deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''CarFilter)
