{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Wiggly.Data.Account
       (
       AccountT(..)
       , Account
       , AccountTableRead
       , AccountTableWrite
       , pAccount

       , CreateAccount(..)
       , createAccountData

       , UpdateAccount(..)
       , updateAccountData

       , AccountFilter(..)
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

data AccountT id name email post_code created_at updated_at = Account {
  _accountId :: id
  , _accountName :: name
  , _accountEmail :: email
  , _accountPostCode :: post_code
  , _accountCreatedAt :: created_at
  , _accountUpdatedAt :: updated_at
  } deriving (Show)

$(makeAdaptorAndInstance "pAccount" ''AccountT)

type Account = AccountT Int T.Text T.Text T.Text Time.UTCTime Time.UTCTime

type AccountTableWrite = AccountT
     (Maybe (Column PGInt4))
     (Column PGText)
     (Column PGText)
     (Column PGText)
     (Maybe (Column PGTimestamptz))
     (Maybe (Column PGTimestamptz))

type AccountTableRead = AccountT
     (Column PGInt4)
     (Column PGText)
     (Column PGText)
     (Column PGText)
     (Column PGTimestamptz)
     (Column PGTimestamptz)

$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''AccountT)

--makeFields ''AccountT -- TODO: lens exports

data CreateAccount = CreateAccount { _createaccountName :: T.Text
                                   , _createaccountEmail :: T.Text
                                   , _createaccountPostCode :: T.Text -- TODO: new type for Post Codes
                                   } deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''CreateAccount)

data UpdateAccount = UpdateAccount { _updateaccountName :: Maybe T.Text
                                   , _updateaccountEmail :: Maybe T.Text
                                   , _updateaccountPostCode :: Maybe T.Text
                                   } deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''UpdateAccount)

createAccountData :: CreateAccount -> AccountTableWrite
createAccountData d = Account {
  _accountId = Nothing
  , _accountName = pgStrictText $ _createaccountName d
  , _accountEmail = pgStrictText $ _createaccountEmail d
  , _accountPostCode = pgStrictText $ _createaccountPostCode d
  , _accountCreatedAt = Nothing
  , _accountUpdatedAt = Nothing
  }

updateAccountData :: UpdateAccount -> AccountTableRead -> AccountTableWrite
updateAccountData d old = Account {
  _accountId = Just $ _accountId old
  , _accountName = maybe (_accountName old) pgStrictText (_updateaccountName d)
  , _accountEmail = maybe (_accountEmail old) pgStrictText (_updateaccountEmail d)
  , _accountPostCode = maybe (_accountPostCode old) pgStrictText (_updateaccountPostCode d)
  , _accountCreatedAt = Just $ _accountCreatedAt old
  , _accountUpdatedAt = Nothing
  }

data AccountFilter = AccountFilter {
  _accountfilterName :: Maybe T.Text
  , _accountfilterEmail :: Maybe T.Text
  , _accountfilterPostCode :: Maybe T.Text
  } deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''AccountFilter)
