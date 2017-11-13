{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Wiggly.Data.Application
       (
         Config(..)
       , AppM
       , ErrorResponse(..)
       ) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Servant (ServantErr)

import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as PGS

import Wiggly.Util.Json (recordNameToJsonName)

data Config = Config {
  environmentName :: String
  , startTime :: Time.UTCTime
  , dbConnectInfo :: PGS.ConnectInfo
  }

type AppM = ReaderT Config (ExceptT ServantErr IO)

data ErrorResponse = ErrorResponse { _errorresponseMessage :: T.Text
                                   } deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''ErrorResponse)
