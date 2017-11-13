{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Wiggly.Data.Pong
       (
         Pong(..)
       ) where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics

import Wiggly.Util.Json (recordNameToJsonName)

data Pong = Pong { _pongUptime :: Integer
                 } deriving (Eq,Show,Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''Pong)

--makeFields ''Pong -- TODO: lens exports
