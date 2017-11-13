{-# LANGUAGE FlexibleContexts #-}

module Wiggly.Db.Util (
  printSql
  , showSql
  ) where

import Data.Maybe (fromMaybe)
import qualified Opaleye as DB
import Data.Profunctor.Product.Default (Default)

showSql :: Default DB.Unpackspec a a => DB.Query a -> String
showSql q = fromMaybe "No SQL" (DB.showSql q)

printSql :: Default DB.Unpackspec a a => DB.Query a -> IO ()
printSql = putStrLn . showSql
