{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wiggly.Api.Doc
       (
         DocAPI
       ) where

import Servant
import qualified Data.Text as T

import Wiggly.Data.Markdown

type DocAPI = "doc" :> Get '[Markdown] T.Text
