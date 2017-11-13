{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wiggly.Api.Application
       (
         ApplicationAPI
       , SystemAPI
       , DocAPI
       , AccountAPI
       , CarAPI
       ) where

import Servant

import Wiggly.Api.System
import Wiggly.Api.Doc
import Wiggly.Api.Account
import Wiggly.Api.Car

type ApplicationAPI = SystemAPI :<|> DocAPI :<|> AccountAPI :<|> CarAPI
