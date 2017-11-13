{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wiggly.Api.Application
       (
         ApplicationAPI
       , SystemAPI
       , DocAPI
       , AccountAPI
       ) where

import Servant

import Wiggly.Api.System
import Wiggly.Api.Doc
import Wiggly.Api.Account

type ApplicationAPI = SystemAPI :<|> DocAPI :<|> AccountAPI
