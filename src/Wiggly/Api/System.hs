{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wiggly.Api.System
       (
         SystemAPI
       ) where

import Servant

import Wiggly.Data.Pong

type SystemAPI = "ping" :> Get '[JSON] (Headers '[Header "X-Api-Version" String, Header "X-Api-Uptime" String] Pong)
