{-# LANGUAGE DataKinds #-}

module Wiggly.Server.System
       (
         systemServer
       ) where

import Control.Monad.Reader
import Servant
import Data.Time

import Wiggly.Api.System
import Wiggly.Data.Application
import Wiggly.Data.Pong

ping :: AppM (Headers '[Header "X-Api-Version" String, Header "X-Api-Uptime" String] Pong)
ping = do
  start <- asks startTime
  t <- liftIO getCurrentTime
  let diff = diffUTCTime t start
      strT = show diff
  return $ addHeader "0.0.1" $ addHeader strT $ Pong { _pongUptime = truncate (toRational diff) }

systemServer :: ServerT SystemAPI AppM
systemServer = ping
