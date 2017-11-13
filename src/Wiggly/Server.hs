{-# LANGUAGE TypeOperators #-}

module Wiggly.Server
       (
         serverAPI
       , server
       , app
       ) where

import Control.Monad.Except
import Control.Monad.Reader
import Servant

import Wiggly.Api.Application
import Wiggly.Data.Application
import Wiggly.Server.Account
import Wiggly.Server.Car
import Wiggly.Server.Doc
import Wiggly.Server.System

-- TODO: rename
readerToExcept :: Config -> AppM :~> ExceptT ServantErr IO
readerToExcept cfg = Nat $ \x -> runReaderT x cfg

combinedServer :: ServerT ApplicationAPI AppM
combinedServer =
  systemServer
  :<|> docServer
  :<|> accountServer
  :<|> carServer

server :: Config -> Server ApplicationAPI
server config = enter (readerToExcept config) combinedServer

serverAPI :: Proxy ApplicationAPI
serverAPI = Proxy

app :: Config -> Application
app config = serve serverAPI (server config)
