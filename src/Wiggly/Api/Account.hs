{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wiggly.Api.Account
       (
         AccountAPI
       ) where

import Servant

import Wiggly.Data.Account

type AccountAPI = "accounts" :> Get '[JSON] [Account]
                  :<|> "accounts" :> ReqBody '[JSON] CreateAccount :> PostCreated '[JSON] Account
                  :<|> "accounts" :> Capture "id" Int :> Get '[JSON] Account
                  :<|> "accounts" :> Capture "id" Int :> ReqBody '[JSON] UpdateAccount :> PutNoContent '[JSON] NoContent
                  :<|> "accounts" :> "search" :> ReqBody '[JSON] AccountFilter :> Post '[JSON] [Account]
