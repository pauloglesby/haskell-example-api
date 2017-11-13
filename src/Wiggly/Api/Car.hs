{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Wiggly.Api.Car
       (
         CarAPI
       ) where

import Servant

import Wiggly.Data.Car

type CarAPI = "cars" :> Get '[JSON] [Car]
                  :<|> "cars" :> ReqBody '[JSON] CreateCar :> PostCreated '[JSON] Car
                  :<|> "cars" :> Capture "id" Int :> Get '[JSON] Car
                  :<|> "cars" :> Capture "id" Int :> ReqBody '[JSON] UpdateCar :> PutNoContent '[JSON] NoContent
                  :<|> "cars" :> "search" :> ReqBody '[JSON] CarFilter :> Post '[JSON] [Car]
