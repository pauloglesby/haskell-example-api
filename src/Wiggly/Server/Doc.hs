{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Wiggly.Server.Doc
       (
         docServer
       ) where

import Servant
import Servant.Docs
import qualified Data.Time as Time
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Wiggly.Api.Application
import Wiggly.Data.Account
import Wiggly.Data.Car
import Wiggly.Data.Pong
import Wiggly.Data.Application

sampleDate :: Time.UTCTime
sampleDate = let d = Time.fromGregorian 2016 12 21
             in Time.UTCTime d 0

instance ToSample Char where
  toSamples _ = singleSample 'x'

instance ToSample Pong where
  toSamples _ = singleSample p
    where p = Pong 23

instance ToSample Account where
  toSamples _ = singleSample r
    where r = Account 42 "Wiggly Worm" "wiggly@example.com" "NW1 4SS" sampleDate sampleDate

instance ToSample CreateAccount where
  toSamples _ = singleSample r
    where r = CreateAccount "Wiggly Worm" "wiggly@example.com" "NW1 4SS"

instance ToSample UpdateAccount where
  toSamples _ = singleSample r
    where r = UpdateAccount (Just "Ralph Wiggum") Nothing (Just "SE19 7DR")

instance ToSample AccountFilter where
  toSamples _ = singleSample f
    where f = AccountFilter (Just "william") Nothing Nothing

instance ToSample Car where
  toSamples _ = singleSample r
    where r = Car 42 23 "Honda" "Civic" "Yellow" "Petrol" 230 sampleDate sampleDate

instance ToSample CreateCar where
  toSamples _ = singleSample r
    where r = CreateCar 23 "Honda" "Civic" "Yellow" "Petrol" 230

instance ToSample UpdateCar where
  toSamples _ = singleSample r
    where r = UpdateCar Nothing (Just "Red") Nothing Nothing

instance ToSample CarFilter where
  toSamples _ = singleSample f
    where f = CarFilter (Just ["Jaguar", "Toyota"]) Nothing (Just ["Orange"]) Nothing Nothing Nothing

instance ToCapture (Capture "id" Int) where
  toCapture _ = DocCapture "id" "ID of resource"

instance ToCapture (Capture "uuid" Int) where
  toCapture _ = DocCapture "uuid" "UUID of resource"

instance ToSample () where
  toSamples _ = singleSample r
    where r = ()

instance ToSample T.Text where
  toSamples _ = singleSample r
    where r = "some text"

instance ToSample TL.Text where
  toSamples _ = singleSample r
    where r = "some lazy text"

serverAPI :: Proxy ApplicationAPI
serverAPI = Proxy

serverDocs :: API
serverDocs = docs serverAPI

doc :: AppM T.Text
doc = do
  let m = T.pack $ markdown serverDocs
  return m

docServer :: ServerT DocAPI AppM
docServer = doc
