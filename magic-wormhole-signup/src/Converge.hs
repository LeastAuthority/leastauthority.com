{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Converge (
  showSubscriptions
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Client

import Network.HTTP.Client (
  newManager
  , defaultManagerSettings
  )

import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- The type of a response to a GET /v1/subscriptions
data ListSubscriptionsResponse = ListSubscriptionsResponse
  { subscriptions :: [Subscription]
  } deriving (Eq, Show)

-- Rules for deserializating JSON to ListSubscriptionsResponse
instance FromJSON ListSubscriptionsResponse where
  parseJSON (Object o) =
    ListSubscriptionsResponse <$> o.: "subscriptions"

-- The type definition for the Subscription Manager API
type SubscriptionManagerAPI =
  "v1" :> "subscriptions" :> Get '[JSON] ListSubscriptionsResponse

-- Just a string for identifying a particular subscription uniquely.
type SubscriptionID = String

-- The customer email address associated with a subscription.
type Email = String

-- The type of a single subscription.  We're skipping a bunch of fields
-- because we don't need/want/care about them.
data Subscription = Subscription
  { subscriptionId :: SubscriptionID
  , subscriptionEmail :: Email
  } deriving (Eq, Show)

-- Deserialization from JSON to a Subscription.
instance FromJSON Subscription where
  parseJSON (Object o) =
    Subscription <$> o .: "subscription_id"
                 <*> o .: "customer_email"

-- Some Servant magic to help generate the client API.
subscriptionManagerAPI :: Proxy SubscriptionManagerAPI
subscriptionManagerAPI = Proxy

-- Generate the Servant client API from the type definitions.
listSubscriptions = client subscriptionManagerAPI

-- Simple driver program to aid with development.
showSubscriptions :: IO ()
showSubscriptions = do
  manager <- newManager defaultManagerSettings
  let env = ClientEnv manager (BaseUrl Http "localhost" 8000 "")
  res <- runClientM showSubscriptions' env
  putStrLn $ show res
  return ()


-- Use the generated Servant client API to list subscriptions on the manager
-- and extract the actual subscription list from the slightly larger response
-- structure.
showSubscriptions' :: ClientM [Subscription]
showSubscriptions' = do
  subscriptions' <- listSubscriptions
  return $ subscriptions subscriptions'
