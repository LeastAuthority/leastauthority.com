{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( listPlans
  , createPlan
  , createPlans
  ) where

import Prelude hiding
  ( id
  )

import Control.Monad.Except
  ( catchError
  , throwError
  )
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base64 (encode)
import GHC.Generics (Generic)
import Data.Aeson hiding (encode)
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty (encodePretty)
import Network.Wai
import Network.Wai.Handler.Warp
import Web.FormUrlEncoded (ToForm)
import Servant
import Network.HTTP.Types
  ( Status(Status)
  , badRequest400
  )

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
  ( BaseUrl(BaseUrl)
  , ClientM
  , Scheme(Https)
  , ServantError(FailureResponse)
  , GenResponse(Response)
  , mkClientEnv
  , runClientM
  , client
  )

-- data Metadata = Map String String deriving (Eq, Show)

data Plan = Plan
  { id        :: String
  , name      :: String
  , invoice_name :: Maybe String
  , description :: Maybe String
  , trial_period :: Maybe Int
  , trial_period_unit :: Maybe String
  , period :: Maybe Int
  , period_unit :: Maybe String
  , setup_cost :: Maybe Int
  , price :: Maybe Integer
  , currency_code :: String
  , billing_cycles :: Maybe Int
  , charge_model :: Maybe String
  , free_quantity :: Maybe Int
  , redirect_url :: Maybe String
  , enabled_in_hosted_pages :: Maybe Bool
  , enabled_in_portal :: Maybe Bool
  , taxable :: Maybe Bool
  , tax_profile_id :: Maybe String
  , tax_code :: Maybe String
  , sku :: Maybe String
  , accounting_code :: Maybe String
  , accounting_category1 :: Maybe String
  , accounting_category2 :: Maybe String
  , invoice_notes :: Maybe String
  -- , metadata :: Maybe Metadata
  , status :: Maybe String
  } deriving (Generic, Eq, Show)

no_value :: a
no_value = error "no plan value supplied"

minimalPlan = Plan
  { id = no_value
  , name = no_value
  , price = no_value
  , currency_code = no_value
  , invoice_name = Nothing
  , description = Nothing
  , trial_period = Nothing
  , trial_period_unit = Nothing
  , period = Nothing
  , period_unit = Nothing
  , setup_cost = Nothing
  , billing_cycles = Nothing
  , charge_model = Nothing
  , free_quantity = Nothing
  , redirect_url = Nothing
  , enabled_in_hosted_pages = Nothing
  , enabled_in_portal = Nothing
  , taxable = Nothing
  , tax_profile_id = Nothing
  , tax_code = Nothing
  , sku = Nothing
  , accounting_code = Nothing
  , accounting_category1 = Nothing
  , accounting_category2 = Nothing
  , invoice_notes = Nothing
  -- , metadata = Nothing
  , status = Nothing
  }

$(deriveJSON defaultOptions ''Plan)

instance ToForm Plan

data WrappedPlan = WrappedPlan
  { plan :: Plan
  } deriving (Generic, Eq, Show)

$(deriveJSON defaultOptions ''WrappedPlan)

data ListResponse a = ListResponse
  { list :: [a]
  } deriving (Generic, Eq, Show)

$(deriveJSON defaultOptions ''ListResponse)

type API =
  Header "Authorization" String :> "api" :> "v2" :> "plans" :> Get '[JSON] (ListResponse WrappedPlan)
  :<|> Header "Authorization" String :> "api" :> "v2" :> "plans" :> ReqBody '[FormUrlEncoded] Plan :> Post '[JSON] WrappedPlan
  :<|> Header "Authorization" String :> "api" :> "v2" :> "plans" :> Capture "id" String :> ReqBody '[FormUrlEncoded] Plan :> Post '[JSON] WrappedPlan

api :: Proxy API
api = Proxy

listPlans ::  Maybe String -> ClientM (ListResponse WrappedPlan)
createPlan :: Maybe String -> Plan -> ClientM WrappedPlan
updatePlan :: Maybe String -> String -> Plan -> ClientM WrappedPlan
listPlans :<|> createPlan :<|> updatePlan = client api

createPlans :: String -> String -> IO ()
createPlans site apikey = do
  manager <- newManager tlsManagerSettings
  let env = mkClientEnv manager (BaseUrl Https (site ++ ".chargebee.com") 443 "")
  res <- runClientM (createPlans' apikey s4Plans) env
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right plans -> putStrLn (toString (encodePretty plans))


updateInsteadOfCreate :: String -> Plan -> ServantError -> ClientM WrappedPlan
updateInsteadOfCreate basicAuth plan (FailureResponse (Response badRequest400 _ _ _)) =
  updatePlan (Just basicAuth) (id plan) plan
updateInsteadOfCreate _ _ e = throwError e



createPlans' :: String -> [Plan] -> ClientM [WrappedPlan]
createPlans' _ [] = return []
createPlans' apikey (p:rest) = do
  let auth = apikey ++ ":"
  let encoded = unpack (encode (pack auth))
  let basicAuth = "Basic " ++ encoded
  plan <- catchError (createPlan (Just basicAuth) p) (updateInsteadOfCreate basicAuth p)
  more <- createPlans' apikey rest
  return (plan:more)

minimalS4Plan = minimalPlan
  { trial_period = Just 1
  , trial_period_unit = Just "month"
  , charge_model = Just "flat_fee"
  , taxable = Just True
  , status = Just "active"
  }

s4Plans :: [Plan]
s4Plans =
  [ minimalS4Plan {
      id = "s4_250gb_monthly_eur"
      , name = "S4 250 GB (monthly EUR)"
      , period = Just 1
      , period_unit = Just "month"
      , currency_code = "EUR"
      , price = Just 995
      }
  , minimalS4Plan {
      id = "s4_250gb_yearly_eur"
      , name = "S4 250 GB (yearly EUR)"
      , period = Just 1
      , period_unit = Just "year"
      , currency_code = "EUR"
      , price = Just 10800
      }
  , minimalS4Plan {
      id = "s4_250gb_monthly_usd"
      , name = "S4 250 GB (monthly USD)"
      , period = Just 1
      , period_unit = Just "month"
      , currency_code = "USD"
      , price = Just 995
      }
  , minimalS4Plan {
      id = "s4_250gb_yearly_usd"
      , name = "S4 250 GB (yearly USD)"
      , period = Just 1
      , period_unit = Just "year"
      , currency_code = "USD"
      , price = Just 10800
      }
  , minimalS4Plan {
      id = "s4_5tb_monthly_eur"
      , name = "S4 5 TB (monthly EUR)"
      , period = Just 1
      , period_unit = Just "month"
      , currency_code = "EUR"
      , price = Just 2595
      }
  , minimalS4Plan {
      id = "s4_5tb_yearly_eur"
      , name = "S4 5 TB (yearly EUR)"
      , period = Just 1
      , period_unit = Just "year"
      , currency_code = "EUR"
      , price = Just 29900
      }
  , minimalS4Plan {
      id = "s4_5tb_monthly_usd"
      , name = "S4 5 TB (monthly USD)"
      , period = Just 1
      , period_unit = Just "month"
      , currency_code = "USD"
      , price = Just 2595
      }
  , minimalS4Plan {
      id = "s4_5tb_yearly_usd"
      , name = "S4 5 TB (yearly USD)"
      , period = Just 1
      , period_unit = Just "year"
      , currency_code = "USD"
      , price = Just 29900
      }
  , minimalS4Plan {
      id = "s4_250gb_complementary"
      , name = "S4 250 GB Complementary"
      , period = Just 1
      , period_unit = Just "month"
      , currency_code = "EUR"
      , price = Nothing
      }
  , minimalS4Plan {
      id = "S4_consumer_iteration_2_beta1_2014-05-27"
      , name = "Simple Secure Storage Service (S4) 200GB"
      , period = Just 1
      , period_unit = Just "month"
      , currency_code = "USD"
      , price = Just 2500
      }
  ]
