module Main where

import System.Console.ArgParser
  ( ParserSpec
  , Descr(Descr)
  , parsedBy
  , andBy
  , reqPos
  , withParseResult
  )

import Lib (createPlans)

data CreatePlanArgs = CreatePlanArgs
  { siteName :: String
  , apiKey :: String
  }

planParser :: ParserSpec CreatePlanArgs
planParser = CreatePlanArgs
  `parsedBy` reqPos "site-name" `Descr` "Create plans in the ChargeBee site with this  name."
  `andBy` reqPos "api-key" `Descr` "Create plans using this ChargeBee API key."

main :: IO ()
main =
  withParseResult planParser (\args -> createPlans (siteName args) (apiKey args))
