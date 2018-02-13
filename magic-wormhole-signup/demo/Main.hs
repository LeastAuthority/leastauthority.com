{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 (
  pack
  )

import System.Environment (
  getArgs
  )

import Lib (
  invite
  )

main :: IO ()
main = do
  args <- getArgs
  case args of
    rendezvous_string:password:[] -> invite rendezvous_string $ pack password
    otherwise                     -> putStrLn "Usage: schmoo <rendezvous url> <password>"
