{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

main :: IO ()
main = someFunc "ws://wormhole.staging.leastauthority.com:4000/v1" "moo"
