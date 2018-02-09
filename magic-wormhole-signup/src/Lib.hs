{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import GHC.Conc (
  atomically
  )

import Crypto.Spake2 (
  Password
  , makePassword
  )

import MagicWormhole (
  runClient
  , parseWebSocketEndpoint
  , AppID(AppID)
  , Side(Side)
  , Session
  , PlainText(PlainText)
  , list
  , allocate
  , claim
  , open
  , close
  , withEncryptedConnection
  , EncryptedConnection
  , sendMessage
  , receiveMessage
  )

-- https://github.com/LeastAuthority/haskell-magic-wormhole/issues/21
import MagicWormhole.Internal.Rendezvous (
  ping
  , release
  )

someFunc :: IO ()
someFunc = do
  let url = parseWebSocketEndpoint "ws://wormhole.staging.leastauthority.com:4000/v1"
  let password = makePassword "inhabited-destiny"
  let appid = AppID "tahoe-lafs.org/tahoe-lafs/v1"
  let side = Side "server"
  let config = PlainText "{this should be some json, so sorry}"
  case url of
    Nothing       -> putStrLn "bad url"
    Just endpoint -> runClient endpoint appid side (invite config password)

invite :: PlainText -> Password -> Session -> IO ()
invite config password session = do
  ping session 3
  nameplates <- list session
  putStrLn $ show nameplates
  nameplate <- allocate session
  putStrLn "Nameplate:"
  putStrLn $ show nameplate
  mailbox <- claim session nameplate
  connection <- open session mailbox
  withEncryptedConnection connection password (send config)


send :: PlainText -> EncryptedConnection  -> IO ()
send config connection = do
  let intro = PlainText "{\"abilities\": {\"server-v1\": {}}}"
  sendMessage connection intro
  client_intro <- atomically $ receiveMessage connection
  -- XXX Should check for a similar intro message to check for compat...
  sendMessage connection config
