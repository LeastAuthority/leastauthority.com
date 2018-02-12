{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where


import qualified Data.Text as DT
import Data.Text.Encoding (
  encodeUtf8
  )

import Data.String (
  String
  )

import qualified Data.ByteString as BS

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
  , Nameplate(Nameplate)
  , PlainText(PlainText)
  , generateSide
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

someFunc :: String-> BS.ByteString -> IO ()
someFunc rendezvous_bytes password_bytes = do
  let rendezvous = parseWebSocketEndpoint rendezvous_bytes
  let appid = AppID "tahoe-lafs.org/tahoe-lafs/v1"
  let config = PlainText "{this should be some json, so sorry}"
  case rendezvous of
    Nothing       -> putStrLn "bad url"
    Just endpoint -> do
      side <- generateSide
      runClient endpoint appid side (invite config password_bytes)

invite :: PlainText -> BS.ByteString -> Session -> IO ()
invite config password_bytes session = do
  nameplate <- allocate session
  putStrLn "Nameplate:"
  putStrLn $ show nameplate
  let (Nameplate nameplate_text) = nameplate
  let complete_password_bytes = BS.concat [encodeUtf8 nameplate_text, "-", password_bytes]
  let password = makePassword complete_password_bytes
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
