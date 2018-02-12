{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where


import qualified Data.Text as DT
import Data.Text.Encoding (
  encodeUtf8
  , decodeUtf8
  )

import Data.String (
  String
  )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Aeson (
  FromJSON
  , ToJSON(toJSON)
  , object
  , (.=)
  , encode
  )

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

data ServerIntroduction = ServerIntroduction ServerAbilities
data ServerAbilities = ServerAbilities ServerVersion
type ServerVersion = DT.Text

instance ToJSON ServerIntroduction where
  toJSON (ServerIntroduction abilities) = object [
    "abilities" .= abilities
    ]

instance ToJSON ServerAbilities where
  toJSON (ServerAbilities version) = object [
    version .= object [ ]
    ]

data Invitation = Invitation SharesHappy SharesNeeded SharesTotal Introducer Nickname
type SharesHappy = Integer
type SharesNeeded = Integer
type SharesTotal = Integer
type Introducer = DT.Text
type Nickname = DT.Text

instance ToJSON Invitation where
  toJSON (Invitation happy needed total introducer nickname) = object [
    "shares-happy" .= happy
    , "shares-needed" .= needed
    , "shares-total" .= total
    , "introducer" .= introducer
    , "nickname" .= nickname
    ]

someFunc :: String-> BS.ByteString -> IO ()
someFunc rendezvous_bytes password_bytes = do
  let rendezvous = parseWebSocketEndpoint rendezvous_bytes
  let appid = AppID "tahoe-lafs.org/tahoe-lafs/v1"
  let invitation = Invitation 2 2 3 "pb://foo" "some stuff"
  case rendezvous of
    Nothing       -> putStrLn "bad url"
    Just endpoint -> do
      side <- generateSide
      runClient endpoint appid side (invite invitation password_bytes)

invite :: Invitation -> BS.ByteString -> Session -> IO ()
invite invitation password_bytes session = do
  nameplate <- allocate session
  putStrLn "Nameplate:"
  putStrLn $ show nameplate
  let (Nameplate nameplate_text) = nameplate
  let complete_password_bytes = BS.concat [encodeUtf8 nameplate_text, "-", password_bytes]
  let password = makePassword complete_password_bytes
  mailbox <- claim session nameplate
  connection <- open session mailbox
  let encoded = PlainText $ LBS.toStrict $ encode invitation
  withEncryptedConnection connection password (send encoded)


send :: PlainText -> EncryptedConnection  -> IO ()
send message connection = do
  putStrLn "send"
  let intro = PlainText $ LBS.toStrict $ encode $ ServerIntroduction $ ServerAbilities "server-v1"
  putStrLn "Sending introduction"
  sendMessage connection intro
  client_intro <- atomically $ receiveMessage connection
  -- XXX Should check for a similar intro message to check for compat...
  putStrLn "got client_intro.  Sending:"
  putStrLn $ show message
  sendMessage connection message
