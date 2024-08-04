{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Attributes. There's no deep reason this is separate other than to avoid cycles in the module graph.
module Attr where

import Data.Aeson
import Data.Aeson.Types qualified as AT
import Data.Text qualified as Text
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics

data EID = Inbox | Vault | EIDNormal (UUID) deriving (Eq, Ord, Show)

data Attr = Attr
  { name :: String
  }
  deriving (Show, Generic)

instance ToJSON Attr where
  toEncoding = genericToEncoding defaultOptions

-- LATER As soon as we add more here, we prob want an implementation with optional fields (which can be configured somehow abstractly)
instance FromJSON Attr

-- We use a custom instance here to get more readable JSON.
instance ToJSON EID where
  -- SOMEDAY implement toEncoding, I can't be bothered rn.
  toJSON Inbox = String "INBOX"
  toJSON Vault = String "VAULT"
  toJSON (EIDNormal uuid) = String (UUID.toText uuid)

instance FromJSON EID where
  parseJSON (String txt)
    | txt == "INBOX" = return Inbox
    | txt == "VAULT" = return Vault
    | otherwise = case UUID.fromText txt of
        Just uuid -> return $ EIDNormal uuid
        Nothing -> fail $ "Invalid UUID: " ++ Text.unpack txt
  parseJSON val = AT.typeMismatch "String" val
