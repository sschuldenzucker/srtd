{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Attributes. There's no deep reason this is separate other than to avoid cycles in the module graph.
module Attr where

-- SOMEDAY it's not very nice we depend on Brick here. Copy the definition (I'm sure it's easy)
import Brick (suffixLenses)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text qualified as Text
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics

data EID = Inbox | Vault | EIDNormal (UUID) deriving (Eq, Ord, Show)

data Status
  = -- | Ready to be worked on
    Next
  | -- | Not ready, but will likely become Next later.
    Later
  | -- | Waiting for someone else. (or *maybe* on an event to happen, not sure)
    Waiting
  | -- | Optional and, if it happens, later. Not committed to.
    Someday
  | -- | Done. (NB there's no 'archived' tag right now)
    Done
  | -- | Active project. Checked for next actions / being stuck.
    Project
  | -- | Open point. Something we can't / don't want to do anything about rn but likely needs to be
    -- resolved to complete the project, and also no clear *other* person is responsible for doing this.
    -- Otherwise treated similar to Waiting.
    Open
  | -- | Work in progress. Like Next, but also I am actively working on it right now.
    WIP
  deriving (Eq, Show, Generic)

suffixLenses ''Status

-- SOMEDAY should be Text.
data Attr = Attr
  { name :: String,
    -- | If Nothing, this item is treated as a transparent "folder" by most analyses.
    -- TODO should Status have a special 'None' element instead? Or just a default 'Item" or whatever?
    status :: Maybe Status
  }
  deriving (Show, Generic)

suffixLenses ''Attr

attrMinimal :: String -> Attr
attrMinimal s = Attr s Nothing

instance ToJSON Status where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Status

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
  parseJSON val = typeMismatch "String" val
