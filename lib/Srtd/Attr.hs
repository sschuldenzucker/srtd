{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Attributes. There's no deep reason this is separate other than to avoid cycles in the module graph.
module Srtd.Attr where

-- SOMEDAY it's not very nice we depend on Brick here. Copy the definition (I'm sure it's easy)
import Brick (suffixLenses)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text qualified as Text
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics
import Srtd.Dates (DateOrTime)

data EID = Inbox | Vault | EIDNormal (UUID) deriving (Eq, Ord, Show)

-- | The default Ord instance (i.e., the order of constructors) is by actionability.
data Status
  = -- | Work in progress. Like Next, but also I am actively working on it right now.
    WIP
  | -- | Ready to be worked on
    Next
  | -- | Active project. Checked for next actions / being stuck.
    Project
  | -- | Not ready, but will likely become Next later.
    Waiting
  | -- | Open point. Something we can't / don't want to do anything about rn but likely needs to be
    -- resolved to complete the project, and also no clear *other* person is responsible for doing this.
    -- Otherwise treated similar to Waiting.
    Open
  | -- | Optional and, if it happens, later. Not committed to.
    Later
  | -- | Waiting for someone else. (or *maybe* on an event to happen, not sure)
    Someday
  | -- | Done. (NB there's no 'archived' tag right now)
    Done
  deriving (Eq, Ord, Show, Generic)

compareStatusActionability :: Status -> Status -> Ordering
compareStatusActionability = compare

-- | Ordering for (Maybe Status) by actionability
--
-- NB Nothing gets the lowest (most actionable) status. This is appropriate because these are
-- usually containers for notes etc. that should be at the top.
compareMStatusActionability :: Maybe Status -> Maybe Status -> Ordering
compareMStatusActionability = compare

suffixLenses ''Status

jsonOptionsAttr :: Options
jsonOptionsAttr = defaultOptions {omitNothingFields = True}

data AttrDates = AttrDates
  { -- SOMEDAY consistency check: remind <= schedule <= goalline <= deadline, if any
    deadline :: Maybe DateOrTime,
    goalline :: Maybe DateOrTime,
    scheduled :: Maybe DateOrTime,
    remind :: Maybe DateOrTime
  }
  deriving (Show, Generic)

suffixLenses ''AttrDates

noDates :: AttrDates
noDates = AttrDates Nothing Nothing Nothing Nothing

-- SOMEDAY should be Text.
data Attr = Attr
  { name :: String,
    -- | If Nothing, this item is treated as a transparent "folder" by most analyses.
    -- TODO should Status have a special 'None' element instead? Or just a default 'Item" or whatever?
    status :: Maybe Status,
    dates :: AttrDates
  }
  deriving (Show, Generic)

suffixLenses ''Attr

attrMinimal :: String -> Attr
attrMinimal s = Attr s Nothing noDates

instance ToJSON Status where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Status

instance ToJSON AttrDates where
  toEncoding = genericToEncoding jsonOptionsAttr
  toJSON = genericToJSON jsonOptionsAttr

instance FromJSON AttrDates where
  parseJSON = genericParseJSON jsonOptionsAttr

instance ToJSON Attr where
  toEncoding = genericToEncoding jsonOptionsAttr
  toJSON = genericToJSON jsonOptionsAttr

instance FromJSON Attr where
  -- Need to write it out manually here to make 'dates' optional (b/c it's not a Maybe)
  parseJSON = withObject "Attr" $ \v ->
    Attr
      <$> v .: "name"
      <*> v .:? "status"
      <*> v .:? "dates" .!= noDates

-- We use a custom instance here to get more readable JSON.
instance ToJSON EID where
  -- SOMEDAY implement toEncoding for speed, I can't be bothered rn.
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
