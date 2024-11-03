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
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics
import Lens.Micro.Platform
import Srtd.Dates (DateOrTime)
import Srtd.Todo
import System.IO.Unsafe (unsafePerformIO)

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

data AttrAutoDates = AttrAutoDates
  { -- | Time of creation of this item.
    created :: UTCTime,
    -- | Only tracks change to the attrs, *not* to hierarchy (e.g. moves)!
    --
    -- This ultimately seems like the better of two things to do, but note that we lose actual
    -- information doing this: we can't reconstruct it hierarchically.
    lastModified :: UTCTime,
    -- | Time of last update to the 'status' attribute, ignoring all other changes.
    -- Useful to track how long we've been waiting for how long a task has been next.
    --
    -- SOMEDAY if we ever wanna track more, we probably have to use barbies or something.
    lastStatusModified :: UTCTime
  }
  deriving (Show, Generic)

suffixLenses ''AttrAutoDates

initAutoDates :: UTCTime -> AttrAutoDates
initAutoDates now = AttrAutoDates now now now

-- | Default value for 'AttrAutoDates' if you don't care what they are.
--
-- NOTE this is a terrible default b/c it's evaluated lazily, so we really don't know what the
-- time is gonna be. This is only ok because it's only relevant for migration and/or situations
-- where we really don't care much what the time is.
unsafeDefaultAutoDates :: AttrAutoDates
unsafeDefaultAutoDates = initAutoDates (unsafePerformIO $ getCurrentTime)

-- SOMEDAY should be Text.
data Attr = Attr
  { name :: String,
    -- | If Nothing, this item is treated as a transparent "folder" by most analyses.
    status :: Maybe Status,
    dates :: AttrDates,
    autoDates :: AttrAutoDates
  }
  deriving (Show, Generic)

suffixLenses ''Attr

attrMinimal :: UTCTime -> String -> Attr
attrMinimal now s = Attr s Nothing noDates (initAutoDates now)

unsafeAttrMinimal :: String -> Attr
unsafeAttrMinimal = attrMinimal (unsafePerformIO getCurrentTime)

instance ToJSON Status where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Status

instance ToJSON AttrAutoDates where
  toEncoding = genericToEncoding jsonOptionsAttr
  toJSON = genericToJSON jsonOptionsAttr

instance FromJSON AttrAutoDates where
  parseJSON = genericParseJSON jsonOptionsAttr

instance ToJSON AttrDates where
  toEncoding = genericToEncoding jsonOptionsAttr
  toJSON = genericToJSON jsonOptionsAttr

instance FromJSON AttrDates where
  parseJSON = genericParseJSON jsonOptionsAttr

instance ToJSON Attr where
  toEncoding = genericToEncoding jsonOptionsAttr
  toJSON = genericToJSON jsonOptionsAttr

instance FromJSON Attr where
  -- Need to write it out manually here to make 'dates' optional (b/c it's not a Maybe), for migration.
  -- Also other migration.
  parseJSON = withObject "Attr" $ \v ->
    Attr
      <$> v .: "name"
      <*> v .:? "status"
      <*> v .:? "dates" .!= noDates
      <*> v .:? "autoDates" .!= unsafeDefaultAutoDates

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

-- * Helpers

setLastModified :: UTCTime -> Attr -> Attr
setLastModified now = autoDatesL . lastModifiedL .~ now

setLastStatusModified :: UTCTime -> Attr -> Attr
setLastStatusModified now = autoDatesL %~ ((lastModifiedL .~ now) . (lastStatusModifiedL .~ now))

-- | Derived properties. These are *not* saved but recomputed live as needed.
data DerivedAttr = DerivedAttr
  { -- | Actionability of the most actionable child. Nothing can mean both 'no status' and 'no children'
    -- TODO is this a problem? If so, maybe make a custom data structure or reorganize somehow.
    daChildActionability :: Maybe Status
  }
  deriving (Show)

-- | The 'DerivedAttr' of an element without any children
emptyDerivedAttr :: DerivedAttr
emptyDerivedAttr =
  DerivedAttr
    { daChildActionability = Nothing
    }
