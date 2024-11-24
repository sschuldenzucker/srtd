{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Attributes. There's no deep reason this is separate other than to avoid cycles in the module graph.
module Srtd.Attr where

-- SOMEDAY it's not very nice we depend on Brick here. Copy the definition (I'm sure it's easy)
import Brick (suffixLenses)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Function (on)
import Data.Text qualified as Text
import Data.Time (TimeZone, UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics
import Lens.Micro.Platform
import Srtd.Dates (DateOrTime, DateRule (..), compareDateOrTime)
import Srtd.Todo
import Srtd.Util (compareByNothingLast, maybe2, unionMaybeWith)
import System.IO.Unsafe (unsafePerformIO)

-- * Node ID (EID)

data EID = Inbox | Vault | EIDNormal (UUID) deriving (Eq, Ord, Show)

showEIDShort :: EID -> String
showEIDShort Inbox = "INBOX"
showEIDShort Vault = "VAULT"
showEIDShort (EIDNormal uuid) = show uuid

-- * Attr Types and helpers

-- ** Status enum

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
  | -- | No status assigned. For notes, containers, areas of responsibility, and "stuff"
    None
  | -- | Done. (NB there's no 'archived' tag right now)
    Done
  deriving (Eq, Ord, Show, Generic)

suffixLenses ''Status

-- ** Dates

data AttrDates = AttrDates
  { -- SOMEDAY consistency check: remind <= schedule <= goalline <= deadline, if any
    deadline :: Maybe DateOrTime
  , goalline :: Maybe DateOrTime
  , scheduled :: Maybe DateOrTime
  , remind :: Maybe DateOrTime
  }
  deriving (Show, Generic)

suffixLenses ''AttrDates

noDates :: AttrDates
noDates = AttrDates Nothing Nothing Nothing Nothing

-- | Map a function over each element of 'AttrDates'
--
-- SOMEDAY Unused. Also, there's probably some generics trick to get this.
mapAttrDates2 ::
  (Maybe DateOrTime -> Maybe DateOrTime -> Maybe DateOrTime) -> AttrDates -> AttrDates -> AttrDates
mapAttrDates2 f ad1 ad2 =
  AttrDates
    { deadline = (f `on` deadline) ad1 ad2
    , goalline = (f `on` goalline) ad1 ad2
    , scheduled = (f `on` scheduled) ad1 ad2
    , remind = (f `on` remind) ad1 ad2
    }

-- | Given a choice function, apply the correct beginning/end-of-day rule to each element to choose.
-- (beginning for remind, end for everything else)
pointwiseChooseAttrDates ::
  (Ordering -> DateOrTime -> DateOrTime -> DateOrTime) ->
  TimeZone ->
  AttrDates ->
  AttrDates ->
  AttrDates
pointwiseChooseAttrDates ch tz ad1 ad2 =
  AttrDates
    { deadline = chooseWith EndOfDay deadline
    , goalline = chooseWith EndOfDay goalline
    , scheduled = chooseWith EndOfDay scheduled
    , remind = chooseWith BeginningOfDay remind
    }
 where
  -- 'Just' values are always dominant! (doesn't matter what 'ch' does)
  chooseWith dr f = (unionMaybeWith (chooseWith' dr) `on` f) ad1 ad2
  chooseWith' dr d1 d2 = ch (compareDateOrTime dr tz d1 d2) d1 d2

data AttrAutoDates = AttrAutoDates
  { created :: UTCTime
  -- ^ Time of creation of this item.
  , lastModified :: UTCTime
  -- ^ Only tracks change to the attrs, *not* to hierarchy (e.g. moves)!
  --
  -- This ultimately seems like the better of two things to do, but note that we lose actual
  -- information doing this: we can't reconstruct it hierarchically.
  , lastStatusModified :: UTCTime
  -- ^ Time of last update to the 'status' attribute, ignoring all other changes.
  -- Useful to track how long we've been waiting for how long a task has been next.
  --
  -- SOMEDAY if we ever wanna track more, we probably have to use barbies or something.
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

-- SOMEDAY I'm sure there's some smart anti-boilerplate thing that would do this for us. (uniplate or so?!)
mapAttrAutoDates2 ::
  (UTCTime -> UTCTime -> UTCTime) -> AttrAutoDates -> AttrAutoDates -> AttrAutoDates
mapAttrAutoDates2 f ad1 ad2 =
  AttrAutoDates
    { created = (f `on` created) ad1 ad2
    , lastModified = (f `on` lastModified) ad1 ad2
    , lastStatusModified = (f `on` lastStatusModified) ad1 ad2
    }

-- ** Master Attr structure

-- | Master Attr structure. These are stored persistently with each item.
--
-- SOMEDAY should use Text instead of String.
data Attr = Attr
  { name :: String
  , status :: Status
  , dates :: AttrDates
  , autoDates :: AttrAutoDates
  }
  deriving (Show, Generic)

suffixLenses ''Attr

-- | Minimal valid attr
attrMinimal :: UTCTime -> String -> Attr
attrMinimal now s = Attr s None noDates (initAutoDates now)

unsafeAttrMinimal :: String -> Attr
unsafeAttrMinimal = attrMinimal (unsafePerformIO getCurrentTime)

-- ** JSON Instance

jsonOptionsAttr :: Options
jsonOptionsAttr = defaultOptions {omitNothingFields = True}

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
      <*> v .:? "status" .!= None
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

-- ** Modification Helpers

setLastModified :: UTCTime -> Attr -> Attr
setLastModified now = autoDatesL . lastModifiedL .~ now

setLastStatusModified :: UTCTime -> Attr -> Attr
setLastStatusModified now = autoDatesL %~ ((lastModifiedL .~ now) . (lastStatusModifiedL .~ now))

-- * Global Derived Attr

-- | Derived properties. These are *not* saved but recomputed live as needed.
data DerivedAttr = DerivedAttr
  { daChildActionability :: Status
  -- ^ Actionability of the most actionable child. None means either None or 'no children'
  -- TODO is this a problem? If so, maybe make a custom data structure or reorganize somehow.
  -- SOMEDAY include this node so daChildActionability ~ glActionability, unless we need this for some reason. (e.g. to isolate blockers??)
  , daEarliestAutodates :: AttrAutoDates
  -- ^ Point-wise earliest autodates of the children and including this node.
  --
  -- NB This is probably not very useful, except for "age" metrics, maybe.
  --
  -- SOMEDAY Inconsistent with daChildActionability, this *includes* the current node.
  , daLatestAutodates :: AttrAutoDates
  -- ^ Point-wise latest autodates of the children and including this node.
  , daEarliestDates :: AttrDates
  -- ^ Point-wise earliest dates of the children and including this node.
  , daLatestDates :: AttrDates
  -- ^ Point-wise latest dates of the children and including this node.
  --
  -- NB This is probably not very useful.
  , daImpliedDates :: AttrDates
  -- ^ Point-wise earliest dates coming from the *parent* and including this node. This is the
  -- correct thing if you want to know, e.g., the deadline of this node if it's under a project.
  }
  deriving (Show)

-- | The 'DerivedAttr' of an element without any children or parent
emptyDerivedAttr :: Attr -> DerivedAttr
emptyDerivedAttr attr =
  DerivedAttr
    { daChildActionability = None
    , daEarliestAutodates = autoDates attr
    , daLatestAutodates = autoDates attr
    , daEarliestDates = dates attr
    , daLatestDates = dates attr
    , daImpliedDates = dates attr
    }

-- | Label (i.e., content) of an element in the global tree of items in memory
type Label = (Attr, DerivedAttr)

-- | Label in the global tree of items including the item ID
type IdLabel = (EID, Label)

-- | Treat Nothing and Project statuses as transparent (they consider children) and everything else
-- not (they consider the item only)
glActionability :: Label -> Status
glActionability (attr, dattr) = case (status attr, daChildActionability dattr) of
  (None, a) -> a
  (Project, a) -> a
  (s, _) -> s

-- * Local Derived Attr

-- | Derived properties at the local (per-subtree / per-view) level
data LocalDerivedAttr = LocalDerivedAttr
  { ldParentActionability :: Status
  -- ^ Actionability of the parent, derived downwards
  --
  -- SOMEDAY Replace this by what's llActionability right now.
  }
  deriving (Show)

-- | Label (i.e., content) of an element in the local (per-view) tree of items in memory
type LocalLabel = (Label, LocalDerivedAttr)

-- | Label in the local tree of items including the item ID
type LocalIdLabel = (EID, LocalLabel)

llActionability :: LocalLabel -> Status
llActionability (label, ldattr) = case (glActionability label, ldParentActionability ldattr) of
  -- SOMEDAY I've seen this patterns a few times now, perhaps abstract it or restructure.
  -- See also 'addLocalDerivedAttrs' in Model and 'glActionability' above. It's duplicated with there.
  (a, None) -> a
  (a, Project) -> a
  (a, ap) -> max a ap

-- SOMEDAY these unused?

llEarliestChildAutodates :: LocalLabel -> AttrAutoDates
llEarliestChildAutodates ((_attr, dattr), _ldattr) = daEarliestAutodates dattr

llLatestChildAutodates :: LocalLabel -> AttrAutoDates
llLatestChildAutodates ((_attr, dattr), _ldattr) = daLatestAutodates dattr
