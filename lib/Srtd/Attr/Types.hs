{-| Separate module for attr types b/c template haskell gets in the way of sane ordering.

We don't _need_ a separate module for this (it's enough to put the declarations together in a
block at the top to avoid bogus out-of-scope issues), but this feels cleaner.

This module only contains types and some instances, and very simple definitions required for these
instances; almost all of the functionality is implemented in 'Srtd.Attr'. There's no business logic
here!
-}
module Srtd.Attr.Types where

import Brick (suffixLenses)
import Data.Aeson (
  FromJSON (..),
  Options (..),
  ToJSON (..),
  Value (..),
  defaultOptions,
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Aeson.Types (typeMismatch)
import Data.EnumMap.Strict (EnumMap)
import Data.Text qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics (Generic, Generic1)
import GHC.IO (unsafePerformIO)
import Lens.Micro.Platform (Lens', view, _1, _2)
import Srtd.Dates (DateOrTime, DateRule)
import Srtd.Util (safeHead)

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
    -- SOMEDAY move this before Project. (but doesn't matter in practice I think)
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
  | -- | Canceled. Never done. We separate this from Done b/c Canceled is more likely to be revived.
    Canceled
  | -- | Done. (NB there's no 'archived' tag right now)
    Done
  deriving (Eq, Ord, Show, Generic, Enum)

-- | Generic version of the AttrDates container.
data AttrDates_ a = AttrDates
  { deadline :: a
  , goalline :: a
  , scheduled :: a
  , remind :: a
  }
  deriving (Show, Generic, Generic1, Functor)

noDates :: AttrDates
noDates = AttrDates Nothing Nothing Nothing Nothing

initAutoDates :: UTCTime -> AttrAutoDates
initAutoDates now = AttrAutoDates now now now

-- | Default value for 'AttrAutoDates' if you don't care what they are.
--
-- NOTE this is a terrible default b/c it's evaluated lazily, so we really don't know what the
-- time is gonna be. This is only ok because it's only relevant for migration and/or situations
-- where we really don't care much what the time is.
unsafeDefaultAutoDates :: AttrAutoDates
unsafeDefaultAutoDates = initAutoDates (unsafePerformIO $ getCurrentTime)

-- Not needed, we use the Generic1 instance instead.
-- instance Apply AttrDates_ where
--   (AttrDates af bf cf df) <.> (AttrDates a b c d) = AttrDates (af a) (bf b) (cf c) (df d)

-- | Concrete instantiation of the dates
type AttrDates = AttrDates_ (Maybe DateOrTime)

-- | Metadata container for each field of 'AttrDates'
data AttrDatesFieldMeta = AttrDatesFieldMeta
  { adfmDateRule :: DateRule
  }

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

-- * JSON Instance

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

-- * Derived Attrs

-- | Counts for visible children by status and actionability. Children are "visible" when their
-- status is not dominated (e.g., a NEXT item below another NEXT item is not visible), and generally we
-- avoid double counting.
--
-- NOTE This is not fully type safe: not all instances are valid. See 'gGlobalActionability'. The
-- data structure is generally questionable, see below.
--
-- SOMEDAY make it type safe.
--
-- SOMEDAY OR collapse the structure to something like (status -> (n, status -> n)) where the second
-- layer is actionability and isn't always there. Maybe.
data StatusActionabilityCounts
  = StatusActionabilityCounts
  { sacTotal :: Int
  -- ^ Total across all visible items. "Active size" indicator.
  , sacSingleStatuses :: EnumMap Status Int
  -- ^ Mapping from status to number of visible child items with that status
  --
  -- SOMEDAY if it's a bottleneck, use a vector (with default 0s this is well defined). Or just a big struct.
  , sacOpens :: EnumMap Status Int
  -- ^ OPEN items by actionability. For the total number of OPEN items see `sacSingleStatuses`.
  , sacProjects :: EnumMap Status Int
  -- ^ PROJECT items by actionability. For the total number of PROJECT items see `sacSingleStatuses`.
  }
  deriving (Show)

-- | Derived properties. These are *not* saved but recomputed live as needed.
data DerivedAttr = DerivedAttr
  { daChildActionability :: Status
  -- ^ Actionability of the most actionable child. None means either None or 'no children'
  -- SOMEDAY is this a problem? If so, maybe make a custom data structure or reorganize somehow.
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
  -- ^ Point-wise earliest dates of the children and including this node. Ignores 'isDone' nodes.
  , daLatestDates :: AttrDates
  -- ^ Point-wise latest dates of the children and including this node. Ignores 'isDone' nodes
  --
  -- NB This is probably not very useful.
  , daImpliedDates :: AttrDates
  -- ^ Point-wise earliest dates coming from the *parent* and including this node. This is the
  -- correct thing if you want to know, e.g., the deadline of this node if it's under a project.
  , daNDescendantsByActionability :: StatusActionabilityCounts
  -- ^ Item counts among the ancestors of the given item.
  }
  deriving (Show)

-- | Derived properties at the local (per-subtree / per-view) level
data LocalDerivedAttr = LocalDerivedAttr
  { ldParentActionability :: Status
  -- ^ Actionability of the parent, derived *only* downwards, contraining children. The meaning of
  -- this is to _constrain_ actionability of this node due to its place in the hierarchy.
  , ldBreadcrumbs :: [LocalIdLabel]
  -- ^ Ancestors starting at the parent up to and excluding the root.
  --
  -- SOMEDAY rename to ancestors.
  , ldLevel :: Int
  -- ^ Level local to the subtree. The root of the subtree (!) is -1, so all nodes have level >= 0.
  --
  -- SOMEDAY does this need to be here? Filters can change this. Same for some of the others.
  , ldIsCollapsed :: Bool
  -- ^ Whether or not this node is collapsed for display and no children are shown.
  , ldHiddenChildren :: Int
  -- ^ Number of direct children marked hidden, see 'HideHierarchyFilter'
  -- SOMEDAY this is a bit weird, should go somewhere else.
  , ldHiddenAncestors :: Int
  -- ^ Number of ancestors marked hidden, see 'HideHierarchyFilter'
  }
  deriving (Show)

-- * Common groupings of types

-- | Label (i.e., content) of an element in the global tree of items in memory
type Label = (Attr, DerivedAttr)

-- | Label in the global tree of items including the item ID
type IdLabel = (EID, Label)

-- | Label (i.e., content) of an element in the local (per-view) tree of items in memory
type LocalLabel = (Label, LocalDerivedAttr)

-- | Label in the local tree of items including the item ID
type LocalIdLabel = (EID, LocalLabel)

-- * Lenses

suffixLenses ''Status
suffixLenses ''AttrDates_
suffixLenses ''AttrAutoDates
suffixLenses ''Attr
suffixLenses ''StatusActionabilityCounts
suffixLenses ''DerivedAttr
suffixLenses ''LocalDerivedAttr

-- * Universal Accessors

-- These accessors let us access different attrs from various data structure. Inspired by RIO. A bit of boilerplate though.

-- SOMEDAY Revive the TH module so we can generate these and don't have to keep them up to date.

-- SOMEDAY make the classes hierarchical (so e.g. HasDerivedAttr requires HasAttr) to simplify a bit.
-- This implies that we can't have the trivial instances, but I think that's fine (we're not using them anyways I think)

class HasAttr a where
  getAttrL :: Lens' a Attr
  getAttr :: a -> Attr
  getAttr = view getAttrL

gName :: (HasAttr a) => a -> String
gName = name . getAttr

gStatus :: (HasAttr a) => a -> Status
gStatus = status . getAttr

gDates :: (HasAttr a) => a -> AttrDates
gDates = dates . getAttr

gAutoDates :: (HasAttr a) => a -> AttrAutoDates
gAutoDates = autoDates . getAttr

instance HasAttr Attr where getAttrL = id

instance HasAttr Label where getAttrL = _1

instance HasAttr IdLabel where getAttrL = _2 . getAttrL

instance HasAttr LocalLabel where getAttrL = _1 . getAttrL

instance HasAttr LocalIdLabel where getAttrL = _2 . getAttrL

class HasDerivedAttr a where
  getDerivedAttrL :: Lens' a DerivedAttr
  getDerivedAttr :: a -> DerivedAttr
  getDerivedAttr = view getDerivedAttrL

gChildActionability :: (HasDerivedAttr a) => a -> Status
gChildActionability = daChildActionability . getDerivedAttr

gEarliestAutodates :: (HasDerivedAttr a) => a -> AttrAutoDates
gEarliestAutodates = daEarliestAutodates . getDerivedAttr

gLatestAutodates :: (HasDerivedAttr a) => a -> AttrAutoDates
gLatestAutodates = daLatestAutodates . getDerivedAttr

gEarliestDates :: (HasDerivedAttr a) => a -> AttrDates
gEarliestDates = daEarliestDates . getDerivedAttr

gLatestDates :: (HasDerivedAttr a) => a -> AttrDates
gLatestDates = daLatestDates . getDerivedAttr

gImpliedDates :: (HasDerivedAttr a) => a -> AttrDates
gImpliedDates = daImpliedDates . getDerivedAttr

instance HasDerivedAttr DerivedAttr where getDerivedAttrL = id

instance HasDerivedAttr Label where getDerivedAttrL = _2

instance HasDerivedAttr IdLabel where getDerivedAttrL = _2 . getDerivedAttrL

instance HasDerivedAttr LocalLabel where getDerivedAttrL = _1 . getDerivedAttrL

instance HasDerivedAttr LocalIdLabel where getDerivedAttrL = _2 . getDerivedAttrL

class HasLocalDerivedAttr a where
  getLocalDerivedAttrL :: Lens' a LocalDerivedAttr
  getLocalDerivedAttr :: a -> LocalDerivedAttr
  getLocalDerivedAttr = view getLocalDerivedAttrL

gParentActionability :: (HasLocalDerivedAttr a) => a -> Status
gParentActionability = ldParentActionability . getLocalDerivedAttr

gLocalBreadcrumbs :: (HasLocalDerivedAttr a) => a -> [LocalIdLabel]
gLocalBreadcrumbs = ldBreadcrumbs . getLocalDerivedAttr

gLocalParent :: (HasLocalDerivedAttr a) => a -> Maybe LocalIdLabel
gLocalParent = safeHead . gLocalBreadcrumbs

gLocalLevel :: (HasLocalDerivedAttr a) => a -> Int
gLocalLevel = ldLevel . getLocalDerivedAttr

instance HasLocalDerivedAttr LocalDerivedAttr where getLocalDerivedAttrL = id

instance HasLocalDerivedAttr LocalLabel where getLocalDerivedAttrL = _2

instance HasLocalDerivedAttr LocalIdLabel where getLocalDerivedAttrL = _2 . getLocalDerivedAttrL

-- SOMEDAY this isn't used super much and maybe isn't useful.
class HasEID a where
  getEIDL :: Lens' a EID
  getEID :: a -> EID
  getEID = view getEIDL

gEID :: (HasEID a) => a -> EID
gEID = getEID

instance HasEID EID where getEIDL = id

instance HasEID (EID, a) where getEIDL = _1
