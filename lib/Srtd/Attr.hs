-- SOMEDAY it's not very nice we depend on Brick here. Copy the definition (I'm sure it's easy)
-- SOMEDAY consistency check: remind <= schedule <= goalline <= deadline, if any

-- | Attributes. There's no deep reason this is separate other than to avoid cycles in the module graph.
module Srtd.Attr where

import Brick (suffixLenses)
import Control.Arrow (second)
import Control.Monad (liftM2)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.EnumMap.Strict (EnumMap)
import Data.EnumMap.Strict qualified as EnumMap
import Data.Function (on)
import Data.Functor.Apply
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Time (TimeZone, UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics
import Lens.Micro.Platform
import Srtd.Dates (DateOrTime, DateRule (..), compareDateOrTime, dateOrTimeToUTCTime)
import Srtd.Util (chooseMin, compareByNothingLast, unionMaybeWith)
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

suffixLenses ''Status

-- | Whether status is 'Done' or 'Canceled'. This excludes an item from many analyses. A bit of a
-- misnomer but I didn't want to make up a new term.
isDone :: Status -> Bool
isDone = \case
  Canceled -> True
  Done -> True
  _ -> False

-- ** Dates

-- | Generic version of the AttrDates container.
data AttrDates_ a = AttrDates
  { deadline :: a
  , goalline :: a
  , scheduled :: a
  , remind :: a
  }
  deriving (Show, Generic, Generic1, Functor)

-- Not needed, we use the Generic1 instance instead.
-- instance Apply AttrDates_ where
--   (AttrDates af bf cf df) <.> (AttrDates a b c d) = AttrDates (af a) (bf b) (cf c) (df d)

suffixLenses ''AttrDates_

-- | Concrete instantiation of the dates
type AttrDates = AttrDates_ (Maybe DateOrTime)

noDates :: AttrDates
noDates = AttrDates Nothing Nothing Nothing Nothing

isDatesEmpty :: AttrDates_ (Maybe a) -> Bool
isDatesEmpty (AttrDates Nothing Nothing Nothing Nothing) = True
isDatesEmpty _ = False

-- | Metadata container for each field of 'AttrDates'
data AttrDatesFieldMeta = AttrDatesFieldMeta
  { adfmDateRule :: DateRule
  }

attrDatesMeta :: AttrDates_ AttrDatesFieldMeta
attrDatesMeta =
  AttrDates
    { deadline = AttrDatesFieldMeta EndOfDay
    , goalline = AttrDatesFieldMeta EndOfDay
    , scheduled = AttrDatesFieldMeta EndOfDay
    , remind = AttrDatesFieldMeta EndOfDay
    }

attrDatesDateRules :: AttrDates_ DateRule
attrDatesDateRules = fmap adfmDateRule attrDatesMeta

-- | List in order of urgency. (= what's in the data type definition)
--
-- SOMEDAY can generics do this for me?
attrDates2List :: AttrDates_ a -> [a]
attrDates2List (AttrDates dl gl sd rm) = [dl, gl, sd, rm]

attrDates2UTCTime :: TimeZone -> AttrDates -> AttrDates_ (Maybe UTCTime)
attrDates2UTCTime tz = gliftF2 go attrDatesDateRules
 where
  go dr = fmap (dateOrTimeToUTCTime dr tz)

-- | Compare lexicographically in order of seriousness.
--
-- Note that this is quite harsh, e.g., if d1 has a deadline in 6 months but d2 has a goalline
-- tomorrow, d1 is sorted before d2.
--
-- SOMEDAY unused except once below.
compareAttrDatesLex :: TimeZone -> AttrDates -> AttrDates -> Ordering
compareAttrDatesLex tz d1 d2 =
  mconcat . attrDates2List $ gliftF3 go attrDatesDateRules d1 d2
 where
  go dr = compareByNothingLast (compareDateOrTime dr tz)

-- | Compare two dates by their earliest time and break ties by seriousness.
--
-- This means that a remind today will be before a deadline tomorrow but a deadline today will be
-- before a remind today.
compareAttrDates :: TimeZone -> AttrDates -> AttrDates -> Ordering
compareAttrDates tz d1 d2 = mconcat [cmpByMins, cmpByLex]
 where
  cmpByMins = fromMaybe EQ $ liftM2 compare (attrDatesMinUTCTime d1) (attrDatesMinUTCTime d2)
  attrDatesMinUTCTime :: AttrDates -> Maybe UTCTime
  attrDatesMinUTCTime =
    minimumBy (compareByNothingLast compare)
      . attrDates2List
      . gliftF2 (\dr -> fmap $ dateOrTimeToUTCTime dr tz) attrDatesDateRules
  cmpByLex = compareAttrDatesLex tz d1 d2

-- | Given a choice function, apply the correct beginning/end-of-day rule to each element to choose.
-- (beginning for remind, end for everything else)
pointwiseChooseAttrDates ::
  (Ordering -> DateOrTime -> DateOrTime -> DateOrTime) ->
  TimeZone ->
  AttrDates ->
  AttrDates ->
  AttrDates
pointwiseChooseAttrDates ch tz = gliftF3 go attrDatesDateRules
 where
  go dr = unionMaybeWith $ \dt1 dt2 -> ch (compareDateOrTime dr tz dt1 dt2) dt1 dt2

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

suffixLenses ''StatusActionabilityCounts

sacEmpty :: StatusActionabilityCounts
sacEmpty = StatusActionabilityCounts 0 EnumMap.empty EnumMap.empty EnumMap.empty

-- | StatusActionabilityCounts for the parent with given status and actionability. This handles
-- visibility / transparency rules for counting.
--
-- SOMEDAY this is the third time we reproduce this pattern, together with g{Global,Local}Actionability.
-- Make some abstraction for it to only have it in one place.
sacForParent :: Status -> Status -> StatusActionabilityCounts -> StatusActionabilityCounts
sacForParent s_ a_ sac = case (s_, a_) of
  -- None aren't counted
  (None, _) -> sac
  -- No double counting for NEXT items with WIP children
  (Next, WIP) -> sac
  -- Projects and Opens are counted but not double counted with the same actionability up the hierarchy.
  -- EXPERIMENTAL. Limit stats within projects, intentional for status bar.
  -- SOMEDAY there's a weird split in heavy lifting vs gGlobalActionability here. (this code only works b/c `a` is coming from that code.)
  -- SOMEDAY maybe there should be two routines and accounting members: one that compute "deep counts", one that computes "shallow counts". We currently only use this for the status bar, so we don't need "deep" but may be useful at some point.
  {- (s@Project, a) -> case EnumMap.lookup a (sacProjects sac) of
    Just _ -> sac
    Nothing ->
      sac
        & sacProjectsL
        %~ EnumMap.insert a 1
        & sacSingleStatusesL
        %~ EnumMap.insertWith (+) s 1
        & sacTotalL
        %~ (+ 1) -}
  (s@Project, a) ->
    StatusActionabilityCounts
      (sacTotal sac + 1) -- SOMEDAY inconsistency to the other counts. Ok for "size" metric but...
      (EnumMap.insert s 1 $ sacSingleStatuses sac)
      (sacOpens sac)
      (EnumMap.fromList [(a, 1)])
  (s@Open, a) | a <= Open -> case EnumMap.lookup a (sacOpens sac) of
    Just _ -> sac
    Nothing ->
      sac
        & sacOpensL
        %~ EnumMap.insert a 1
        & sacSingleStatusesL
        %~ EnumMap.insertWith (+) s 1
        & sacTotalL
        %~ (+ 1)
  -- All other statuses do not inherit anything
  (s, _) -> StatusActionabilityCounts 1 (EnumMap.fromList [(s, 1)]) EnumMap.empty EnumMap.empty

-- | Union sacs across siblings
sacUnionForSiblings ::
  [StatusActionabilityCounts] -> StatusActionabilityCounts
sacUnionForSiblings sacs =
  StatusActionabilityCounts
    { sacTotal = sum . map sacTotal $ sacs
    , sacSingleStatuses = EnumMap.unionsWith (+) . map sacSingleStatuses $ sacs
    , sacOpens = EnumMap.unionsWith (+) . map sacOpens $ sacs
    , sacProjects = EnumMap.unionsWith (+) . map sacProjects $ sacs
    }

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

suffixLenses ''DerivedAttr

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
    , daNDescendantsByActionability = sacEmpty
    }

-- | Label (i.e., content) of an element in the global tree of items in memory
type Label = (Attr, DerivedAttr)

-- | Label in the global tree of items including the item ID
type IdLabel = (EID, Label)

-- * Local Derived Attr

-- | Derived properties at the local (per-subtree / per-view) level
data LocalDerivedAttr = LocalDerivedAttr
  { ldParentActionability :: Status
  -- ^ Actionability of the parent, derived downwards
  --
  -- SOMEDAY Replace this by what's llActionability right now.
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

-- | Label (i.e., content) of an element in the local (per-view) tree of items in memory
type LocalLabel = (Label, LocalDerivedAttr)

-- | Label in the local tree of items including the item ID
type LocalIdLabel = (EID, LocalLabel)

suffixLenses ''LocalDerivedAttr

-- | Convenience transformation
localIdLabel2IdLabel :: LocalIdLabel -> IdLabel
localIdLabel2IdLabel = second fst

-- SOMEDAY make Label and LocalLabel into proper data structures.

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

-- * Derived attr calculation

-- | Actionability based on the node and its children, but ignoring its parents. This is globally
-- the same for all views.
--
-- Treat Nothing and Project statuses as transparent (they consider children) and everything else
-- not (they consider the item only)
--
-- SOMEDAY naming is bad, should explicitly mention children
gGlobalActionability :: (HasAttr a, HasDerivedAttr a) => a -> Status
gGlobalActionability x = case (gStatus x, gChildActionability x) of
  (None, a) -> a
  -- This a little bit inconsistent but we typically *mean* this.
  -- Con: there's no way to pause a WIP task to next now.
  -- SOMEDAY handle this using the new "force" status.
  (Next, WIP) -> WIP
  (Project, a) -> a
  -- This makes Open items semi-transparent: they're transparent to Next etc. tasks but also stand
  -- for their own (likely project blockers). This is *different* from Project nodes, which are
  -- fully transparent!
  -- SOMEDAY we could also make it transparent to Later but I'm not right now.
  (Open, a) | a <= Open -> a
  (s, _) -> s

-- | Actionability based on 'gGlobalActionability' and also its parents. This is (or can be) local
-- to the view b/c it depends on which part of the tree is visible to the given view.
gLocalActionability :: (HasAttr a, HasDerivedAttr a, HasLocalDerivedAttr a) => a -> Status
gLocalActionability x = case (gGlobalActionability x, gParentActionability x) of
  -- SOMEDAY I've seen this patterns a few times now, perhaps abstract it or restructure.
  -- See also 'addLocalDerivedAttrs' in Model and 'glActionability' above. It's duplicated with there.
  (a, None) -> a
  (WIP, Next) -> WIP
  (a, Project) -> a
  (a, ap) -> max a ap

-- | Implied or earliest child dates, whichever come earlier. This considers both parents and
-- children and is essentially "when this item has to be paid attention to", either directly or
-- because their children become relevant.
--
-- Note that, in contrast to actionability, these dates are always global.
gEarliestImpliedOrChildDates :: (HasDerivedAttr a) => TimeZone -> a -> AttrDates
gEarliestImpliedOrChildDates tz llabel = pointwiseChooseAttrDates chooseMin tz (gImpliedDates llabel) (gEarliestDates llabel)
