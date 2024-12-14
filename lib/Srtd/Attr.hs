-- SOMEDAY it's not very nice we depend on Brick here. Copy the definition (I'm sure it's easy)
-- SOMEDAY consistency check: remind <= schedule <= goalline <= deadline, if any

-- | Attributes. There's no deep reason this is separate other than to avoid cycles in the module graph.
module Srtd.Attr where

import Brick (suffixLenses)
import Control.Arrow (second)
import Control.Monad (liftM2)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
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
  | -- | Done. (NB there's no 'archived' tag right now)
    Done
  deriving (Eq, Ord, Show, Generic)

suffixLenses ''Status

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
  -- ^ Point-wise earliest dates of the children and including this node. Ignores 'Done' nodes.
  , daLatestDates :: AttrDates
  -- ^ Point-wise latest dates of the children and including this node. Ignores 'Done' nodes
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
  }
  deriving (Show)

-- | Label (i.e., content) of an element in the local (per-view) tree of items in memory
type LocalLabel = (Label, LocalDerivedAttr)

-- | Label in the local tree of items including the item ID
type LocalIdLabel = (EID, LocalLabel)

-- ** Convenience accessors

llActionability :: LocalLabel -> Status
llActionability (label, ldattr) = case (glActionability label, ldParentActionability ldattr) of
  -- SOMEDAY I've seen this patterns a few times now, perhaps abstract it or restructure.
  -- See also 'addLocalDerivedAttrs' in Model and 'glActionability' above. It's duplicated with there.
  (a, None) -> a
  (WIP, Next) -> WIP
  (a, Project) -> a
  (a, ap) -> max a ap

llImpliedDates :: LocalLabel -> AttrDates
llImpliedDates = daImpliedDates . snd . fst

llEarliestChildDates :: ((a, DerivedAttr), b) -> AttrDates
llEarliestChildDates = daEarliestDates . snd . fst

llEarliestImpliedOrChildDates :: TimeZone -> LocalLabel -> AttrDates
llEarliestImpliedOrChildDates tz llabel = pointwiseChooseAttrDates chooseMin tz (llImpliedDates llabel) (llEarliestChildDates llabel)

llName :: LocalLabel -> String
llName = name . fst . fst

llStatus :: LocalLabel -> Status
llStatus = status . fst . fst

llDates :: LocalLabel -> AttrDates
llDates = dates . fst . fst

localIdLabel2IdLabel :: LocalIdLabel -> IdLabel
localIdLabel2IdLabel = second fst

-- SOMEDAY all these accessors tell me that we should prob make LocalLabel etc. actual data structures instead of tuples.

llBreadcrumbs :: LocalLabel -> [LocalIdLabel]
llBreadcrumbs = ldBreadcrumbs . snd

-- SOMEDAY these unused?

llEarliestChildAutodates :: LocalLabel -> AttrAutoDates
llEarliestChildAutodates ((_attr, dattr), _ldattr) = daEarliestAutodates dattr

llLatestChildAutodates :: LocalLabel -> AttrAutoDates
llLatestChildAutodates ((_attr, dattr), _ldattr) = daLatestAutodates dattr
