-- SOMEDAY it's not very nice we depend on Brick here. Copy the definition (I'm sure it's easy)
-- SOMEDAY consistency check: remind <= schedule <= goalline <= deadline, if any

-- | Attributes. There's no deep reason this is separate other than to avoid cycles in the module graph.
module Srtd.Attr (
  -- * Reexport types
  module Srtd.Attr.Types,

  -- * EID
  showEIDShort,

  -- * Status
  isDone,
  isDatesEmpty,

  -- * Dates
  attrDatesMeta,
  attrDatesDateRules,
  attrDates2List,
  attrDates2UTCTime,
  compareAttrDatesLex,
  compareAttrDates,
  pointwiseChooseAttrDates,
  mapAttrAutoDates2,

  -- * Attr
  attrMinimal,
  unsafeAttrMinimal,
  setLastModified,
  setLastStatusModified,

  -- * StatusActionabilityCounts
  sacEmpty,
  sacForParent,
  sacUnionForSiblings,

  -- * Derived Attrs
  emptyDerivedAttr,

  -- * Local Derived Attrs
  localIdLabel2IdLabel,

  -- * Querying Business Logic
  applyActionabilityTransparency,
  applyActionabilityTransparencyFallback,
  gGlobalActionability,
  gLocalActionability,
  gEarliestImpliedOrChildDates,
  isGlobalStalledProject,
  isLocalStalledProject,
) where

import Control.Arrow (second)
import Control.Monad (liftM2)
import Data.EnumMap.Strict qualified as EnumMap
import Data.Function (on)
import Data.Functor.Apply
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Time (TimeZone, UTCTime, getCurrentTime)
import Lens.Micro.Platform
import Srtd.Attr.Types -- Types and data definitions are coming from here so TH doesn't fuck us.
import Srtd.Dates (DateOrTime, DateRule (..), compareDateOrTime, dateOrTimeToUTCTime)
import Srtd.Util (chooseMin, compareByNothingLast, ignore, unionMaybeWith)
import System.IO.Unsafe (unsafePerformIO)

-- * Node ID (EID)

showEIDShort :: EID -> String
showEIDShort Inbox = "INBOX"
showEIDShort Vault = "VAULT"
showEIDShort (EIDNormal uuid) = show uuid

-- * Attr Types and helpers

-- ** Status enum

-- | Whether status is 'Done' or 'Canceled'. This excludes an item from many analyses. A bit of a
-- misnomer but I didn't want to make up a new term.
isDone :: Status -> Bool
isDone = \case
  Canceled -> True
  Done -> True
  _ -> False

-- ** Dates

isDatesEmpty :: AttrDates_ (Maybe a) -> Bool
isDatesEmpty (AttrDates Nothing Nothing Nothing Nothing) = True
isDatesEmpty _ = False

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

-- | Minimal valid attr
attrMinimal :: UTCTime -> String -> Attr
attrMinimal now s = Attr s None noDates (initAutoDates now)

unsafeAttrMinimal :: String -> Attr
unsafeAttrMinimal = attrMinimal (unsafePerformIO getCurrentTime)

-- ** Modification Helpers

setLastModified :: UTCTime -> Attr -> Attr
setLastModified now = autoDatesL . lastModifiedL .~ now

setLastStatusModified :: UTCTime -> Attr -> Attr
setLastStatusModified now = autoDatesL %~ ((lastModifiedL .~ now) . (lastStatusModifiedL .~ now))

-- * Global Derived Attr

sacEmpty :: StatusActionabilityCounts
sacEmpty = StatusActionabilityCounts 0 EnumMap.empty EnumMap.empty EnumMap.empty 0

-- | StatusActionabilityCounts for the parent with given status and actionability. This handles
-- visibility / transparency rules for counting.
--
-- SOMEDAY this is the third time we reproduce this pattern, together with g{Global,Local}Actionability.
-- Make some abstraction for it to only have it in one place.
--
-- See `docs/actionability.md` for discussion.
sacForParent :: Label -> StatusActionabilityCounts -> StatusActionabilityCounts
sacForParent label sac = case (gStatus label, gGlobalActionability label) of
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
      -- For this logic see isLocalStalledProject
      ( if isGlobalStalledProject label
          then 1
          else
            if a > Next
              then sacNStalledProjects sac
              else 0
      )
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
        & sacNStalledProjectsL
        .~ 0
  -- All other statuses do not inherit anything
  (s, _) -> StatusActionabilityCounts 1 (EnumMap.fromList [(s, 1)]) EnumMap.empty EnumMap.empty 0

-- | Union sacs across siblings
sacUnionForSiblings ::
  [StatusActionabilityCounts] -> StatusActionabilityCounts
sacUnionForSiblings sacs =
  StatusActionabilityCounts
    { sacTotal = sum . map sacTotal $ sacs
    , sacSingleStatuses = EnumMap.unionsWith (+) . map sacSingleStatuses $ sacs
    , sacOpens = EnumMap.unionsWith (+) . map sacOpens $ sacs
    , sacProjects = EnumMap.unionsWith (+) . map sacProjects $ sacs
    , sacNStalledProjects = sum . map sacNStalledProjects $ sacs
    }

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

-- * Local Derived Attr

-- | Convenience transformation
localIdLabel2IdLabel :: LocalIdLabel -> IdLabel
localIdLabel2IdLabel = second fst

-- SOMEDAY make Label and LocalLabel into proper data structures.

-- * Derived attr calculation

-- | Given actionability/status of a child/children and actionability (or actionability constraint)
-- of the parent, check if any transparency rules apply and if so, return the status of the whole
-- parent-child object.
--
-- NB This isn't terribly well-defined theoretically, but we collect rules here that we have in
-- many places.
--
-- SOMEDAY the logic here is replicated with 'sacForParent', which is a bit bad and weird.
applyActionabilityTransparency :: Status -> Status -> Maybe Status
applyActionabilityTransparency c_ p_ = case (c_, p_) of
  -- A None parent is fully transparent
  (c, None) -> Just c
  -- A Next parent with a WIP child is itself WIP.
  -- This a little bit inconsistent but we typically *mean* this.
  -- Con: there's no way to pause a WIP task using a Next parent now. (until we implement a "force
  -- status" flag)
  (WIP, Next) -> Just WIP
  -- Projects are fully transparent
  (c, Project) -> Just c
  -- Opens are semi-transparent: they're transparent to Next etc. tasks but also stand on their own
  -- (likely project blockers). This is *different* from Project nodes, which are fully transparent!
  (c, Open) | c <= Open -> Just c
  _ -> Nothing

-- | 'applyActionabilityTransparency' with a fallback function to create a status. This is a common pattern.
applyActionabilityTransparencyFallback :: Status -> Status -> (Status -> Status -> Status) -> Status
applyActionabilityTransparencyFallback c p fallback = fromMaybe (fallback c p) $ applyActionabilityTransparency c p

-- | Actionability based on the node and its children, but ignoring its parents. This is globally
-- the same for all views.
--
-- This contains the logic to enable (partial) transparency for some statuses.
gGlobalActionability :: (HasAttr a, HasDerivedAttr a) => a -> Status
gGlobalActionability x = applyActionabilityTransparencyFallback (gChildActionability x) (gStatus x) ignore

-- | Actionability based on 'gGlobalActionability' and also its local parents. This is (or can be)
-- local to the view b/c it depends on which part of the tree is visible to the given view.
gLocalActionability :: (HasAttr a, HasDerivedAttr a, HasLocalDerivedAttr a) => a -> Status
gLocalActionability x = applyActionabilityTransparencyFallback (gGlobalActionability x) (gParentActionability x) max

-- | Implied or earliest child dates, whichever come earlier. This considers both parents and
-- children and is essentially "when this item has to be paid attention to", either directly or
-- because their children become relevant.
--
-- Note that, in contrast to actionability, these dates are always global.
gEarliestImpliedOrChildDates :: (HasDerivedAttr a) => TimeZone -> a -> AttrDates
gEarliestImpliedOrChildDates tz llabel = pointwiseChooseAttrDates chooseMin tz (gImpliedDates llabel) (gEarliestDates llabel)

-- * Business logic

-- | Whether the item is a stalled project, considering only its children.
--
-- Note: If we also consider the parent/ancestors, this project may ultimately not be actually
-- stalled b/c it's below another item that is not actionable (e.g. below a Someday). This is not
-- considered here. See 'isLocalStalledProject' for that.
--
-- See `docs/actionability.md` for discussion.
isGlobalStalledProject :: (HasAttr a, HasDerivedAttr a) => a -> Bool
isGlobalStalledProject label = gStatus label == Project && gGlobalActionability label > Waiting

-- | Whether the item is a stalled project, considering the whole current subtree.
--
-- SOMEDAY this needs to be kept in sync with 'sacForParent', which is kinda bad. Can we unify??
--
-- See `docs/actionability.md` for discussion.
isLocalStalledProject :: (HasAttr a, HasDerivedAttr a, HasLocalDerivedAttr a) => a -> Bool
isLocalStalledProject llabel =
  isGlobalStalledProject llabel
    && (gParentActionability llabel <= Project || gParentActionability llabel == None)
    -- The following ensures that the project actually blocks anything.
    -- See `docs/actionability.md` for discussion.
    && parentIsNotNextActionable
 where
  parentIsNotNextActionable = case gLocalParent llabel of
    Nothing -> True
    Just plilabel -> gLocalActionability plilabel > Next || gParentActionability llabel > Project
