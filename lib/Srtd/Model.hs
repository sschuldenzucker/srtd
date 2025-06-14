-- | Model stuff.
module Srtd.Model where

-- Really just a helper here. Should prob not import this for separation
import Brick (suffixLenses)
import Data.Aeson
import Data.Function (on)
import Data.List (find, foldl', sortBy)
import Data.Ord (comparing)
import Data.Time (TimeZone)
import Data.Tree
import Data.UUID (UUID)
import GHC.Generics
import Lens.Micro.Platform
import Srtd.Attr
import Srtd.Data.IdTree
import Srtd.Data.TreeZipper
import Srtd.ModelJSON qualified as ModelJSON
import Srtd.Todo
import Srtd.Util (
  chooseMax,
  chooseMin,
  forEmptyList,
  forestFlatten,
  forestFlattenPostorder,
  forestTreesWithBreadcrumbs,
  leaf,
  mapForest,
  onForestChildren,
  transformForestTopDown,
 )

-- import Data.UUID.V4 (nextRandom)

-- * Helper Types

type MForest = IdForest EID Label

-- * Fundamental Data Structures

-- | In-memory model
data Model = Model
  { forest :: MForest
  }
  deriving (Show, Generic)

suffixLenses ''Model

-- | Model that's written to disk. This does not include derived attrs.
data DiskModel = DiskModel
  { dmForest :: IdForest EID Attr
  }

suffixLenses ''DiskModel

-- | Empty model with only the required toplevel entries.
emptyDiskModel :: DiskModel
emptyDiskModel =
  DiskModel $
    -- SOMEDAY this is a bad hack that points us to the fact that the "synthetic" elements should
    -- really be different from the rest.
    -- But I'm too attached to the nice rose trees & everything right now.
    -- We shouldn't make a special node type b/c in almost all relevant cases, a node will be a
    -- "normal" one. Perhaps we can restructure 'Model' so that the toplevel elements are not part
    -- of a tree. It would be a bit cumbersome, but maybe not too much.
    IdForest $
      [ leaf (Inbox, unsafeAttrMinimal "INBOX")
      , leaf (Vault, unsafeAttrMinimal "VAULT")
      ]

-- We do *not* use the generic JSON instance b/c the ToJSON instance of Tree (provided by aeson)
-- makes for kinda messy JSON. It encodes the whole thing as a list (not an object). While we're
-- at it, we also transform the presentation of attr and id. The whole thing is a bit slow b/c we
-- transmogrify the whole structure.

diskModelToJSONModel :: DiskModel -> ModelJSON.Model
diskModelToJSONModel (DiskModel (IdForest forest)) = ModelJSON.Model forestJSON
 where
  forestJSON = map treeToJSONTree forest
  treeToJSONTree = foldTree $ \(i, attr) children -> ModelJSON.Tree i attr children

diskModelFromJSONModel :: ModelJSON.Model -> DiskModel
diskModelFromJSONModel (ModelJSON.Model forestJSON) = DiskModel $ IdForest forest
 where
  forest = jsonForestToForest forestJSON
  jsonForestToForest = unfoldForest $ \(ModelJSON.Tree i attr children) -> ((i, attr), children)

instance ToJSON DiskModel where
  toJSON = toJSON . diskModelToJSONModel
  toEncoding = toEncoding . diskModelToJSONModel

instance FromJSON DiskModel where
  parseJSON = fmap diskModelFromJSONModel . parseJSON

-- * Generic Forest Helpers

-- | Environment for updating the model, specifically for computing derived params. We pass this
-- around as an implicit parameter.
--
-- SOMEDAY rename to ModelUpdateContext, ?mux
data ModelUpdateEnv = ModelUpdateEnv {mueTimeZone :: TimeZone}

-- For some reason, this has to be above `diskModelToModel`, otherwise it's not found.
_forestMakeDerivedAttrs :: (?mue :: ModelUpdateEnv) => IdForest EID Attr -> IdForest EID Label
_forestMakeDerivedAttrs = transformIdForestDownUpRec $ \mplabel clabels attr -> (attr, makeNodeDerivedAttr mplabel clabels attr)
 where
  makeNodeDerivedAttr mplabel clabels attr =
    DerivedAttr
      { daChildActionability = forEmptyList None minimum . map gGlobalActionability $ clabels
      , daEarliestAutodates =
          foldl' (mapAttrAutoDates2 min) (autoDates attr) . map (daEarliestAutodates . snd) $ clabels
      , daLatestAutodates =
          foldl' (mapAttrAutoDates2 max) (autoDates attr) . map (daEarliestAutodates . snd) $ clabels
      , daEarliestDates =
          foldl'
            (pointwiseChooseAttrDates chooseMin tz)
            (dates attr)
            [daEarliestDates d | (a, d) <- clabels, not (isDone $ status a)]
      , daLatestDates =
          foldl'
            (pointwiseChooseAttrDates chooseMax tz)
            (dates attr)
            [daLatestDates d | (a, d) <- clabels, not (isDone $ status a)]
      , -- Writing this as fold to match the above, but `mplabel` is just a Maybe, not a list.
        daImpliedDates =
          foldl' (pointwiseChooseAttrDates chooseMin tz) (dates attr) . fmap (daImpliedDates . snd) $ mplabel
      }
  tz = mueTimeZone ?mue

diskModelToModel :: (?mue :: ModelUpdateEnv) => DiskModel -> Model
diskModelToModel (DiskModel forest) = Model (_forestMakeDerivedAttrs forest)

-- Forgets its derived attrs
modelToDiskModel :: Model -> DiskModel
modelToDiskModel (Model (IdForest forest)) = DiskModel $ IdForest (mapForest (\(i, (attr, _)) -> (i, attr)) forest)

-- * Subtrees

type STForest = IdForest EID LocalLabel

-- | A subtree is an - uh - subtree of the model with info on the root and breadcrumbs. It's somewhat
-- like a zipper but without navigation or modification.
--
-- This is read-only. For all operations that would modify / navigate, we use IDs.
--
-- SOMEDAY can be generalized to label and id types and moved to IdForest. (and called IdSubtree)
data Subtree = Subtree
  -- SOMEDAY we compute breadcrumbs twice: here for the root and in ldBreadcrumbs for children.
  -- That's not wrong but maybe we can unify code?
  { breadcrumbs :: [IdLabel]
  , root :: EID
  , rootLabel :: Label
  , stForest :: STForest
  }
  deriving (Show)

suffixLenses ''Subtree

data IdNotFoundError = IdNotFoundError deriving (Show)

filterSubtree :: (LocalLabel -> Bool) -> Subtree -> Subtree
filterSubtree p = stForestL %~ (filterIdForest p)

flattenSubtreePreorder :: Subtree -> Subtree
flattenSubtreePreorder = stForestL %~ (withIdForest $ forestFlatten)

-- TODO we may not need this anymore
forestFindTreeWithBreadcrumbs :: (Eq id) => id -> IdForest id a -> Maybe ([(id, a)], Tree (id, a))
forestFindTreeWithBreadcrumbs tgt forest = find (\(_, Node (i, _) _) -> i == tgt) $ treesWithIdBreadcrumbs
 where
  -- Mogrify b/c forestTreesWithBreadcrumbs also returns the attrs, which we don't care about here.
  treesWithIdBreadcrumbs = forestTreesWithBreadcrumbs . idForest $ forest

-- | Add local derived attrs across a subtree.
--
-- SOMEDAY also do this for the root?
addLocalDerivedAttrs :: MForest -> STForest
addLocalDerivedAttrs = withIdForest $ transformForestTopDown _go
 where
  _go Nothing (i, label) = (i, (label, LocalDerivedAttr {ldParentActionability = None, ldBreadcrumbs = [], ldLevel = 0}))
  _go (Just plilabel@(_i, (plabel@(_parAttr, _parDAttr), parLDAttr))) (i, label) =
    ( i
    ,
      ( label
      , LocalDerivedAttr
          { ldParentActionability =
              stepParentActionability (gGlobalActionability plabel) (ldParentActionability parLDAttr)
          , ldBreadcrumbs = plilabel : ldBreadcrumbs parLDAttr
          , ldLevel = ldLevel parLDAttr + 1
          }
      )
    )

  stepParentActionability :: Status -> Status -> Status
  stepParentActionability a_ pa_ = case (a_, pa_) of
    -- SOMEDAY this is a very ad-hoc solution. Think about the structure, also re the upwards derivation.
    -- Not super sure if this is the right way.
    -- NB it's also kinda inconsistent with glActionability and stuff. Maybe we wanna step against glActionability, not status?
    (a, None) -> a
    (None, pa) -> pa
    (a, Project) -> a
    (a, ap) -> max a ap

-- stepParentActionability st pst = case (st, pst) of
--   (st, None) -> st
-- stepParentActionability st None = st
-- stepParentActionability None pst = pst
-- stepParentActionability st pst = max st pst

forestGetSubtreeBelow :: EID -> MForest -> Either IdNotFoundError Subtree
forestGetSubtreeBelow tgt forest = case forestFindTreeWithBreadcrumbs tgt forest of
  Just (crumbs, (Node (i, attr) cs)) -> Right (Subtree crumbs i attr (addLocalDerivedAttrs $ IdForest cs))
  Nothing -> Left IdNotFoundError

modelGetSubtreeBelow :: EID -> Model -> Either IdNotFoundError Subtree
modelGetSubtreeBelow i (Model forest) = forestGetSubtreeBelow i forest

-- | Given a subtree and a LocalLabel that is part of that forest, get the parent. Otherwise, returns a wrong result.
stParentEID :: (HasLocalDerivedAttr a) => Subtree -> a -> EID
stParentEID st llabel = case gBreadcrumbs llabel of
  ((i, _) : _) -> i
  [] -> root st

-- * Filters

-- | Context for executing a filter.
--
-- SOMEDAY This is the same as ModelUpdateEnv but has a slightly different role (it's further away
-- from the ModelServer). If we pull them even further apart (e.g. going async somewhat), we may want
-- to review.
data FilterContext = FilterContext
  { fcTimeZone :: TimeZone
  }

-- | A Filter applies various operations like - uh - filtering, sorting, and restructuring to
-- a subtree.
data Filter = Filter
  { fiName :: String
  -- ^ Name to be displayed in the UI
  , fiIncludeDone :: Bool
  -- ^ Whether to include Done items. If False, we remove Done subtrees before deriving local attrs
  -- and before applying 'fiPostprocess'.
  --
  -- These could be filtered out in 'fiPostprocess' but it's such a common thing to do that we have
  -- a field for them (and also better performance to filter early)
  , fiPostprocess :: (?fctx :: FilterContext) => STForest -> STForest
  -- ^ Postprocessing function that can apply pretty much any transformation.
  }

instance Show Filter where
  show f = "<Filter " ++ fiName f ++ ">"

runFilter :: (?fctx :: FilterContext) => Filter -> EID -> Model -> Either IdNotFoundError Subtree
runFilter (Filter {fiIncludeDone, fiPostprocess}) i m = do
  st0 <- modelGetSubtreeBelow i m
  let
    -- TODO apply fiIncludeDone *before* deriving local attrs!
    st1 = (if fiIncludeDone then id else filterSubtree pNotDone) st0
    st2 = (stForestL %~ fiPostprocess) $ st1
  return st2
 where
  pNotDone = not . isDone . gStatus

-- ** Specific filters

-- | Returns the full subtree without modification.
f_all :: Filter
f_all =
  Filter
    { fiName = "all"
    , fiIncludeDone = True
    , fiPostprocess = id
    }

-- | Hide completed subtrees, top-down
--
-- Note that all sub-items below a completed item are hidden. (this is usually desired)
f_notDone :: Filter
f_notDone =
  Filter
    { fiName = "not done"
    , fiIncludeDone = False
    , fiPostprocess = id
    }

-- | Flat list of all non-done child nodes, ordered by dates then actionability
f_flatByDates :: Filter
f_flatByDates =
  Filter
    { fiName = "flat, by simple urgency"
    , fiIncludeDone = False
    , fiPostprocess = go
    }
 where
  -- Need to mention the implicit param explicitly here for some reason, o/w it's not seeing it.
  go :: (?fctx :: FilterContext) => STForest -> STForest
  -- TODO also, we should show implied dates, especially in this view. (maybe only this one)
  -- TODO also, refactor the rendering code for dates.
  -- TODO the secondary sort should be by (actionability, status) so that Next is sorted before Project I think. (and Waiting??)
  -- (projects that are ready have Next actionability, which is good, but not the whole story)
  -- (sorting kinda unclear here; should project actually be less actionable than waiting?)
  -- Using postorder so that parents show up below their children, which is typically what you want for "urgency".
  go = sortIdForestBy cmp False . filterIdForest p . withIdForest forestFlattenPostorder
   where
    -- We filter out non-actionable items here that are otherwise kinda in the way. We leave them
    -- in the (unlikely) case that they have a date themselves, to be sure we're not missing anything
    -- important.
    -- NB this filter could be even more extreme I think. Any amount of filtering we want really.
    p llabel = gStatus llabel /= None || not (isDatesEmpty $ gDates llabel)
    cmp llabel1 llabel2 =
      mconcat
        [ (compareAttrDates tz `on` gImpliedDates) llabel1 llabel2
        , comparing gLocalActionability llabel1 llabel2
        ]
    tz = fcTimeZone ?fctx

-- | Flat list of next and WIP tasks, ordered by dates. This is a simple "do mode".
f_nextFlatByDates :: Filter
f_nextFlatByDates =
  Filter
    { fiName = "flat next, by simple urgency"
    , fiIncludeDone = False
    , fiPostprocess = go
    }
 where
  -- SOMEDAY there's quite some overlap between the different filters, maybe make top-level helpers.
  -- But not quite yet, we'll wanna restructure.
  go :: (?fctx :: FilterContext) => STForest -> STForest
  go = sortIdForestBy cmp False . filterIdForest p . withIdForest forestFlattenPostorder
   where
    -- SOMEDAY this combo tells me some concept is missing / there's an overlap of concepts.
    p llabel = gStatus llabel <= Next && gLocalActionability llabel <= Next
    cmp llabel1 llabel2 =
      mconcat
        -- NB here we list WIP tasks first b/c it just makes sense.
        -- SOMEDAY do this for the other filters, too.
        [ comparing gLocalActionability llabel1 llabel2
        , (compareAttrDates tz `on` gImpliedDates) llabel1 llabel2
        ]
    tz = fcTimeZone ?fctx

-- | Flat list of next tasks, ordered by dates. This is a simple "do mode".
f_waitingFlatByDates :: Filter
f_waitingFlatByDates =
  Filter
    { fiName = "flat waiting, by simple urgency"
    , fiIncludeDone = False
    , fiPostprocess = go
    }
 where
  -- SOMEDAY there's quite some overlap between the different filters, maybe make top-level helpers.
  -- But not quite yet, we'll wanna restructure.
  go :: (?fctx :: FilterContext) => STForest -> STForest
  go = sortIdForestBy cmp False . filterIdForest p . withIdForest forestFlattenPostorder
   where
    p llabel = gStatus llabel == Waiting && gLocalActionability llabel <= Waiting
    cmp llabel1 llabel2 =
      mconcat
        [ (compareAttrDates tz `on` gImpliedDates) llabel1 llabel2
        -- , comparing gLocalActionability llabel1 llabel2
        ]
    tz = fcTimeZone ?fctx

-- | Hierarchy-preserving non-done child nodes but deep-sort like `f_flatByDates`
f_deepByDates :: Filter
f_deepByDates =
  Filter
    { fiName = "by simple urgency"
    , fiIncludeDone = False
    , fiPostprocess = go
    }
 where
  -- Need to mention the implicit param explicitly here for some reason, o/w it's not seeing it.
  go :: (?fctx :: FilterContext) => STForest -> STForest
  go = sortIdForestBy cmp True
   where
    cmp llabel1 llabel2 =
      mconcat
        [ (compareAttrDates tz `on` gEarliestImpliedOrChildDates tz) llabel1 llabel2
        , comparing gLocalActionability llabel1 llabel2
        ]
    tz = fcTimeZone ?fctx

-- * Model modifications

-- SOMEDAY Implemente Undo. Major restructuring probably.

-- | Update derived attrs for the whole model.
--
-- Currently a dummy.
--
-- SOMEDAY this is currently used in all modifying functions, which is quite expensive and not necessary.
updateDerivedAttrs :: (?mue :: ModelUpdateEnv) => Model -> Model
updateDerivedAttrs = forestL %~ (_forestMakeDerivedAttrs . fmap fst)

-- | Update an item's attr.
modifyAttrByEID :: (?mue :: ModelUpdateEnv) => EID -> (Attr -> Attr) -> Model -> Model
modifyAttrByEID tgt f = updateDerivedAttrs . (forestL %~ mapIdForestWithIds updateContent)
 where
  updateContent eid (attr, dattr) = (if eid == tgt then f attr else attr, dattr)

-- | Delete the given ID and the subtree below it.
deleteSubtree :: (?mue :: ModelUpdateEnv) => EID -> Model -> Model
deleteSubtree eid = updateDerivedAttrs . (forestL %~ filterIdForestWithIds (\eid' _ -> eid' /= eid))

-- | Insert new node relative to a target using the given 'InsertWalker'.
--
-- The caller needs to make sure that the provided uuid is actually new, probably using `UUID.nextRandom`.
insertNewNormalWithNewId ::
  (?mue :: ModelUpdateEnv) => UUID -> Attr -> EID -> InsertWalker IdLabel -> Model -> Model
insertNewNormalWithNewId uuid attr tgt go (Model forest) = updateDerivedAttrs $ Model forest'
 where
  -- NB 'emptyDerivedAttr' is immediately overwritten by 'updateDerivedAttrs'. It's just here so
  -- we can re-use code.
  forest' = forestInsertLabelRelToId tgt go (EIDNormal uuid) (attr, emptyDerivedAttr attr) forest

-- | Move the subtree below the given target to a new position. See 'forestMoveSubtreeRelFromForestId'.
moveSubtreeRelFromForest ::
  (?mue :: ModelUpdateEnv) =>
  EID ->
  GoWalker (EID, a) ->
  InsertWalker IdLabel ->
  IdForest EID a ->
  Model ->
  Model
moveSubtreeRelFromForest tgt go ins haystack = updateDerivedAttrs . (forestL %~ (forestMoveSubtreeRelFromForestId tgt go ins haystack))

-- | Dynamic variant of 'moveSubtreeRelFromForest'.
moveSubtreeRelFromForestDynamic ::
  (?mue :: ModelUpdateEnv) =>
  EID ->
  DynamicMoveWalker (EID, a) IdLabel ->
  IdForest EID a ->
  Model ->
  Model
moveSubtreeRelFromForestDynamic tgt dto haystack = updateDerivedAttrs . (forestL %~ (forestMoveSubtreeRelFromForestIdDynamic tgt dto haystack))

-- * Sorting (physical)

-- | Sort the subtree below the given target ID (only one level).
sortShallowBelow ::
  (?mue :: ModelUpdateEnv) => (Label -> Label -> Ordering) -> EID -> Model -> Model
sortShallowBelow ord root = updateDerivedAttrs . (forestL %~ onForestBelowId root (sortBy ord'))
 where
  ord' (Node (_, attr1) _) (Node (_, attr2) _) = ord attr1 attr2

-- | Sort the subtree below the given target ID, recursive at all levels
sortDeepBelow :: (?mue :: ModelUpdateEnv) => (Label -> Label -> Ordering) -> EID -> Model -> Model
sortDeepBelow ord root = updateDerivedAttrs . (forestL %~ onForestBelowId root deepSortByOrd)
 where
  -- prob kinda inefficient but I got <= 10 levels or something.
  deepSortByOrd = onForestChildren deepSortByOrd . sortBy ord'
  ord' (Node (_, attr1) _) (Node (_, attr2) _) = ord attr1 attr2
