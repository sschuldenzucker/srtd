{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Model stuff.
module Srtd.Model where

-- Really just a helper here. Should prob not import this for separation
import Brick (suffixLenses)
import Control.Applicative (asum, (<|>))
import Control.Monad ((<=<))
import Data.Aeson
import Data.Either (fromRight)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (find, foldl', minimumBy, sortBy, unfoldr)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Time (TimeZone)
import Data.Tree
import Data.Tree.Zipper (Empty, Full, TreePos)
import Data.Tree.Zipper qualified as Z
import Data.UUID (UUID)
import GHC.Generics
import GHC.List (uncons)
import Lens.Micro.Platform
import Srtd.Attr
import Srtd.Data.IdTree
import Srtd.Data.TreeZipper
import Srtd.Log
import Srtd.ModelJSON qualified as ModelJSON
import Srtd.Todo
import Srtd.Util (chooseMax, chooseMin, forEmptyList, mapForest)
import System.IO.Unsafe (unsafePerformIO)

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
      [ leaf (Inbox, unsafeAttrMinimal "INBOX"),
        leaf (Vault, unsafeAttrMinimal "VAULT")
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

leaf :: a -> Tree a
leaf x = Node x []

-- | Run forest modification function on the children of a tree.
onTreeChildren :: ([Tree a] -> [Tree a]) -> Tree a -> Tree a
onTreeChildren f (Node x children) = Node x (f children)

-- | Map forest modification function over each children of each tree. (go one level down)
onForestChildren :: ([Tree a] -> [Tree a]) -> [Tree a] -> [Tree a]
onForestChildren f = map (onTreeChildren f)

-- | All subtrees (with repetitions of children) with breadcrumbs (parents), in preorder.
forestTreesWithBreadcrumbs :: Forest a -> [([a], Tree a)]
forestTreesWithBreadcrumbs = concatMap (goTree [])
  where
    goTree crumbs n@(Node x children) = (crumbs, n) : concatMap (goTree (x : crumbs)) children

-- | Preorder nodes with their respecive levels
forestFlattenWithLevels :: Forest a -> [(Int, a)]
forestFlattenWithLevels = map extr . forestTreesWithBreadcrumbs
  where
    extr (crumbs, (Node x _)) = (length crumbs, x)

-- | Environment for updating the model, specifically for computing derived params. We pass this
-- around as an implicit parameter.
data ModelUpdateEnv = ModelUpdateEnv {mueTimeZone :: TimeZone}

-- For some reason, this has to be above `diskModelToModel`, otherwise it's not found.
_forestMakeDerivedAttrs :: (?mue :: ModelUpdateEnv) => IdForest EID Attr -> IdForest EID Label
_forestMakeDerivedAttrs = transformIdForestDownUpRec $ \mplabel clabels attr -> (attr, makeNodeDerivedAttr mplabel clabels attr)
  where
    -- TODO WIP re-write to use 'transformIdForestDownUp' and the "down" step is for updating 'daImpliedDates'.
    -- Requires some thought how exactly these should be implied from the parent (always inherit or base on status/actionability?)
    -- Also make this a test case for the structure of transformIdForestDownUp. Should it be more symmetic? Or can we hack it by making 'u' a closure? Or use a final mapping step? Add this to transformIdForestDownUp or separate fmap? Or mutual recursion after all?
    makeNodeDerivedAttr mplabel clabels attr =
      DerivedAttr
        { daChildActionability = forEmptyList None minimum . map glActionability $ clabels,
          daEarliestAutodates = foldl' (mapAttrAutoDates2 min) (autoDates attr) . map (daEarliestAutodates . snd) $ clabels,
          daLatestAutodates = foldl' (mapAttrAutoDates2 max) (autoDates attr) . map (daEarliestAutodates . snd) $ clabels,
          daEarliestDates = foldl' (pointwiseChooseAttrDates chooseMin tz) (dates attr) . map (daEarliestDates . snd) $ clabels,
          daLatestDates = foldl' (pointwiseChooseAttrDates chooseMax tz) (dates attr) . map (daLatestDates . snd) $ clabels,
          -- TODO should this actually *always* inherit from the parent? Or depend on status?
          -- Writing this as fold to match the above, but `mplabel` is just a Maybe, not a list.
          daImpliedDates = foldl' (pointwiseChooseAttrDates chooseMin tz) (dates attr) . fmap (daImpliedDates . snd) $ mplabel
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
  { breadcrumbs :: [IdLabel],
    root :: EID,
    rootLabel :: Label,
    stForest :: STForest
  }
  deriving (Show)

suffixLenses ''Subtree

data IdNotFoundError = IdNotFoundError deriving (Show)

filterSubtree :: (LocalLabel -> Bool) -> Subtree -> Subtree
filterSubtree p = stForestL %~ (filterIdForest p)

forestFindTreeWithBreadcrumbs :: (Eq id) => id -> IdForest id a -> Maybe ([(id, a)], Tree (id, a))
forestFindTreeWithBreadcrumbs tgt forest = find (\(_, Node (i, _) _) -> i == tgt) $ treesWithIdBreadcrumbs
  where
    -- Mogrify b/c forestTreesWithBreadcrumbs also returns the attrs, which we don't care about here.
    treesWithIdBreadcrumbs = forestTreesWithBreadcrumbs . idForest $ forest

-- | Add local derived attrs across a subtree.
--
-- SOMEDAY also do this for the root?
addLocalDerivedAttrs :: MForest -> STForest
addLocalDerivedAttrs = transformIdForestTopDown _go
  where
    _go Nothing label = (label, LocalDerivedAttr {ldParentActionability = None})
    _go (Just ((parAttr, _parDAttr), parLDAttr)) label =
      (label, LocalDerivedAttr {ldParentActionability = stepParentActionability (status parAttr) (ldParentActionability parLDAttr)})

    stepParentActionability :: Status -> Status -> Status
    stepParentActionability a pa = case (a, pa) of
      -- SOMEDAY this is a very ad-hoc solution. Think about the structure, also re the upwards derivation.
      -- Not super sure if this is the right way.
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

-- * Filters

-- | A Filter applies various operations like - uh - filtering, sorting, and restructuring to
-- a subtree.
--
-- SOMEDAY unclear if this is the right structure. Feels too general.
data Filter = Filter
  { filterName :: String,
    filterRun :: EID -> Model -> Subtree
  }

instance Show Filter where
  show f = "<Filter " ++ filterName f ++ ">"

-- ** Specific filters

-- | The identiy filter, returning its subtree unmodified.
--
-- SOMEDAY decide on error handling
f_identity :: Filter
f_identity = Filter "all" f
  where
    f i m = fromRight (error "root EID not found") $ modelGetSubtreeBelow i m

-- | Hide completed tasks, top-down
--
-- Note that all sub-items below a completed item are hidden. (this is usually desired)
f_hide_completed :: Filter
f_hide_completed =
  let (Filter _ fi) = f_identity
      fi' i m = filterSubtree p $ fi i m
   in Filter "not done" fi'
  where
    p ((Attr {status}, _), _) = status /= Done

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
insertNewNormalWithNewId :: (?mue :: ModelUpdateEnv) => UUID -> Attr -> EID -> InsertWalker IdLabel -> Model -> Model
insertNewNormalWithNewId uuid attr tgt go (Model forest) = updateDerivedAttrs $ Model forest'
  where
    -- NB 'emptyDerivedAttr' is immediately overwritten by 'updateDerivedAttrs'. It's just here so
    -- we can re-use code.
    forest' = forestInsertLabelRelToId tgt go (EIDNormal uuid) (attr, emptyDerivedAttr attr) forest

-- | Move the subtree below the given target to a new position. See 'forestMoveSubtreeRelFromForestId'.
moveSubtreeRelFromForest :: (?mue :: ModelUpdateEnv) => EID -> GoWalker (EID, a) -> InsertWalker IdLabel -> IdForest EID a -> Model -> Model
moveSubtreeRelFromForest tgt go ins haystack = updateDerivedAttrs . (forestL %~ (forestMoveSubtreeRelFromForestId tgt go ins haystack))

-- | Dynamic variant of 'moveSubtreeRelFromForest'.
moveSubtreeRelFromForestDynamic :: (?mue :: ModelUpdateEnv) => EID -> DynamicMoveWalker (EID, a) IdLabel -> IdForest EID a -> Model -> Model
moveSubtreeRelFromForestDynamic tgt dto haystack = updateDerivedAttrs . (forestL %~ (forestMoveSubtreeRelFromForestIdDynamic tgt dto haystack))

-- * Sorting (physical)

-- | Sort the subtree below the given target ID (only one level).
sortShallowBelow :: (?mue :: ModelUpdateEnv) => (Label -> Label -> Ordering) -> EID -> Model -> Model
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
