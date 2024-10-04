{-# LANGUAGE DeriveGeneric #-}
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
import Data.List (find, sortBy, unfoldr)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
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

-- import Data.UUID.V4 (nextRandom)

-- * Helper Types

type Label = (Attr, DerivedAttr)

type IdLabel = (EID, Label)

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
  DiskModel
    -- SOMEDAY this is a bad hack that points us to the fact that the "synthetic" elements should
    -- really be different from the rest.
    -- But I'm too attached to the nice rose trees & everything right now.
    -- We shouldn't make a special node type b/c in almost all relevant cases, a node will be a
    -- "normal" one. Perhaps we can restructure 'Model' so that the toplevel elements are not part
    -- of a tree. It would be a bit cumbersome, but maybe not too much.
    [ leaf (Inbox, unsafeAttrMinimal "INBOX"),
      leaf (Vault, unsafeAttrMinimal "VAULT")
    ]

-- We do *not* use the generic JSON instance b/c the ToJSON instance of Tree (provided by aeson)
-- makes for kinda messy JSON. It encodes the whole thing as a list (not an object). While we're
-- at it, we also transform the presentation of attr and id. The whole thing is a bit slow b/c we
-- transmogrify the whole structure.

diskModelToJSONModel :: DiskModel -> ModelJSON.Model
diskModelToJSONModel (DiskModel forest) = ModelJSON.Model forestJSON
  where
    forestJSON = map treeToJSONTree forest
    treeToJSONTree = foldTree $ \(i, attr) children -> ModelJSON.Tree i attr children

diskModelFromJSONModel :: ModelJSON.Model -> DiskModel
diskModelFromJSONModel (ModelJSON.Model forestJSON) = DiskModel forest
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

-- | The sane `fmap` instance. (the default is the list instance, which isn't normally desired.)
--
-- Helper.
mapForest :: (a -> b) -> Forest a -> Forest b
mapForest f = map (fmap f)

-- | Run forest modification function on the children of a tree.
onTreeChildren :: ([Tree a] -> [Tree a]) -> Tree a -> Tree a
onTreeChildren f (Node x children) = Node x (f children)

-- | Map forest modification function over each children of each tree. (go one level down)
onForestChildren :: ([Tree a] -> [Tree a]) -> [Tree a] -> [Tree a]
onForestChildren f = map (onTreeChildren f)

-- | Only leaves the initial segments of the forest where the predicate all applies.
filterForest :: (a -> Bool) -> Forest a -> Forest a
filterForest p forest = [Node x (filterForest p children) | Node x children <- forest, p x]

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

-- TODO actually compute derived attrs
diskModelToModel :: DiskModel -> Model
diskModelToModel (DiskModel forest) = Model (mapForest (\(i, attr) -> (i, (attr, DerivedAttr))) forest)

-- Forgets its derived attrs
modelToDiskModel :: Model -> DiskModel
modelToDiskModel (Model forest) = DiskModel (mapForest (\(i, (attr, _)) -> (i, attr)) forest)

-- * Subtrees

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
    stForest :: MForest
  }
  deriving (Show)

suffixLenses ''Subtree

data IdNotFoundError = IdNotFoundError deriving (Show)

stFilter :: (IdLabel -> Bool) -> Subtree -> Subtree
stFilter p = stForestL %~ (filterForest p)

forestFindTreeWithBreadcrumbs :: (Eq id) => id -> IdForest id a -> Maybe ([(id, a)], Tree (id, a))
forestFindTreeWithBreadcrumbs tgt forest = find (\(_, Node (i, _) _) -> i == tgt) $ treesWithIdBreadcrumbs
  where
    -- Mogrify b/c forestTreesWithBreadcrumbs also returns the attrs, which we don't care about here.
    treesWithIdBreadcrumbs = forestTreesWithBreadcrumbs forest

forestGetSubtreeBelow :: EID -> MForest -> Either IdNotFoundError Subtree
forestGetSubtreeBelow tgt forest = case forestFindTreeWithBreadcrumbs tgt forest of
  Just (crumbs, (Node (i, attr) cs)) -> Right (Subtree crumbs i attr cs)
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
      fi' i m = stFilter p $ fi i m
   in Filter "not done" fi'
  where
    p (_, (Attr {status}, _)) = status /= Just Done

-- * Model modifications

-- SOMEDAY Implemente Undo. Major restructuring probably.

-- | Update an item's attr.
--
-- TODO update derived attrs. This is the case for many many places here.
modifyAttrByEID :: EID -> (Attr -> Attr) -> Model -> Model
modifyAttrByEID tgt f = forestL %~ map (fmap updateContent)
  where
    updateContent (eid, (attr, dattr)) = (eid, (if eid == tgt then f attr else attr, dattr))

-- | Delete the given ID and the subtree below it.
deleteSubtree :: EID -> Model -> Model
deleteSubtree eid (Model forest) = Model (filterForest (\(eid', _) -> eid' /= eid) forest)

-- | Insert new node relative to a target using the given 'InsertWalker'.
--
-- The caller needs to make sure that the provided uuid is actually new, probably using `UUID.nextRandom`.
insertNewNormalWithNewId :: UUID -> Attr -> EID -> InsertWalker IdLabel -> Model -> Model
insertNewNormalWithNewId uuid attr tgt go (Model forest) = Model forest'
  where
    -- TODO actually derive something (DerivedAttr is dummy): Everything needs to update (in general)
    forest' = forestInsertLabelRelToId tgt go (EIDNormal uuid) (attr, DerivedAttr) forest

-- | Move the subtree below the given target to a new position. See 'forestMoveSubtreeRelFromForestId'.
-- TODO update derived values
moveSubtreeRelFromForest :: EID -> GoWalker (EID, a) -> InsertWalker IdLabel -> Forest (EID, a) -> Model -> Model
moveSubtreeRelFromForest tgt go ins haystack = forestL %~ (forestMoveSubtreeRelFromForestId tgt go ins haystack)

-- * Sorting (physical)

-- | Sort the subtree below the given target ID (only one level).
sortShallowBelow :: (Label -> Label -> Ordering) -> EID -> Model -> Model
sortShallowBelow ord root = forestL %~ onForestBelowId root (sortBy ord')
  where
    ord' (Node (_, attr1) _) (Node (_, attr2) _) = ord attr1 attr2

-- | Sort the subtree below the given target ID, recursive at all levels
sortDeepBelow :: (Label -> Label -> Ordering) -> EID -> Model -> Model
sortDeepBelow ord root = forestL %~ onForestBelowId root deepSortByOrd
  where
    -- prob kinda inefficient but I got <= 10 levels or something.
    deepSortByOrd = onForestChildren deepSortByOrd . sortBy ord'
    ord' (Node (_, attr1) _) (Node (_, attr2) _) = ord attr1 attr2
