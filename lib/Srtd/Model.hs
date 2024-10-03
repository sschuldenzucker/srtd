{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Model stuff.
--
-- SOMEDAY this model has grown very large and should be split into sub-aspects.
-- Most importantly, tree modification operations should be split out.
module Srtd.Model where

-- Really just a helper here. Should prob not import this for separation
import Brick (suffixLenses)
import Control.Applicative (asum, (<|>))
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

leaf :: a -> Tree a
leaf x = Node x []

type Label = (Attr, DerivedAttr)

type IdLabel = (EID, Label)

type MForest = IdForest EID Label

data Model = Model
  { forest :: MForest
  }
  deriving (Show, Generic)

suffixLenses ''Model

data DiskModel = DiskModel
  { dmForest :: IdForest EID Attr
  }

suffixLenses ''DiskModel

-- We do *not* use the generic JSON instance b/c the ToJSON instance of Tree (provided by aeson)
-- makes for kinda messy JSON. It encodes the whole thing as a list (not an object). While we're
-- at it, we also transform the presentation of attr and id. The whole thing is a bit slow b/c we
-- transmogrify the whole structure.

-- TODO these should now be for the `DiskModel`, not `Model`.

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

-- | The sane `fmap` instance. (the default is the list instance, which isn't normally desired.)
--
-- Helper.
mapForest :: (a -> b) -> Forest a -> Forest b
mapForest f = map (fmap f)

-- TODO actually compute derived attrs
diskModelToModel :: DiskModel -> Model
diskModelToModel (DiskModel forest) = Model (mapForest (\(i, attr) -> (i, (attr, DerivedAttr))) forest)

-- Forgets its derived attrs
modelToDiskModel :: Model -> DiskModel
modelToDiskModel (Model forest) = DiskModel (mapForest (\(i, (attr, _)) -> (i, attr)) forest)

splitFind :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
splitFind p xs = case break p xs of
  (before, x : after) -> Just (before, x, after)
  _ -> Nothing

treeHasID :: (Eq id) => id -> Tree (id, a) -> Bool
treeHasID tgt (Node (i, _) _) = tgt == i

onTreeChildren :: ([Tree a] -> [Tree a]) -> Tree a -> Tree a
onTreeChildren f (Node x children) = Node x (f children)

onForestChildren :: ([Tree a] -> [Tree a]) -> [Tree a] -> [Tree a]
onForestChildren f = map (onTreeChildren f)

splitLast :: [a] -> ([a], a)
splitLast xs = case uncons (reverse xs) of
  Just (lst, revinit) -> (reverse revinit, lst)
  Nothing -> error "splitLast: empty list"

deleteSubtree :: EID -> Model -> Model
deleteSubtree eid (Model forest) = Model (filterForest (\(eid', _) -> eid' /= eid) forest)

-- | Only leaves the initial segments of the forest where the predicate all applies.
filterForest :: (a -> Bool) -> Forest a -> Forest a
filterForest p forest = [Node x (filterForest p children) | Node x children <- forest, p x]

-- TODO update derived attrs. This is the case for many many places here.
modifyAttrByEID :: EID -> (Attr -> Attr) -> Model -> Model
modifyAttrByEID tgt f = forestL %~ map (fmap updateContent)
  where
    updateContent (eid, (attr, dattr)) = (eid, (if eid == tgt then f attr else attr, dattr))

data Subtree = Subtree
  { breadcrumbs :: [IdLabel],
    root :: EID,
    rootLabel :: Label,
    stForest :: MForest
  }
  deriving (Show)

suffixLenses ''Subtree

data IdNotFoundError = IdNotFoundError deriving (Show)

modelGetSubtreeBelow :: EID -> Model -> Either IdNotFoundError Subtree
modelGetSubtreeBelow i (Model forest) = forestGetSubtreeBelow i forest

-- SOMEDAY some of the following could probably be further abstracted away by supplying a condition on either the tree labels or subtrees themselves.

-- | All subtrees (with repetitions of children) with breadcrumbs (parents), in preorder.
forestTrees :: Forest a -> [([a], Tree a)]
forestTrees = concatMap (goTree [])
  where
    goTree crumbs n@(Node x children) = (crumbs, n) : concatMap (goTree (x : crumbs)) children

-- | Preorder nodes with their respecive levels
forestFlattenWithLevels :: Forest a -> [(Int, a)]
forestFlattenWithLevels = map extr . forestTrees
  where
    extr (crumbs, (Node x _)) = (length crumbs, x)

-- TODO move these to IdForest.
forestFindTree :: (Eq id) => id -> IdForest id a -> Maybe ([(id, a)], Tree (id, a))
forestFindTree tgt forest = find (\(_, Node (i, _) _) -> i == tgt) $ treesWithIdBreadcrumbs
  where
    -- Mogrify b/c forestTrees also returns the attrs, which we don't care about here.
    treesWithIdBreadcrumbs = forestTrees forest

forestGetSubtreeBelow :: EID -> MForest -> Either IdNotFoundError Subtree
forestGetSubtreeBelow tgt forest = case forestFindTree tgt forest of
  Just (crumbs, (Node (i, attr) cs)) -> Right (Subtree crumbs i attr cs)
  Nothing -> Left IdNotFoundError

-- SOMEDAY kinda inefficient, but everything here is, so w/e. Could also use forestTrees but, again, w/e.
forestGetParentsWhere :: (a -> Bool) -> Forest a -> [a]
forestGetParentsWhere p = mapMaybe trans . forestTrees
  where
    trans (par : _, (Node x _)) | p x = Just par
    trans _ = Nothing

listToMaybeId :: [(id, a)] -> Maybe id
listToMaybeId [(i, _)] = Just i
listToMaybeId _ = Nothing

forestGetParentId :: (Eq id) => id -> Forest (id, a) -> Maybe id
forestGetParentId tgt = listToMaybeId . forestGetParentsWhere (\(i, _) -> i == tgt)

forestGetNextSiblingId :: (Eq id) => id -> Forest (id, a) -> Maybe id
forestGetNextSiblingId tgt forest = case splitFind (treeHasID tgt) forest of
  Just (_, _, (Node (ret, _) _) : _) -> Just ret
  Just _ -> Nothing
  Nothing -> asum [forestGetNextSiblingId tgt cs | (Node _ cs) <- forest]

forestGetPrevSiblingId :: (Eq id) => id -> Forest (id, a) -> Maybe id
forestGetPrevSiblingId tgt forest = case splitFind (treeHasID tgt) forest of
  Just (prevs@(_ : _), _, _) -> let (Node (ret, _) _) = last prevs in Just ret
  Just _ -> Nothing
  Nothing -> asum [forestGetPrevSiblingId tgt cs | (Node _ cs) <- forest]

-- --------------------
-- Filter infra
-- --------------------

-- SOMEDAY unclear if this is the right structure. Feels too general.
data Filter = Filter
  { filterName :: String,
    filterRun :: EID -> Model -> Subtree
  }

-- SOMEDAY maybe these should have a name actually. Also for display.
instance Show Filter where
  show f = "<Filter " ++ filterName f ++ ">"

-- SOMEDAY decide on error handling
f_identity :: Filter
f_identity = Filter "all" f
  where
    f i m = fromRight (error "root EID not found") $ modelGetSubtreeBelow i m

-- | Hide completed tasks, top-down
--
-- TODO This is very simplistic: A non-completed task below a completed one is not shown (even though it seems important)
f_hide_completed :: Filter
f_hide_completed =
  let (Filter _ fi) = f_identity
      fi' i m = stFilter p $ fi i m
   in Filter "not done" fi'
  where
    p (_, (Attr {status}, _)) = status /= Just Done

stFilter :: (IdLabel -> Bool) -> Subtree -> Subtree
stFilter p = stForestL %~ (filterForest p)

-- --------------------
-- Forest modifications
-- --------------------

-- TODO WIP Remove everything above that we don't need (or quickly rewrite).
-- Then, make it work with the change we started to (Attr, DerivedAttr) instead of just Attr.
-- Then or while doing that, derive the DerivedAttr (dummy for now).

-- | *You* need to make sure that the provided uuid is actually new, probably using UUID.nextRandom.
insertNewNormalWithNewId :: UUID -> Attr -> EID -> InsertWalker IdLabel -> Model -> Model
insertNewNormalWithNewId uuid attr tgt go (Model forest) = Model forest'
  where
    -- TODO actually derive something (DerivedAttr is dummy): Everything needs to update (in general)
    forest' = forestInsertLabelRelToId tgt go (EIDNormal uuid) (attr, DerivedAttr) forest

-- Model (forestInsertAtId loc (EIDNormal uuid, attr) forest)

-- TODO The following was used but created problems. Rewrite calling code, then delete.
-- moveSubtreeBelow' :: EID -> EID -> MoveWalker Label -> Model -> Model
-- moveSubtreeBelow' root tgt go = forestL %~ (forestMoveSubtreeIdBelow' root tgt go)

-- TODO Delete

-- | Does nothing if the EID is not found.
-- forestMoveSubtreeId' :: (Eq id) => id -> MoveWalker (id, a) -> Forest (id, a) -> Forest (id, a)
-- forestMoveSubtreeId' tgt go forest = case zFindIdFirst tgt . Z.fromForest $ forest of
--   Nothing -> forest
--   Just loc -> zFullForest $ mzMove go loc

-- | Like forestMoveSubtreeId' but only consider the subtree below `root`.
-- forestMoveSubtreeIdBelow' :: (Eq id) => id -> id -> MoveWalker (id, a) -> Forest (id, a) -> Forest (id, a)
-- forestMoveSubtreeIdBelow' root tgt go forest =
--   -- SOMEDAY not really a deep reason to use zippers here other than that it's easy.
--   case zFindIdFirst root . Z.fromForest $ forest of
--     Nothing -> forest
--     Just rootLoc -> zFullForest . Z.modifyTree (onTreeChildren $ forestMoveSubtreeId' tgt go) $ rootLoc

-- -- SOMEDAY may be obsolete, or can be simplified (actually vaporized) using ZForestRoot.
-- class ZFullForest t where
--   zFullForest :: TreePos t a -> Forest a

-- instance ZFullForest Full where
--   zFullForest = Z.forest . Z.first . Z.prevSpace . Z.root

-- instance ZFullForest Empty where
--   zFullForest eloc = case Z.parent eloc of
--     Just loc -> zFullForest loc
--     Nothing -> Z.forest . Z.first $ eloc

-- TODO clean up the above manual code which should then be unused.
-- TODO also implement deletion using this infra.

-- | See `forestMoveSubtreeRelFromForestId`
-- TODO update derived values
moveSubtreeRelFromForest :: EID -> GoWalker (EID, a) -> InsertWalker IdLabel -> Forest (EID, a) -> Model -> Model
moveSubtreeRelFromForest tgt go ins haystack = forestL %~ (forestMoveSubtreeRelFromForestId tgt go ins haystack)

-- ------------------
-- Sorting (physical)
-- ------------------

sortShallowBelow :: (Label -> Label -> Ordering) -> EID -> Model -> Model
sortShallowBelow ord root = forestL %~ onForestBelowId root (sortBy ord')
  where
    ord' (Node (_, attr1) _) (Node (_, attr2) _) = ord attr1 attr2

sortDeepBelow :: (Label -> Label -> Ordering) -> EID -> Model -> Model
sortDeepBelow ord root = forestL %~ onForestBelowId root deepSortByOrd
  where
    -- prob kinda inefficient but I got <= 10 levels or something.
    deepSortByOrd = onForestChildren deepSortByOrd . sortBy ord'
    ord' (Node (_, attr1) _) (Node (_, attr2) _) = ord attr1 attr2

-- * Helpers
