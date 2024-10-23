-- | Data structure for trees/forests where nodes can be identified via IDs, and helpers
--
-- SOMEDAY change this to something where ID access is more efficient; see the Elixir version I made at some point.
--
-- To be imported qualified.
module Srtd.Data.IdTree where

import Control.Monad ((<=<))
import Data.List (find, sortBy, unfoldr)
import Data.Maybe (fromMaybe)
import Data.Tree
import Data.Tree.Zipper (Empty, Full, TreePos)
import Data.Tree.Zipper qualified as Z
import Lens.Micro.Platform
import Srtd.Data.TreeZipper
import Srtd.Todo
import Srtd.Util (mapForest)

newtype IdForest id attr = IdForest {idForest :: Forest (id, attr)}
  deriving (Show)

-- The following can probably be automated using some iso construction but I tried Lens.Micro.Pro and couldn't get it to work.

withIdForest :: (Forest (id, attr) -> Forest (id', attr')) -> IdForest id attr -> IdForest id' attr'
withIdForest f = IdForest . f . idForest

onIdForest :: (IdForest id attr -> IdForest id' attr') -> Forest (id, attr) -> Forest (id', attr')
onIdForest f = idForest . f . IdForest

-- | This also provides the "correct" fmap instance over forests (instead of the accidental list instance)
instance Functor (IdForest id) where
  fmap f = withIdForest $ mapForest $ \(i, x) -> (i, f x)

-- TODO WIP Move the abstract machienery from Model here, and prob also unify it.
-- Start with the find-by-id methods

-- * Convenience data extraction

zGetId :: TreePos Full (id, a) -> id
zGetId = fst . Z.label

-- * Navigation by ID

-- | Find the descendant with the given ID, or Nothing if none was found.
zFindId :: (ZDescendants t, Eq id) => id -> TreePos t (id, b) -> Maybe (TreePos Full (id, b))
zFindId tgt = zFindDescendantByLabel $ \(i, _) -> i == tgt

zForestFindId :: (Eq id) => id -> IdForest id b -> Maybe (TreePos Full (id, b))
zForestFindId tgt = zFindId tgt . Z.fromForest . idForest

-- | Given an anchor ID and a walker, return the ID that we walk to here (if any).
-- TODO should be in IdTree
forestGoFromToId :: (Eq id) => id -> GoWalker (id, a) -> IdForest id a -> Maybe id
forestGoFromToId tgt go = fmap zGetId . go <=< zForestFindId tgt

-- * Modification

-- | This is like `fmap` but also allows access to the *ids* while updating labels (but doesn't allow modification of the labels)
mapIdForestWithIds :: (id -> a -> b) -> IdForest id a -> IdForest id b
mapIdForestWithIds f = withIdForest $ mapForest go
  where
    go (i, x) = (i, f i x)

-- | Only leaves the initial segments of the forest where the predicate all applies.
filterIdForest :: (a -> Bool) -> IdForest id a -> IdForest id a
filterIdForest p = withIdForest filter'
  where
    filter' forest = [Node l (filter' children) | Node l@(_, x) children <- forest, p x]

filterIdForestWithIds :: (id -> a -> Bool) -> IdForest id a -> IdForest id a
filterIdForestWithIds p = withIdForest filter'
  where
    filter' forest = [Node l (filter' children) | Node l children <- forest, uncurry p l]

-- | Modify the forest below the given target ID by a function. No-op if the ID is not found.
onForestBelowId :: (Eq id) => id -> (Forest (id, a) -> Forest (id, a)) -> IdForest id a -> IdForest id a
-- This is probably a bit inefficient but it was there so w/e
-- SOMEDAY it's actually an error if root is not found.
onForestBelowId root f idforest@(IdForest forest) = case zFindId root . Z.fromForest $ forest of
  Nothing -> idforest
  Just rootLoc -> IdForest $ Z.forest . zForestRoot . Z.modifyTree (onTreeChildren f) $ rootLoc
  where
    onTreeChildren g (Node x children) = Node x (g children)

-- * Inserting nodes

forestInsertLabelRelToId :: (Eq id) => id -> InsertWalker (id, a) -> id -> a -> IdForest id a -> IdForest id a
forestInsertLabelRelToId tgt go i label forest = fromMaybe forest $ do
  tgtLoc <- zFindId tgt forestLoc
  insLoc <- go tgtLoc
  let postInsLoc = Z.insert (Node (i, label) []) insLoc
  return $ IdForest $ Z.forest . zForestRoot $ postInsLoc
  where
    forestLoc = Z.fromForest . idForest $ forest

-- * Moving nodes

-- | Given a `tgt` and an `anchor` and a walker to go from the anchor to an insert position, move `tgt` there.
--
-- `anchor` cannot be `tgt` or one of its descendants. This will silently fail as a no-op.
--
-- SOMEDAY Instead of silent failure, it's actually an *error* if any of the nodes are not found (except maybe in `go`)
forestMoveSubtreeIdRelToAnchorId :: (Eq id) => id -> id -> InsertWalker (id, a) -> IdForest id a -> IdForest id a
forestMoveSubtreeIdRelToAnchorId tgt anchor go forest = fromMaybe forest $ do
  tgtLoc <- zFindId tgt forestLoc
  let tgtTree = Z.tree tgtLoc
  let forestLoc' = zForestRoot . Z.delete $ tgtLoc
  anchorLoc <- zFindId anchor forestLoc'
  insertLoc <- go anchorLoc
  let forest'' = Z.forest . zForestRoot . Z.insert tgtTree $ insertLoc
  return $ IdForest forest''
  where
    forestLoc = Z.fromForest . idForest $ forest

-- | When called like `forestMoveSubtreeRelFromForestId tgt go ins haystack forest`, this doesn the following:
--
-- 1. Apply `go` to the position of `tgt` in `haystack` to find an `anchor` node, identified by its ID.
-- 2. Consider that ID in `forest` and apply `ins` to find a position to which `tgt` should be moved.
-- 3. Move `tgt` there in `forest` and return the result.
--
-- This silently fails if one of the IDs cannot be found OR if one of the walkers fails.
--
-- SOMEDAY the former is actually an error. The latter is not.
forestMoveSubtreeRelFromForestId :: (Eq id) => id -> GoWalker (id, a) -> InsertWalker (id, b) -> IdForest id a -> IdForest id b -> IdForest id b
forestMoveSubtreeRelFromForestId tgt go ins haystack forest = fromMaybe forest $ do
  tgtLoc <- zFindId tgt haystackLoc
  anchorLoc <- go tgtLoc
  let (anchorId, _) = Z.label anchorLoc
  return $ forestMoveSubtreeIdRelToAnchorId tgt anchorId ins forest
  where
    haystackLoc = Z.fromForest . idForest $ haystack
