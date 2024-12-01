-- | Built on top of rose zipper, some helpers for move operations etc.
module Srtd.Data.TreeZipper where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.List (unfoldr)
import Data.Maybe (listToMaybe)
import Data.Tree.Zipper (Empty, Full, TreePos)
import Data.Tree.Zipper qualified as Z

-- * Walker types

-- Walkers are functions that map a certain kind of position to another kind of position and may fail

-- ** Move Walker

-- | Walks from an empty position (of a just-removed node) to a new empty position (where the node is to be moved), and may fail at that.
--
-- SOMEDAY Delete this? Looks like this always gets us in trouble. And I don't think we're using it.
type MoveWalker a = TreePos Empty a -> Maybe (TreePos Empty a)

toNextSibling
  , toPrevSibling
  , toBeforeParent
  , toAfterParent
  , toFirstChildOfNext
  , toFirstChildOfPrev
  , toLastChildOfNext
  , toLastChildOfPrev ::
    MoveWalker a
toNextSibling = Z.next
toPrevSibling = Z.prev
toBeforeParent = fmap Z.prevSpace . Z.parent
toAfterParent = fmap Z.nextSpace . Z.parent
toFirstChildOfNext = fmap Z.children . Z.nextTree
toFirstChildOfPrev = fmap Z.children . Z.prevTree
toLastChildOfNext = fmap (Z.last . Z.children) . Z.nextTree
toLastChildOfPrev = fmap (Z.last . Z.children) . Z.prevTree

-- Not sure if "preorder" is the right wording here; it doesn't actually recur upwards.
-- SOMEDAY I'm sure there's a monad or something that does these alternatives.
toBeforeNextPreorder, toAfterPrevPreorder :: MoveWalker a
toBeforeNextPreorder eloc =
  toBeforeFirstChildOfNext eloc
    <|> Z.next eloc
    <|> Z.nextSpace
      <$> Z.parent eloc
 where
  -- Like toFirstChildOfNext but fails if there is no first child. This is intuitive for some use cases.
  toBeforeFirstChildOfNext eloc' = do
    nxt <- Z.nextTree eloc'
    fc <- Z.firstChild nxt
    return $ Z.prevSpace fc
toAfterPrevPreorder eloc =
  toAfterLastChildOfPrev eloc
    <|> Z.prev eloc
    <|> Z.prevSpace
      <$> Z.parent eloc
 where
  -- Like toLastChildOfPrev but fails if there is no first child. This is intuitive for some use cases.
  toAfterLastChildOfPrev eloc' = do
    nxt <- Z.prevTree eloc'
    fc <- Z.lastChild nxt
    return $ Z.nextSpace fc

-- ** Go Walker

-- | Walks from a node position to another node position and may fail at that.
type GoWalker a = TreePos Full a -> Maybe (TreePos Full a)

-- The following are a trivial abstraction and could be eliminated, but we keep them for now to
-- abstract the tree implementation away.
goNextSibling
  , goPrevSibling
  , goParent
  , goFirstChild
  , goFirstChildOfNext
  , goLastChildOfPrev ::
    GoWalker a
goNextSibling = Z.next
goPrevSibling = Z.prev
goParent = Z.parent
goFirstChild = Z.firstChild
goFirstChildOfNext = Z.firstChild <=< Z.next
goLastChildOfPrev = Z.lastChild <=< Z.prev

-- ** Insert Walker

-- | Walks from a node position to an empty position where a new tree should be inserted.
--
-- NB we currently don't use the may-fail feature, but it's good to have for consistency.
type InsertWalker a = TreePos Full a -> Maybe (TreePos Empty a)

-- The following are a trivial abstraction and could be eliminated, but we keep them for now to
-- abstract the tree implementation away.
insBefore, insAfter, insFirstChild, insLastChild :: InsertWalker a
insBefore = Just . Z.prevSpace
insAfter = Just . Z.nextSpace
insFirstChild = Just . Z.children
insLastChild = Just . Z.last . Z.children

-- ** Dynamic Move Walker

-- This is combined 'GoWalker' and 'InsertWalker' where the walker that chooses the
-- insert location can be chosen dynamically based on the content/structure. See
-- 'forestMoveSubtreeRelFromForestIdDynamic'.

type DynamicMoveWalker a b = TreePos Full a -> Maybe (TreePos Full a, InsertWalker b)

-- NB there are some details in here re (1) which potential positions are skipped and (2) which
-- position the moved node ends up in the unfiltered tree when it's moved in a filtered tree. E.g., 'go
-- to next, insert as first child' is different from 'go to first child as next, insert before'
dtoNextPreorder, dtoPrevPreorder :: DynamicMoveWalker a b
dtoNextPreorder loc =
  ((,insBefore) <$> goFirstChildOfNext loc)
    <|> ((,insAfter) <$> goNextSibling loc)
    <|> ((,insAfter) <$> goParent loc)
dtoPrevPreorder loc =
  ((,insAfter) <$> goLastChildOfPrev loc)
    <|> ((,insBefore) <$> goPrevSibling loc)
    <|> ((,insBefore) <$> goParent loc)

-- * Operations

-- These use the walkers to perform certain operations.

-- | Take the tree out of the current position, walk from the hole left via the given fct, and put it there.
-- The walking function may fail and then we do nothing.
-- Returns the new position that we've moved to.
mzMove :: MoveWalker a -> TreePos Full a -> TreePos Full a
mzMove go pos = case go . Z.delete $ pos of
  Nothing -> pos
  Just epos' -> Z.insert (Z.tree pos) epos'

-- * Listing Nodes

-- | List all trees after an empty position, in order. Non-recursive.
zFollowingTrees :: TreePos Empty a -> [TreePos Full a]
zFollowingTrees epos = case Z.nextTree epos of
  Nothing -> []
  Just pos -> pos : unfoldr go pos
 where
  go = fmap dup . Z.next
  dup x = (x, x)

-- | List all descendents in DFS preorder. For empty positions, we list the descendants of all
-- following trees (so that the `children` position, before the first child, lists the forest .)
class ZDescendants t where
  zDescendants :: TreePos t a -> [TreePos Full a]

instance ZDescendants Full where
  zDescendants pos = pos : zDescendants (Z.children pos)

instance ZDescendants Empty where
  zDescendants epos = zFollowingTrees epos >>= zDescendants

-- | Find the first descendant where the predicate returns true, or Nothing if none was found.
--
-- SOMEDAY This is probably pretty inefficient b/c we construct and iterate through positions, not trees.
-- (the zipper doesn't "know" we're not using intermediate results and we don't have closer access
-- to the underlying data b/c it's not exported :/) SOMEDAY if it ever becomes a bottleneck, copy
-- rosezipper here and add zFindDescendantByLabel, but more efficiently (or zFindDescendantByLabel
-- more generally actually) - And then prob also optimized zFullForest etc.
zFindDescendantByLabel :: (ZDescendants t) => (b -> Bool) -> TreePos t b -> Maybe (TreePos Full b)
zFindDescendantByLabel lp = zFindFirst $ lp . Z.label
 where
  zFindFirst :: (ZDescendants t) => (TreePos Full a -> Bool) -> TreePos t a -> Maybe (TreePos Full a)
  zFindFirst p = listToMaybe . filter p . zDescendants

-- * Rebuilding Trees and Forests

-- | The "root" of the forest, i.e., the space before the first child at the toplevel node (if any).
class ZForestRoot t where
  zForestRoot :: TreePos t a -> TreePos Empty a

instance ZForestRoot Full where
  zForestRoot = Z.first . Z.prevSpace . Z.root

instance ZForestRoot Empty where
  zForestRoot eloc = case Z.parent eloc of
    Just loc -> zForestRoot loc
    Nothing -> Z.first eloc

-- * Insertion

-- * Modification

-- * Sorting
