-- | Data structure for trees/forests where nodes can be identified via IDs, and helpers
--
-- SOMEDAY change this to something where ID access is more efficient; see the Elixir version I made at some point.
--
-- To be imported qualified.
module Srtd.IdTree where

import Data.List (find, sortBy, unfoldr)
import Data.Tree
import Data.Tree.Zipper (Empty, Full, TreePos)
import Data.Tree.Zipper qualified as Z

type IdForest id attr = Forest (id, attr)

-- TODO WIP Move the abstract machienery from Model here, and prob also unify it.
-- Start with the find-by-id methods

-- * Zipper Helpers

zFollowingTrees :: TreePos Empty a -> [TreePos Full a]
zFollowingTrees epos = case Z.nextTree epos of
  Nothing -> []
  Just pos -> pos : unfoldr go pos
  where
    go = fmap dup . Z.next
    dup x = (x, x)

class ZDescendants t where
  zDescendants :: TreePos t a -> [TreePos Full a]

instance ZDescendants Full where
  zDescendants pos = pos : zDescendants (Z.children pos)

instance ZDescendants Empty where
  zDescendants epos = zFollowingTrees epos >>= zDescendants
