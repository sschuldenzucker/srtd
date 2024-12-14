-- | Helpers for Brick List widgets. Mostly copied & modified from Brick.
module Srtd.BrickListHelpers where

-- #if !(MIN_VERSION_base(4,11,0))
-- import Data.Semigroup (Semigroup, (<>))
-- #endif

import Brick.AttrMap
import Brick.Main (lookupViewport)
import Brick.Types
import Brick.Util (clamp)
import Brick.Widgets.Core
import Brick.Widgets.List
import Control.Applicative ((<|>))
import Control.Monad.State (evalState)
import Data.Foldable (find, toList)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import Data.Sequence qualified as Seq
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Lens.Micro.Platform (Traversal', set, (%~), (&), (.~), (^.), (^?), _2)
import Prelude hiding (reverse, splitAt)

-- | Like 'listFindBy' but move backwards through the list.
listFindBackwardsBy ::
  (Foldable t, Splittable t, Reversible t) =>
  (e -> Bool) ->
  GenericList n t e ->
  GenericList n t e
-- ORIG code for forwards find.
-- listFindBackwardsBy test l =
--   let start = maybe 0 (+ 1) (l ^. listSelectedL)
--       (h, t) = splitAt start (l ^. listElementsL)
--       tailResult = find (test . snd) . zip [start ..] . toList $ t
--       headResult = find (test . snd) . zip [0 ..] . toList $ h
--       result = tailResult <|> headResult
--    in maybe id (set listSelectedL . Just . fst) result l
-- SOMEDAY I'm kinda shocked this works. Maybe rewrite it to be less silly, if only for performance.
listFindBackwardsBy test l = listReverse . listFindBy test . listReverse $ l
