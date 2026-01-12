module Srtd.BrickAttrHelpers (combineAttrs) where

import Data.Bits ((.|.))
import Graphics.Vty.Attributes

-- | Combine attrs along their dimensions. The second argument dominates.
--
-- Copied from Brick.AttrMap, which unfortunately doesn't export this.
combineAttrs :: Attr -> Attr -> Attr
combineAttrs (Attr s1 f1 b1 u1) (Attr s2 f2 b2 u2) =
  Attr
    (s1 `combineStyles` s2)
    (f1 `combineMDs` f2)
    (b1 `combineMDs` b2)
    (u1 `combineMDs` u2)

combineMDs :: MaybeDefault a -> MaybeDefault a -> MaybeDefault a
combineMDs _ (SetTo v) = SetTo v
combineMDs (SetTo v) _ = SetTo v
combineMDs _ v = v

combineStyles :: MaybeDefault Style -> MaybeDefault Style -> MaybeDefault Style
combineStyles (SetTo a) (SetTo b) = SetTo $ a .|. b
combineStyles _ (SetTo v) = SetTo v
combineStyles (SetTo v) _ = SetTo v
combineStyles _ v = v
