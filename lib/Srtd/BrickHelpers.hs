module Srtd.BrickHelpers where

import Brick

-- | Like `str` but truncate when we don't have enough space. Does *not* insert a "..." ellipsis or so.
--
-- Useful pretty much only in the context of an 'hBox' where something else occurs to the right.
--
-- SOMEDAY I'm honestly not sure this actually works or if it's just lucky circumstances. Hacky implementation.
strTruncateAvailable :: String -> Widget n
strTruncateAvailable s =
  -- very easy!
  let (Widget _ _ r) = (str s)
   in Widget Greedy Fixed r

-- | Set widget to a fixed width
setWidth :: Int -> Widget n -> Widget n
setWidth w = hLimit w . padRight Max

-- | Like `emptyWidget` but occupies at least one space. This is gonna stretch when used together
-- with `setWidth`, whereas `emptyWidget` won't. (is this a bug?)
almostEmptyWidget :: Widget n
almostEmptyWidget = str " "
