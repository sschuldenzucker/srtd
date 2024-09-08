module Srtd.BrickHelpers where

import Brick

-- | Like `str` but truncate when we don't have enough space. Does *not* insert a "..." ellipsis or so.
--
-- Useful pretty much only in the context of an 'hBox' where something else occurs to the right.
strTruncateAvailable :: String -> Widget n
strTruncateAvailable s =
  -- very easy!
  let (Widget _ _ r) = (str s)
   in Widget Greedy Fixed r
