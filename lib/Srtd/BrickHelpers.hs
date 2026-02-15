module Srtd.BrickHelpers where

import Brick
import Graphics.Vty qualified as Vty

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

-- * BrickEvent patterns

-- To simplify events handling when we want to ignore these.

-- | For ignoring clicks
pattern SomeMouseDown :: BrickEvent n e
pattern SomeMouseDown <- MouseDown _ _ _ _

pattern SomeMouseUp :: BrickEvent n e
pattern SomeMouseUp <- MouseUp _ _ _

isMouse :: BrickEvent n e -> Bool
isMouse (MouseDown _ _ _ _) = True
isMouse (MouseUp _ _ _) = True
isMouse _ = False

pattern SomeMouse :: BrickEvent n e
pattern SomeMouse <- (isMouse -> True)

pattern VtyKeyEvent :: Vty.Key -> [Vty.Modifier] -> BrickEvent n e
pattern VtyKeyEvent key mods = VtyEvent (Vty.EvKey key mods)

isSomeOtherVtyEvent :: BrickEvent n e -> Bool
isSomeOtherVtyEvent = \case
  VtyEvent (Vty.EvKey _ _) -> False
  VtyEvent _ -> True
  _ -> False

pattern SomeVtyOtherEvent :: BrickEvent n e
pattern SomeVtyOtherEvent <- (isSomeOtherVtyEvent -> True)

isSomeNonVtyKeyBrickEvent :: BrickEvent n e -> Bool
isSomeNonVtyKeyBrickEvent = \case
  VtyEvent (Vty.EvKey _ _) -> False
  AppEvent _ -> False
  _ -> True

pattern SomeNonVtyKeyBrickEvent :: BrickEvent n e
pattern SomeNonVtyKeyBrickEvent <- (isSomeNonVtyKeyBrickEvent -> True)

-- o/w the type checker wrongly still complains about incomplete cases. Seems kinda dumb.
{-# COMPLETE VtyEvent, AppEvent, SomeMouseUp, SomeMouseDown #-}

{-# COMPLETE VtyEvent, AppEvent, MouseDown, SomeMouseUp #-}

{-# COMPLETE VtyEvent, AppEvent, SomeMouse #-}

{-# COMPLETE VtyKeyEvent, SomeVtyOtherEvent, AppEvent, SomeMouse #-}

{-# COMPLETE VtyKeyEvent, SomeNonVtyKeyBrickEvent, AppEvent #-}
