{-# LANGUAGE OverloadedStrings #-}

module Keymap (KeymapZipper, kmzMake, kmLeaf, kmSub, kmzUp, kmzDown, kmzDesc, stepKeymap, KeymapResult (..)) where

import Brick.Keybindings (Binding)
import Brick.Keybindings.KeyConfig (binding)
import Brick.Keybindings.Pretty (ppBinding)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Graphics.Vty (Key, Modifier)

data Keymap a = Keymap (Map Binding (Text, KeymapItem a))

data KeymapItem a = LeafItem a | SubmapItem (Keymap a)

data KeymapZipper a = KeymapZipper
  { parents :: [Keymap a],
    cur :: Keymap a
  }

keymapToZipper :: Keymap a -> KeymapZipper a
keymapToZipper = KeymapZipper []

-- | Convenience helper for building keymaps. Use with `mkLeaf` and `mkSub`.
kmzMake :: [(Binding, Text, KeymapItem a)] -> KeymapZipper a
kmzMake = keymapToZipper . Keymap . Map.fromList . map (\(b, l, i) -> (b, (l, i)))

kmLeaf :: Binding -> Text -> a -> (Binding, Text, KeymapItem a)
kmLeaf b l i = (b, l, LeafItem i)

kmSub :: Binding -> Text -> Keymap a -> (Binding, Text, KeymapItem a)
kmSub b l i = (b, l, SubmapItem i)

-- I guess we could do something fancy with type class recursion but let's not.

kmzUp :: KeymapZipper a -> KeymapZipper a
kmzUp (KeymapZipper (p : ps) _) = KeymapZipper ps p
kmzUp kz = kz

kmzDown :: Keymap a -> KeymapZipper a -> KeymapZipper a
kmzDown km (KeymapZipper ps cur) = KeymapZipper (cur : ps) km

-- | [(key desc, action desc)]
kmDesc :: Keymap a -> [(Text, Text)]
kmDesc (Keymap theMap) = Map.toList theMap & fmap (\(k, item) -> (ppBinding k, describeItem item))
  where
    describeItem (d, LeafItem _) = d
    describeItem (d, SubmapItem _) = d <> "…"

-- | (is toplevel, [(key label, action description)])
kmzDesc :: KeymapZipper a -> (Bool, [(Text, Text)])
kmzDesc (KeymapZipper ps cur) = (null ps, kmDesc cur)

data KeymapResult a = NotFound | SubmapResult (KeymapZipper a) | LeafResult a

-- SOMEDAY Maybe we should use the keymap infrastructure. But it's a bit overcomplicated for us here.
-- TODO check normalization: uppercase / lowercase.
stepKeymap :: KeymapZipper a -> Key -> [Modifier] -> KeymapResult a
stepKeymap kz@(KeymapZipper {cur = Keymap theMap}) key mods = case Map.lookup (binding key mods) theMap of
  Nothing -> NotFound
  Just (_, LeafItem x) -> LeafResult x
  Just (_, SubmapItem sm) -> SubmapResult (kmzDown sm kz)
