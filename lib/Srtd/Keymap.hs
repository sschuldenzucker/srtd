
module Srtd.Keymap where

import Brick (suffixLenses)
import Brick.Keybindings (Binding)
import Brick.Keybindings.KeyConfig (binding)
import Brick.Keybindings.Pretty (ppBinding)
import Data.Function ((&))
import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)
import Graphics.Vty (Key, Modifier)

data Keymap a = Keymap
  { kmName :: Text
  , kmSticky :: Bool
  -- ^ If True, we don't go back to the toplevel when a leaf action in the submap was triggered.
  -- Only relevant for submaps.
  , kmMap :: (Map Binding (KeymapItem a))
  , kmAddlDesc :: [(Text, Text)]
  -- ^ Additional descriptions for hidden items.
  }

data KeymapItem a = KeymapItem
  { kmiItem :: KeymapItemItem a
  , kmiHidden :: Bool
  -- ^ If True, this won't show up by default in `kmDesc`. Use `kmaAddlDesc` to describe these
  -- manually. Useful for pairs/groups of keys (e.g., hjkl or cursor keys.)
  }

data KeymapItemItem a
  = LeafItem
      -- | Name. For submap items, the name is the name of the keymap.
      Text
      -- | Action
      a
  | SubmapItem (Keymap a)

data KeymapZipper a = KeymapZipper
  { parents :: [Keymap a]
  , cur :: Keymap a
  }

-- | We only provide a trivial-ish show instance here b/c you usually don't care and a isn't showable.
-- Could show more but prob not worth it.
instance Show (KeymapZipper a) where
  show (KeymapZipper ps _) = "KeymapZipper(level " ++ show (length ps) ++ ")"

keymapToZipper :: Keymap a -> KeymapZipper a
keymapToZipper = KeymapZipper []

kmMake :: Text -> [(Binding, KeymapItem a)] -> Keymap a
kmMake name kvs = Keymap name False (Map.fromList kvs) []

sticky :: Keymap a -> Keymap a
sticky km = km {kmSticky = True}

kmWithAddlDesc :: [(Text, Text)] -> Keymap a -> Keymap a
kmWithAddlDesc addlDesc km@Keymap {kmAddlDesc} = km {kmAddlDesc = kmAddlDesc ++ addlDesc}

kmLeaf :: Binding -> Text -> a -> (Binding, KeymapItem a)
kmLeaf b l i = (b, KeymapItem (LeafItem l i) False)

kmSub :: Binding -> Keymap a -> (Binding, KeymapItem a)
kmSub b i = (b, KeymapItem (SubmapItem i) False)

hide :: (Binding, KeymapItem a) -> (Binding, KeymapItem a)
hide (b, itm) = (b, itm {kmiHidden = True})

-- I guess we could do something fancy with type class recursion but let's not.

kmzUp :: KeymapZipper a -> KeymapZipper a
kmzUp (KeymapZipper (p : ps) _) = KeymapZipper ps p
kmzUp kz = kz

kmzDown :: Keymap a -> KeymapZipper a -> KeymapZipper a
kmzDown km (KeymapZipper ps cur) = KeymapZipper (cur : ps) km

-- | Reset to root keymap
kmzResetRoot :: KeymapZipper a -> KeymapZipper a
kmzResetRoot kz@(KeymapZipper [] _) = kz
kmzResetRoot (KeymapZipper ps _) = KeymapZipper [] (last ps)

-- | Reset to next sticky keymap above, or to self, or to root.
kmzResetSticky :: KeymapZipper a -> KeymapZipper a
kmzResetSticky kz@(KeymapZipper [] _) = kz
kmzResetSticky kz@(KeymapZipper (Keymap {kmSticky} : _) _)
  | kmSticky = kz
  | otherwise = kmzResetSticky $ kmzUp kz

kmzIsToplevel :: KeymapZipper a -> Bool
kmzIsToplevel (KeymapZipper ps _) = null ps

-- | [(key desc, action desc)]
kmDesc :: Keymap a -> [(Text, Text)]
kmDesc (Keymap {kmMap, kmAddlDesc}) =
  Map.toList kmMap
    & filter (not . kmiHidden . snd)
    & fmap (\(k, item) -> (ppBinding k, describeItem item))
    & (++ kmAddlDesc)
    & sortBy (comparing fst)
 where
  describeItem (KeymapItem {kmiItem = LeafItem name _}) = name
  describeItem (KeymapItem {kmiItem = SubmapItem (Keymap {kmName})}) = kmName <> "..."

data KeyDesc = KeyDesc
  { kdName :: Text
  , kdIsToplevel :: Bool
  , kdPairs :: [(Text, Text)]
  }

suffixLenses ''KeyDesc

kmzDesc :: KeymapZipper a -> KeyDesc
kmzDesc (KeymapZipper ps cur) =
  KeyDesc
    { kdName = kmName cur
    , kdIsToplevel = null ps
    , kdPairs = kmDesc cur
    }

data KeymapResult a = NotFound | SubmapResult (KeymapZipper a) | LeafResult a (KeymapZipper a)

-- SOMEDAY Maybe we should use Brick's keymap infrastructure, at least for some of our types. But it's a bit overcomplicated for us here.
kmzLookup :: KeymapZipper a -> Key -> [Modifier] -> KeymapResult a
kmzLookup kz@(KeymapZipper {cur = Keymap {kmMap, kmSticky}}) key mods = case Map.lookup (binding key mods) kmMap of
  Nothing -> NotFound
  Just KeymapItem {kmiItem = LeafItem _ x} -> LeafResult x (if kmSticky then kz else kmzResetSticky (kmzUp kz))
  Just KeymapItem {kmiItem = SubmapItem sm} -> SubmapResult (kmzDown sm kz)
