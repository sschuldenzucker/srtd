-- | Stateful keymaps with sub-maps and sticky options
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

-- * Keymaps

-- | Stateless keymap description with value type `a`. Often, `a` is a monadic action type (e.g., `EventM`).
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

-- | Stateful keymap state if you have sub-maps. Typically, components store a `KeymapZipper` in
-- their state to support this, unless the keymap is fully static / there are no sub-maps.
data KeymapZipper a = KeymapZipper
  { parents :: [Keymap a]
  , cur :: Keymap a
  }

-- | We only provide a trivial-ish show instance here b/c you usually don't care and `a` isn't showable.
-- Could show more but prob not worth it.
instance Show (KeymapZipper a) where
  show (KeymapZipper ps _) = "KeymapZipper(level " ++ show (length ps) ++ ")"

keymapToZipper :: Keymap a -> KeymapZipper a
keymapToZipper = KeymapZipper []

-- | Main convenience function to build a Keymap. This sets some defaults, use `sticky` and `kmWithAddlDesc` to adjust.
kmMake :: Text -> [(Binding, KeymapItem a)] -> Keymap a
kmMake name kvs = Keymap name False (Map.fromList kvs) []

-- | Leaf item, use with 'kmMake'.
kmLeaf :: Binding -> Text -> a -> (Binding, KeymapItem a)
kmLeaf b l i = (b, KeymapItem (LeafItem l i) False)

-- | Submap, use with 'kmMake'.
kmSub :: Binding -> Keymap a -> (Binding, KeymapItem a)
kmSub b i = (b, KeymapItem (SubmapItem i) False)

-- | Flag binding as hidden when used with 'kmMake'
hide :: (Binding, KeymapItem a) -> (Binding, KeymapItem a)
hide (b, itm) = (b, itm {kmiHidden = True})

-- | Add additional items to an existing keymap
kmAddItems :: Keymap a -> [(Binding, KeymapItem a)] -> Keymap a
kmAddItems km pairs = km {kmMap = Map.union (kmMap km) (Map.fromList pairs)}

-- | Union of two keymaps. Metadata (name, sticky) is taken from the _first_ keymap.
kmUnion :: Keymap a -> Keymap a -> Keymap a
kmUnion km1 km2 = km1 {kmMap = Map.union (kmMap km1) (kmMap km2), kmAddlDesc = kmAddlDesc km1 ++ kmAddlDesc km2}

-- | Make keymap sticky
sticky :: Keymap a -> Keymap a
sticky km = km {kmSticky = True}

-- | Add a 'kmAddlDesc' to a keymap
kmWithAddlDesc :: [(Text, Text)] -> Keymap a -> Keymap a
kmWithAddlDesc addlDesc km@Keymap {kmAddlDesc} = km {kmAddlDesc = kmAddlDesc ++ addlDesc}

-- | Flipped version of 'kmWithAddlDesc'
kmAddAddlDesc :: Keymap a -> [(Text, Text)] -> Keymap a
kmAddAddlDesc = flip kmWithAddlDesc

-- I guess we could do something fancy with type class recursion but let's not.

-- | Move to parent keymap
kmzUp :: KeymapZipper a -> KeymapZipper a
kmzUp (KeymapZipper (p : ps) _) = KeymapZipper ps p
kmzUp kz = kz

-- | Move into the given child keymap
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

-- | True iff we're at the root.
kmzIsToplevel :: KeymapZipper a -> Bool
kmzIsToplevel (KeymapZipper ps _) = null ps

-- | Make the list `[(key desc, action desc)]`
kmKeysDesc :: Keymap a -> [(Text, Text)]
kmKeysDesc (Keymap {kmMap, kmAddlDesc}) =
  Map.toList kmMap
    & filter (not . kmiHidden . snd)
    & fmap (\(k, item) -> (ppBinding k, describeItem item))
    & (++ kmAddlDesc)
    & sortBy (comparing fst)
 where
  describeItem (KeymapItem {kmiItem = LeafItem name _}) = name
  describeItem (KeymapItem {kmiItem = SubmapItem (Keymap {kmName})}) = kmName <> "..."

-- | Description of a keymap. See 'kmzDesc'
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
    , kdPairs = kmKeysDesc cur
    }

-- | Variant of kmzDesc for Keymap. Always at toplevel.
kmDesc :: Keymap a -> KeyDesc
kmDesc km =
  KeyDesc
    { kdName = kmName km
    , kdIsToplevel = True
    , kdPairs = kmKeysDesc km
    }

data KeymapResult a = NotFound | SubmapResult (KeymapZipper a) | LeafResult a (KeymapZipper a)

-- | Look up a pressed key in a keymap. Returns (if any) the leaf item and the updated zipper.
--
-- SOMEDAY Handling of the result is manual right now. We also don't make any effort to handle
-- ESC/BS (go up) here. We could provide a convenience method that lives in a MonadState and that
-- updates the KeymapZipper in state, and is used via zoom. Need to flag whether the key was handled,
-- though. Some structure using Alternative seems to be the right way to handle these events, actually.
--
-- SOMEDAY Maybe we should use Brick's keymap infrastructure, at least for some of our types. But it's a bit overcomplicated for us here.
kmzLookup :: KeymapZipper a -> Key -> [Modifier] -> KeymapResult a
kmzLookup kz@(KeymapZipper {cur = Keymap {kmMap, kmSticky}}) key mods = case Map.lookup (binding key mods) kmMap of
  Nothing -> NotFound
  Just KeymapItem {kmiItem = LeafItem _ x} -> LeafResult x (if kmSticky then kz else kmzResetSticky (kmzUp kz))
  Just KeymapItem {kmiItem = SubmapItem sm} -> SubmapResult (kmzDown sm kz)

-- | Like 'kmzLookup' but for Keymap instead of KeymapZipper.
kmLookup :: Keymap a -> Key -> [Modifier] -> KeymapResult a
kmLookup km key mods = kmzLookup (keymapToZipper km) key mods
