-- | Helpers for attrs. Right now exposes `withDescendAttr`
module Srtd.BrickAttrHelpers (withDescendAttr, attrMapDescend, attrMapKeys) where

import Brick (AttrMap, AttrName, Widget, updateAttrMap)
import Brick.AttrMap (
  attrMap,
  attrMapLookup,
  attrName,
  attrNameComponents,
  mapAttrName,
  setDefaultAttr,
 )
import Data.List (stripPrefix)
import Data.Map qualified as M
import Graphics.Vty (Attr)
import Unsafe.Coerce (unsafeCoerce)

-- Evil hack b/c brick doesn't export enough accessors for AttrMap (and also not constructors)
-- TODO this shouldn't exist.
data AttrMap'
  = AttrMap Attr (M.Map AttrName Attr)
  | ForceAttr Attr
  | ForceAttrAllowStyle Attr AttrMap

-- | List keys mapped by the AttrMap.
--
-- Brick doesn't expose this functionality, so we have to use `unsafeCoerce`. :/
attrMapKeys :: AttrMap -> [AttrName]
attrMapKeys m =
  case unsafeCoerce m of
    AttrMap _ theMap -> M.keys theMap
    _ -> []

-- | AttrMap version of 'withDescendAttr'. See there.
attrMapDescend :: AttrName -> AttrMap -> AttrMap
attrMapDescend a m =
  foldl' go (setDefaultAttr (attrMapLookup a m) m) (attrMapKeys m)
 where
  go m' a' = case stripPrefix ac (attrNameComponents a') of
    Just rest ->
      mapAttrName a' (mconcat . map attrName $ rest) m'
    Nothing -> m'
  ac = attrNameComponents a

-- | Descend into the given attr name `a`. This is like `withDefAttr a` but also, for any `b`, if
-- `a.b` is defined, it overrides `b` in the toplevel map. These overrides are always full (they don't
-- get merged).
--
-- This is useful for themes, which may prefer to be more or less granular, depending on the theme.
withDescendAttr :: AttrName -> Widget n -> Widget n
withDescendAttr a = updateAttrMap $ attrMapDescend a
