{-# LANGUAGE FunctionalDependencies #-}

{-| The 42nd attempt at creating a standardized map interface.

Incomplete, only implements what I need.

SOMEDAY currently underused.
-}
module Srtd.Data.MapLike where

import Data.EnumMap.Strict qualified as EMS
import Data.IntMap.Strict qualified as IMS
import Data.Map.Strict qualified as MS
import Data.Maybe (fromMaybe, isJust)
import Prelude hiding (lookup)

class MapLike k v m | m -> k v where
  empty :: m
  insertWith :: (v -> v -> v) -> k -> v -> m -> m
  unionWith :: (v -> v -> v) -> m -> m -> m
  lookup :: k -> m -> Maybe v
  fromList :: [(k, v)] -> m

  insert :: k -> v -> m -> m
  insert = insertWith const

  union :: m -> m -> m
  union = unionWith const

  member :: k -> m -> Bool
  member k = isJust . lookup k

  findWithDefault :: v -> k -> m -> v
  findWithDefault d k = fromMaybe d . lookup k

instance (Ord k) => MapLike k v (MS.Map k v) where
  empty = MS.empty
  insertWith = MS.insertWith
  unionWith = MS.unionWith
  insert = MS.insert
  union = MS.union
  lookup = MS.lookup
  member = MS.member
  fromList = MS.fromList
  findWithDefault = MS.findWithDefault

instance (Enum k) => MapLike k v (EMS.EnumMap k v) where
  empty = EMS.empty
  insertWith = EMS.insertWith
  unionWith = EMS.unionWith
  insert = EMS.insert
  union = EMS.union
  lookup = EMS.lookup
  member = EMS.member
  fromList = EMS.fromList
  findWithDefault = EMS.findWithDefault

instance MapLike Int v (IMS.IntMap v) where
  empty = IMS.empty
  insertWith = IMS.insertWith
  unionWith = IMS.unionWith
  insert = IMS.insert
  union = IMS.union
  lookup = IMS.lookup
  member = IMS.member
  fromList = IMS.fromList
  findWithDefault = IMS.findWithDefault
