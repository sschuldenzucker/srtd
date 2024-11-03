-- | Collected simple utilities. Most of them are prob in extra but w/e.
module Srtd.Util where

import Control.Monad ((<=<))
import Data.Tree (Forest, foldTree)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err = maybe (Left err) Right

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- | This is really defined for anything with a neutral element, in this case the neutral element being `pure ()`
whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust mx f = maybe (pure ()) f mx

-- | Compose a function n times with itself (0 times being the identity)
composeNTimes :: Int -> (a -> a) -> a -> a
composeNTimes n f = foldr (.) id $ replicate n f

-- | Monadic variant of `composeNTimes`.
composeNTimesM :: (Monad m) => Int -> (a -> m a) -> a -> m a
composeNTimesM n f = foldr (<=<) return $ replicate n f

-- * List helpers

-- | Transform a function partial on the empty list (like 'minimum') to a complete one by providing
-- a default.
forEmptyList :: b -> ([a] -> b) -> [a] -> b
forEmptyList dflt _ [] = dflt
forEmptyList _ f xs = f xs

-- * Basic tree helpers

-- | The sane `fmap` instance. (the default is the list instance, which isn't normally desired.)
--
-- Helper.
mapForest :: (a -> b) -> Forest a -> Forest b
mapForest f = map (fmap f)

-- | The catamorphism but for forests
foldForest :: (a -> [b] -> b) -> Forest a -> [b]
foldForest f = map (foldTree f)
