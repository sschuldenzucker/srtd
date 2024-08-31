-- | Collected simple utilities. Most of them are prob in extra but w/e.
module Srtd.Util where

import Control.Monad ((<=<))

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err = maybe (Left err) Right

-- | This is really defined for anything with a neutral element, in this case the neutral element being `pure ()`
whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust mx f = maybe (pure ()) f mx

-- | Compose a function n times with itself (0 times being the identity)
composeNTimes :: Int -> (a -> a) -> a -> a
composeNTimes n f = foldr (.) id $ replicate n f

-- | Monadic variant of `composeNTimes`.
composeNTimesM :: (Monad m) => Int -> (a -> m a) -> a -> m a
composeNTimesM n f = foldr (<=<) return $ replicate n f
