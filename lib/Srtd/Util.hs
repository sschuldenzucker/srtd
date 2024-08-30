-- | Collected simple utilities
module Srtd.Util where

-- | This is really defined for anything with a neutral element, in this case the neutral element being `pure ()`
whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust mx f = maybe (pure ()) f mx
