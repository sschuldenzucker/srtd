{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf #-}

-- | An imperative event framework
module Srtd.ProactiveBandana where

import Brick (suffixLenses)
import Control.Monad.State (MonadState (..))
import Lens.Micro.Platform

-- SOMEDAY not clear I need to be this general.

-- TODO should we make these higher-kind generic, only passing the type constructor like SimpleStream?

-- | Minimal implementation: trigger or triggerArg
--
-- SOMEDAY do I need MonadState here already? It'd mean that the stream can transform itself, which might be useful.
class (MonadState s m) => IsStream s m a | s -> m a where
  trigger :: a -> m ()

-- SOMEDAY the 'm' parameter is not used.
class (IsStream s m a) => IsCell s m a | s -> m a where
  current :: s -> a

  getCurrent :: m a
  getCurrent = current <$> get

data Stream m a = forall s. (IsStream s m a) => Stream {unStream :: s}

data Cell m a = forall s. (IsCell s m a) => Cell {unCell :: s}

cellToStream :: Cell m a -> Stream m a
cellToStream (Cell s) = Stream s

instance (MonadState (Stream m a) m) => IsStream (Stream m a) m a where
  trigger x = do
    (Stream s) <- get
    -- Funky lens hacking
    let theLens = lens (const s) (const Stream)
    zoom theLens $ trigger x

instance (MonadState (Cell m a) m) => IsStream (Cell m a) m a where
  trigger x = do
    (Cell s) <- get
    -- Funky lens hacking
    let theLens = lens (const s) (const Cell)
    zoom theLens $ trigger x

instance IsCell (Cell m a) m a where
  current (Cell s) = current s

data SimpleStream m a = SimpleStream (a -> m ())

-- | A Stream that just execute a given callback on trigger.
callStream :: (MonadState (SimpleStream m a) m) => (a -> m ()) -> Stream m a
callStream = Stream . SimpleStream

instance (MonadState (SimpleStream m a) m) => IsStream (SimpleStream m a) m a where
  trigger x = get >>= \(SimpleStream f) -> f x

data SimpleCell m a = SimpleCell a (a -> m ())

-- | A cell that just stores the current value and always triggers its callback.
simpleCell :: (MonadState (SimpleCell m a) m) => a -> (a -> m ()) -> Cell m a
simpleCell x f = Cell $ SimpleCell x f

instance (MonadState (SimpleCell m a) m) => IsStream (SimpleCell m a) m a where
  trigger x = get >>= \(SimpleCell _ f) -> f x

instance (MonadState (SimpleCell m a) m) => IsCell (SimpleCell m a) m a where
  current (SimpleCell x _) = x

data UniqueCell m a = UniqueCell a (a -> m ())

uniqueCell :: (Eq a, MonadState (UniqueCell m a) m) => a -> (a -> m ()) -> Cell m a
uniqueCell x f = Cell $ UniqueCell x f

instance (Eq a, MonadState (UniqueCell m a) m) => IsStream (UniqueCell m a) m a where
  trigger x' =
    get >>= \(UniqueCell x f) ->
      if
        | x == x' -> return ()
        | otherwise -> put (UniqueCell x' f) >> f x'

-- trigger (UniqueCell x f) x'
--   | x == x' = return ()
--   | otherwise = put (UniqueCell x' f) >> f x'

instance (Eq a, MonadState (UniqueCell m a) m) => IsCell (UniqueCell m a) m a where
  current (UniqueCell x _) = x
