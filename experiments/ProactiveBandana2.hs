{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

-- | An imperative events framework. Kinda.
module Srtd.ProactiveBandana2 where

import Control.Monad.State (MonadState (..))
import Data.Kind (Type)
import Lens.Micro.Mtl.Internal (Zoom)
import Lens.Micro.Platform
import Srtd.Util (ALens' (..))

data TrivialCell n a = TrivialCell (a -> n ())

class IsCell s where
  type Input s
  type Effect s :: Type -> Type
  type Value s

  pushCell :: (MonadState s m) => (Input s) -> m (Effect s ())
  value :: s -> Value s

  getValue :: (MonadState s m) => m (Value s)
  getValue = value <$> get

runPushCell ::
  (Zoom m (Effect s) s t, IsCell s, Functor (Zoomed m (Effect s ()))) =>
  ALens' t s -> Input s -> Effect s ()
runPushCell l x = do
  act <- zoom (runALens' l) $ pushCell x
  act

instance IsCell (TrivialCell n a) where
  type Input (TrivialCell n a) = a
  type Effect (TrivialCell n a) = n
  type Value (TrivialCell n a) = ()

  pushCell x = get >>= \(TrivialCell f) -> return (f x)
  value = const ()

data SimpleCell n a = SimpleCell a (a -> n ())

instance IsCell (SimpleCell n a) where
  type Input (SimpleCell n a) = a
  type Effect (SimpleCell n a) = n
  type Value (SimpleCell n a) = a

  pushCell x' = do
    (SimpleCell _x f) <- get
    put (SimpleCell x' f)
    return (f x')
  value (SimpleCell x _f) = x

data UniqueCell n a = UniqueCell a (a -> n ())

instance (Eq a, Monad n) => IsCell (UniqueCell n a) where
  type Input (UniqueCell n a) = a
  type Effect (UniqueCell n a) = n
  type Value (UniqueCell n a) = a

  pushCell x' = do
    (UniqueCell x f) <- get
    if
      | x /= x' -> put (UniqueCell x' f) >> return (f x')
      | otherwise -> return (return ())

  value (UniqueCell x _f) = x

data UniqueMaybeCell n a = (Eq a, Monad n) => UniqueMaybeCell (Maybe a) (a -> n ())

instance IsCell (UniqueMaybeCell n a) where
  -- SOMEDAY can I leave input and effect generic?
  type Input (UniqueMaybeCell n a) = a
  type Effect (UniqueMaybeCell n a) = n
  type Value (UniqueMaybeCell n a) = Maybe a -- ! Only place where I need this.

  pushCell x' = do
    (UniqueMaybeCell mx f) <- get
    case mx of
      Nothing -> put (UniqueMaybeCell (Just x') f) >> return (f x')
      Just x
        | x /= x' -> put (UniqueMaybeCell (Just x') f) >> return (f x')
        | otherwise -> return (return ())

  value (UniqueMaybeCell mx _f) = mx

data Cell i e v = forall s. (IsCell s, Input s ~ i, Effect s ~ e, Value s ~ v) => Cell s

instance IsCell (Cell i e v) where
  type Input (Cell i e v) = i
  type Effect (Cell i e v) = e
  type Value (Cell i e v) = v

  pushCell x' = do
    (Cell c) <- get
    let theLens = lens (const c) (const Cell)
    zoom theLens $ pushCell x'

  value (Cell c) = value c
