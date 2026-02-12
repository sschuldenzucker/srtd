{-# LANGUAGE MultiWayIf #-}

module Srtd.ProactiveBandana where

import Brick (suffixLenses)
import Control.Monad.State
import Lens.Micro.Mtl.Internal (Zoom)
import Lens.Micro.Platform

-- * Type

-- | An imperative-reactive cell that (optionally) stores a state and (optionally) returns a value
-- on update. The value is often a monadic action to notify others.
--
-- - `i` is the input type: updates input that type into the cell.
-- - `o` is the output type: updates result in that value being output. Usually, `o = m ()` where
--    `m` is some monad. Usually, `m` is another state monad and then we can run the update and the
--    returned action via 'runUpdateALens'.
-- - `v` is the type of the stored value. Usually either `()` or `i`.
--
-- SOMEDAY not clear if we need v or if v is always either () or i.
data Cell i o v = Cell
  { cValue :: v
  -- ^ Stored value
  , cUpdate :: i -> State (Cell i o v) o
  -- ^ Update routine that updates the cell state in response to a new value and returns a callback.
  }

suffixLenses ''Cell

-- * Construction

-- | A cell that just calls its callback and doesn't store any state.
justCallCell :: (i -> o) -> Cell i o ()
justCallCell f =
  Cell () $ \x' -> return (f x')

-- | A cell that stores its input and calls the callback on each update.
simpleCell :: v -> (v -> o) -> Cell v o v
simpleCell x0 f =
  Cell x0 $ \x' -> cValueL .= x' >> return (f x')

-- | A cell that just stores its input and doesn't have a callback.
justStoreCell :: v -> Cell v () v
justStoreCell x0 = simpleCell x0 (const ())

-- | A cell that stores its input and calls its callback _iff_ the input has changed. Otherwise a given default.
uniqueCell :: (Eq v) => o -> v -> (v -> o) -> Cell v o v
uniqueCell dflt x0 f =
  Cell x0 $ \x' -> do
    x <- gets cValue
    if
      | x /= x' -> cValueL .= x' >> return (f x')
      | otherwise -> return $ dflt

-- | Monadic version of 'uniqueCell' that does nothing on no change.
uniqueCellM :: (Eq v, Monad m) => v -> (v -> m ()) -> Cell v (m ()) v
uniqueCellM = uniqueCell (return ())

-- | A cell that stores a Maybe value and calls its callback _iff_ the input is Just and changed.
-- Otherwise a given default.
--
-- This is useful when Nothing means "invalid". The cell then stores the last valid input and calls
-- back on each new valid input.
uniqueMaybeCell :: (Eq v) => o -> Maybe v -> (v -> o) -> Cell (Maybe v) o (Maybe v)
uniqueMaybeCell dflt mx0 f =
  Cell mx0 $ \mx' -> do
    mx <- gets cValue
    case (mx, mx') of
      (Nothing, Just x') -> cValueL .= Just x' >> return (f x')
      (Just x, Just x')
        | x == x' -> cValueL .= Just x' >> return (f x')
        | otherwise -> return dflt
      (_, Nothing) -> return dflt

-- | Monadic version of 'uniqueMaybeCell' that does nothing unless there's a new valid input.
uniqueMaybeCellM :: (Eq v, Monad m) => Maybe v -> (v -> m ()) -> Cell (Maybe v) (m ()) (Maybe v)
uniqueMaybeCellM = uniqueMaybeCell (return ())

-- * Combinators

-- TODO what combinator patterns are available? Standard type classes? Reorder variables to match that? Custom combinators?

-- * Access

-- | Get the value of a cell in a state monad
getValue :: (MonadState (Cell i o v) m) => m v
getValue = gets cValue

-- | Get the value of a Cell field in a state monad
getsValue :: (MonadState s m) => (s -> Cell i o a) -> m a
getsValue f = gets (cValue . f)

-- | Get the value of a lens pointing to a Cell in a state monad
useValue :: (MonadState s m) => Lens' s (Cell i o a) -> m a
useValue l = use (l . cValueL)

-- * Updating

-- | Given a lens pointing to a cell with monadic output type, supply a new value and run the
-- callback action.
--
-- This is how cells are most commonly used.
runUpdateLens ::
  (Zoom n m (Cell i (m b) v) t, Functor (Zoomed n (m b))) =>
  Lens' t (Cell i (m b) v) -> i -> m b
runUpdateLens l x = do
  act <- zoom l $ do
    up <- gets cUpdate
    state (runState (up x))
  act
