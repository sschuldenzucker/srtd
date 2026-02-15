{-# LANGUAGE MultiWayIf #-}

-- TODO limit exports. Ideally we cannot just set the value.

{-| An imperative events handlers framework.

This is basically a thin upgrade over an imperative setter function, but provides a single point
of entry and some basic consistency enforcement / hiding.
-}
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
-- - `h` is the handler type: updates result in that value being output. Usually, `h = m ()` where
--    `m` is some monad. Usually, `m` is another state monad and then we can run the update and the
--    returned action via 'runUpdateALens'.
-- - `v` is the type of the stored value.
data Cell i h v = Cell
  { cValue :: v
  -- ^ Stored value
  , cUpdate :: i -> State (Cell i h v) h
  -- ^ Update routine that updates the cell state in response to a new value and returns a handler.
  }

suffixLenses ''Cell

-- * Construction

-- | A cell that just calls its handler and doesn't store any state.
justCallCell :: (i -> h) -> Cell i h ()
justCallCell f =
  Cell () $ \x' -> return (f x')

-- | A cell that stores its input and calls the handler on each update.
simpleCell :: v -> (v -> h) -> Cell v h v
simpleCell x0 f =
  Cell x0 $ \x' -> cValueL .= x' >> return (f x')

-- | A variant of 'simpleCell' that maps its input to its value using a function.
--
-- We have `simpleCell x0 g == simpleMappingCell x0 id g`
simpleMappingCell :: v -> (i -> v) -> (v -> h) -> Cell i h v
simpleMappingCell y0 f g =
  Cell y0 $ \x' ->
    let y' = f x'
     in cValueL .= y' >> return (g y')

-- | Like 'simpleMappingCell' but the handler depends on the _input_, not the mapped value.
--
-- May be useful when the input contains some kind of context for the handler to run, but we don't
-- need to store this.
simplePreMappingCell :: v -> (i -> v) -> (i -> h) -> Cell i h v
simplePreMappingCell y0 f g =
  Cell y0 $ \x' ->
    let y' = f x'
     in cValueL .= y' >> return (g x')

-- | A cell that just stores its input and doesn't have a handler.
justStoreCell :: v -> Cell v () v
justStoreCell x0 = simpleCell x0 (const ())

-- | A cell that just stores its input and does nothing on handler.
justStoreCellM :: (Monad m) => v -> Cell v (m ()) v
justStoreCellM x0 = simpleCell x0 (const $ return ())

-- | A cell that stores its input and calls its handler _iff_ the input has changed. Otherwise a given default.
uniqueCell :: (Eq v) => h -> v -> (v -> h) -> Cell v h v
uniqueCell dflt x0 f =
  Cell x0 $ \x' -> do
    x <- gets cValue
    if
      | x /= x' -> cValueL .= x' >> return (f x')
      | otherwise -> return $ dflt

-- | Monadic version of 'uniqueCell' that does nothing on no change.
uniqueCellM :: (Eq v, Monad m) => v -> (v -> m ()) -> Cell v (m ()) v
uniqueCellM = uniqueCell (return ())

-- | A cell that stores a Maybe value and calls its handler _iff_ the input is Just and changed.
-- Otherwise a given default.
--
-- This is useful when Nothing means "invalid". The cell then stores the last valid input and calls
-- back on each new valid input.
uniqueMaybeCell :: (Eq v) => h -> Maybe v -> (v -> h) -> Cell (Maybe v) h (Maybe v)
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

-- TODO what combinator patterns are available? Standard type classes? Reorder variables to match that? Custom combinators? And which are useful?

-- * Access

-- TODO these may not be needed.

-- | Get the value of a cell in a state monad
getValue :: (MonadState (Cell i h v) m) => m v
getValue = gets cValue

-- | Get the value of a Cell field in a state monad
getsValue :: (MonadState s m) => (s -> Cell i h a) -> m a
getsValue f = gets (cValue . f)

-- | Get the value of a lens pointing to a Cell in a state monad
useValue :: (MonadState s m) => Lens' s (Cell i h a) -> m a
useValue l = use (l . cValueL)

-- * Updating

-- | Given a lens pointing to a cell with monadic output type, supply a new value and run the
-- handler action.
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

-- | Apply a pure modification function to a cell inside a lens
runModifyLens ::
  (Zoom n m (Cell v (m b) v) t, Functor (Zoomed n (m b))) =>
  Lens' t (Cell v (m b) v) -> (v -> v) -> m b
runModifyLens l f = do
  act <- zoom l $ do
    (Cell x up) <- get
    state (runState (up $ f x))
  act
