{-| An imperative events handlers framework.

This is basically a thin upgrade over an imperative setter function, but provides a single point
of entry and some basic consistency enforcement / hiding.
-}
module Srtd.ProactiveBandana (
  -- * Type

  -- NB we intentionally do not export the constructor or fields for safety; the only way to update
  -- a cell is to at least observe the handler.
  --
  -- SOMEDAY maybe we should export Cell_ and have some useful combinators. Definite state lets us transform it.
  Cell,
  Cell',
  cValue,

  -- * Construction
  cell,
  cellWithState,

  -- ** Ready-made constructions
  justCallCell,
  justStoreCell,
  justStoreCellM,
  simpleCell,
  simpleMappingCell,
  simplePreMappingCell,
  uniqueCell,
  uniqueCellM,
  uniqueMaybeCell,
  uniqueMaybeCellM,

  -- * Combinators

  -- ** Transforming input
  mapCellInput,

  -- ** Transforming handlers
  transformCellHandler,
  mapCellHandler,
  mapCellHandlerM,
  mapCellHandlerM_,
  setCellHandler,

  -- ** Transforming input and handlers
  mapCellHandlerInput,
  mapCellHandlerInputM,
  mapCellHandlerInputM_,

  -- ** Combining cells

  -- Note: These combine *cell state*. The network of dependencies is exactly as you define it. No
  -- effort is made to resolve a network of dependencies in some efficient way or avoid duplicate
  -- updates, or ensuree consistency. If you have non-tree dependencies, this means that you should
  -- probably build a pure pipeline first and then use 'mapCellHandler' to map this to handler
  -- actions executed.
  pairCells,
  eitherCells,

  -- * Access
  getValue,
  getsValue,
  useValue,

  -- * Updating
  updateCellState,
  runUpdateLens,
  runModifyLens,

  -- * Experimental / Examples
  meanCell,
) where

import Brick (suffixLenses)
import Control.Monad (join)
import Control.Monad.State
import Lens.Micro.Mtl.Internal (Zoom)
import Lens.Micro.Platform
import Srtd.Util (safeConst)

-- * Type

-- | An imperative-reactive cell that (optionally) stores a state and (optionally) returns a value
-- on update. The value is often a monadic action to notify others.
--
-- - `i` is the input type: updates input that type into the cell.
-- - `h` is the handler type: updates result in that value being output. Usually, `h = m ()` where
--    `m` is some monad. Usually, `m` is another state monad and then we can run the update and the
--    returned action via 'runUpdateALens'.
-- - `v` is the type of the stored value.
data Cell i h v = forall s. Cell (Cell_ s i h v)

-- | Variant of 'Cell' with its state variable exposed. Lets you modify the state.
data Cell_ s i h v = Cell_
  { csValue :: v
  , csState :: s
  , csUpdate :: i -> State (v, s) h
  }

suffixLenses ''Cell_

csValueStateL :: Lens' (Cell_ s i h v) (v, s)
csValueStateL =
  -- I'm sure there's a smarter way to do this...
  lens
    (\c_ -> (c_ ^. csValueL, c_ ^. csStateL))
    (\c_ (v, s) -> (c_ & csValueL .~ v & csStateL .~ s))

-- | Construct a cell from initial value and update function, without hidden state
cell :: v -> (i -> State v h) -> Cell i h v
cell y0 up = Cell $ Cell_ y0 () $ zoom _1 . up

-- | A stateful cell of type 's'
cellWithState :: v -> s -> (i -> State (v, s) h) -> Cell i h v
cellWithState y0 s0 up = Cell (Cell_ y0 s0 up)

-- | A cell with input == value, which is common.
type Cell' v h = Cell v h v

-- | Get the current value of a cell
cValue :: Cell i h v -> v
-- NB we cannot just export the field b/c that would let users modify the cell. (there's no way to
-- just export the accessor function but not the field.)
cValue (Cell c_) = csValue c_

-- * Construction

-- | A cell that just calls its handler and doesn't store any state.
justCallCell :: (i -> h) -> Cell i h ()
justCallCell go = cell () $ return . go

-- | A cell that stores its input and calls the handler on each update.
simpleCell :: v -> (v -> h) -> Cell' v h
simpleCell v0 go = cell v0 $ \i -> put i >> return (go i)

-- | A variant of 'simpleCell' that maps its input to its value using a function.
--
-- We have `simpleCell x0 g == simpleMappingCell x0 id g`
simpleMappingCell :: v -> (i -> v) -> (v -> h) -> Cell i h v
simpleMappingCell v0 f go = cell v0 $ \i ->
  let v = f i
   in put v >> return (go v)

-- | Like 'simpleMappingCell' but the handler depends on the _input_, not the mapped value.
--
-- May be useful when the input contains some kind of context for the handler to run, but we don't
-- need to store this.
simplePreMappingCell :: v -> (i -> v) -> (i -> h) -> Cell i h v
simplePreMappingCell v0 f go = cell v0 $ \i -> put (f i) >> return (go i)

-- | A cell that just stores its input and doesn't have a handler.
justStoreCell :: v -> Cell v () v
justStoreCell x0 = simpleCell x0 (const ())

-- | A cell that just stores its input and does nothing on handler.
justStoreCellM :: (Monad m) => v -> Cell v (m ()) v
justStoreCellM x0 = simpleCell x0 (const $ return ())

-- | A cell that stores its input and calls its handler _iff_ the input has changed. Otherwise a given default.
uniqueCell :: (Eq v) => h -> v -> (v -> h) -> Cell' v h
uniqueCell dflt x0 f = cell x0 $ \x' -> do
  x <- get
  if
    | x /= x' -> put x' >> return (f x')
    | otherwise -> return $ dflt

-- | Monadic version of 'uniqueCell' that does nothing on no change.
uniqueCellM :: (Eq v, Monad m) => v -> (v -> m ()) -> Cell v (m ()) v
uniqueCellM = uniqueCell (return ())

-- | A cell that stores a Maybe value and calls its handler _iff_ the input is Just and changed.
-- Otherwise a given default.
--
-- This is useful when Nothing means "invalid". The cell then stores the last valid input and calls
-- back on each new valid input.
uniqueMaybeCell :: (Eq v) => h -> Maybe v -> (v -> h) -> Cell' (Maybe v) h
uniqueMaybeCell dflt mx0 f = cell mx0 $ \mx' -> do
  mx <- get
  case (mx, mx') of
    (Nothing, Just x') -> put (Just x') >> return (f x')
    (Just x, Just x')
      | x == x' -> put (Just x') >> return (f x')
      | otherwise -> return dflt
    (_, Nothing) -> return dflt

-- | Monadic version of 'uniqueMaybeCell' that does nothing unless there's a new valid input.
uniqueMaybeCellM :: (Eq v, Monad m) => Maybe v -> (v -> m ()) -> Cell' (Maybe v) (m ())
uniqueMaybeCellM = uniqueMaybeCell (return ())

-- * Cells with hidden state

-- EXAMPLE: Everything here.

-- | A cell with value equal to the mean of all its inputs, initialized at 0
meanCell :: (Fractional a) => Cell a () a
meanCell = cellWithState 0 0 $ \x' -> do
  (x, n) <- get
  put (((x * n) + x') / (n + 1), n + 1)

-- * Combinators

-- ** Transforming input

-- | Map a cell's input using a (contravariant) function.
--
-- This conceptually turns `Cell _ h v` into a contravariant functor.
mapCellInput :: (j -> i) -> Cell i h v -> Cell j h v
mapCellInput f = transformCellHandler (. f)

-- ** Transforming handlers

-- | Transform a cell handler using a state-universal function.
--
-- This is the most general variant of the handler transformation functions.
transformCellHandler ::
  (forall s. ((i -> State (v, s) h) -> (j -> State (v, s) g))) -> Cell i h v -> Cell j g v
transformCellHandler f (Cell (Cell_ v s go)) = cellWithState v s (f go)

-- | Map a cell's handler using a (covariant) function.
--
-- This conceptually turns `Cell i _ v` into a functor.
mapCellHandler :: (h -> g) -> Cell i h v -> Cell i g v
mapCellHandler f = transformCellHandler (fmap f .)

-- | Set a handler for a cell that doesn't have one yet
setCellHandler :: h -> Cell i () v -> Cell i h v
setCellHandler h = mapCellHandler (safeConst h)

-- | Monadic variant of 'mapCellHandler' for the common case where our handler transformation is of
-- shape "and then do something else with the result"
mapCellHandlerM :: (Monad m) => (a -> m b) -> Cell i (m a) v -> Cell i (m b) v
mapCellHandlerM act = mapCellHandler (>>= act)

-- | Variant of 'mapCellHandlerM' without an input
mapCellHandlerM_ :: (Monad m) => m b -> Cell i (m ()) v -> Cell i (m b) v
mapCellHandlerM_ act = mapCellHandlerM $ safeConst act

-- | Transform both a cell's input (contravariantly) and a cell's handler (covariantly), and the
-- handler transformation has the original input available.
mapCellHandlerInput :: (j -> i) -> (j -> h -> g) -> Cell i h v -> Cell j g v
mapCellHandlerInput f g = transformCellHandler $ \go x -> fmap (g x) . go . f $ x

-- | Monadic variant of 'mapCellHandlerInput' where our handler transformation is of shape "and then
-- do something else with the result", with the (contravariantly transformed) input available.
mapCellHandlerInputM :: (Monad m) => (j -> i) -> (j -> a -> m b) -> Cell i (m a) v -> Cell j (m b) v
mapCellHandlerInputM f g = mapCellHandlerInput f $ \j h -> h >>= g j

-- | Variant of 'mapCellHandlerInputM' without a result from the original handler.
--
-- This runs the original handler "in the background" and also lets us do things on top of it.
--
-- A common use case is when we wanna give our handler additional context as part of the input,
-- for example:
--
--     mapCellHandlerInputM_ fst (\(i, ctx) -> do_something_else i ctx)
mapCellHandlerInputM_ :: (Monad m) => (j -> i) -> (j -> m b) -> Cell i (m ()) v -> Cell j (m b) v
mapCellHandlerInputM_ f g = mapCellHandlerInput f $ \j h -> h >> g j

-- ** Combining cells

_11L :: Lens' ((a, b), (x, y)) (a, x)
_11L =
  lens
    (\((v1', _v2'), (s1', _s2')) -> (v1', s1'))
    (\((_v1', v2'), (_s1', s2')) (v1'', s1'') -> ((v1'', v2'), (s1'', s2')))

_22L :: Lens' ((a, b), (x, y)) (b, y)
_22L =
  lens
    (\((_v1', v2'), (_s1', s2')) -> (v2', s2'))
    (\((v1', _v2'), (s1', _s2')) (v2'', s2'') -> ((v1', v2''), (s1', s2'')))

-- | Combine two cells along inputs, handlers, and value.
pairCells :: Cell i1 h1 v1 -> Cell i2 h2 v2 -> Cell (i1, i2) (h1, h2) (v1, v2)
pairCells (Cell (Cell_ v1 s1 go1)) (Cell (Cell_ v2 s2 go2)) = cellWithState (v1, v2) (s1, s2) $ \(i1, i2) -> do
  h1 <- zoom _11L $ go1 i1
  h2 <- zoom _22L $ go2 i2
  return (h1, h2)

-- | Combine twc cells into a cell that updates one of the two and returns the respective handler.
-- The value is the pair of _both_ values at that point in time.
eitherCells :: Cell i1 h1 v1 -> Cell i2 h2 v2 -> Cell (Either i1 i2) (Either h1 h2) (v1, v2)
eitherCells (Cell (Cell_ v1 s1 go1)) (Cell (Cell_ v2 s2 go2)) = cellWithState (v1, v2) (s1, s2) $ \case
  Left i1 -> fmap Left . zoom _11L $ go1 i1
  Right i2 -> fmap Right . zoom _22L $ go2 i2

-- * Access

-- SOMEDAY these may not be needed.

-- | Get the value of a cell in a state monad
getValue :: (MonadState (Cell i h v) m) => m v
getValue = gets cValue

-- | Get the value of a Cell field in a state monad
getsValue :: (MonadState s m) => (s -> Cell i h a) -> m a
getsValue f = gets (cValue . f)

-- | Get the value of a lens pointing to a Cell in a state monad
useValue :: (MonadState s m) => Lens' s (Cell i h a) -> m a
useValue l = gets (cValue . view l)

-- * Updating

-- | Update a cell in a state monad.
--
-- NB You usually want to use 'runUpdateLens' instead.
updateCellState :: i -> State (Cell i h v) h
updateCellState x = do
  (Cell c_) <- get
  let theLens = lens (const c_) (const Cell)
  zoom (theLens . csValueStateL) $ csUpdate c_ x

-- | Given a lens pointing to a cell with monadic output type, supply a new value and run the
-- handler action.
--
-- This is how cells are most commonly used.
runUpdateLens :: (MonadState t m) => Lens' t (Cell i (m b) v) -> i -> m b
runUpdateLens l = join . liftState . zoom l . updateCellState

liftState :: (MonadState s m) => State s a -> m a
liftState = state . runState

-- | Apply a pure modification function to a cell inside a lens.
--
-- You typically use this with i == v (i.e., a Cell') but this is not necessary.
runModifyLens :: (MonadState t m) => Lens' t (Cell i (m b) v) -> (v -> i) -> m b
runModifyLens l f = join . liftState . zoom l $ do
  s <- gets cValue
  updateCellState (f s)
