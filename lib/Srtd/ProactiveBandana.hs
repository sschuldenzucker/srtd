{-| An imperative events handlers framework.

This is basically a thin upgrade over an imperative setter function, but provides a single point
of entry and some basic consistency enforcement.

You probably wanna import this qualified.
-}
module Srtd.ProactiveBandana (
  -- * Types
  CellUpdate,
  CellUpdate',
  Cell (..),
  Cell',

  -- * Conversion
  cell,

  -- * Construction
  justCall,
  simple,
  simpleOldNew,
  simpleMap,
  simplePreMap,
  store,
  storeM,
  unique,
  uniqueM,
  uniqueMaybe,
  uniqueMaybeM,

  -- * Modification
  mapInput,
  mapHandler,
  setHandler,
  bindHandler,
  bindHandler_,
  mapInputHandler,

  -- * Combinators
  pair,
  pairM_,
  either2,
  either2_,
  either1,
  either1_,

  -- * Convenience Access
  getsValue,
  useValue,

  -- * Updating
  updateCellState,
  runUpdateLens,
  runModifyLens,
) where

import Brick (suffixLenses)
import Control.Monad (join)
import Control.Monad.State
import Lens.Micro.Platform
import Srtd.Util (fromEither, safeConst)

-- * Type

-- | Type of cell updates.
--
-- The usual workflow is to construct an update, either raw or using our simple combinators, then
-- use 'cell' to make a 'Cell' out of that. You can also re-use existing cells by reading
-- 'cUpdate'.
--
-- Note that this is conceptually a monad, namely `ReaderT (State v) h`. We don't implement this
-- though, b/c it's not that useful.
type CellUpdate i v h = i -> State v h

-- | The common case where the input is equal to the value type.
type CellUpdate' v h = CellUpdate v v h

-- | An imperative-reactive cell that (optionally) returns a value on update. The value is often a
-- monadic action to notify others.
--
-- - `i` is the input type: updates input that type into the cell.
-- - `h` is the handler type: updates result in that value being output. Usually, `h = m ()` where
--    `m` is some monad. Usually, `m` is another state monad and then we can run the update and the
--    returned action via 'runUpdateALens'.
-- - `v` is the type of the stored value.
--
-- NB An earlier version of this had some internal (erased) state type s and could keep it but we
-- never used this. Could bring it back. In that case, it's prob best to _only_ store the internal
-- state and make cValue just a view into the state. This is to enforce consistency in combinators.
data Cell i v h = Cell
  { cValue :: v
  , cUpdate :: CellUpdate i v h
  }

suffixLenses ''Cell

-- | A cell with input == value, which is common.
type Cell' v h = Cell v v h

-- | Construct a cell from initial value and update function
cell :: v -> CellUpdate i v h -> Cell i v h
cell = Cell

-- | Just call the handler and doesn't store any state.
justCall :: (i -> h) -> CellUpdate i v h
justCall go = return . go

-- | Store the input and call the handler on each update.
simple :: (v -> h) -> CellUpdate' v h
simple go = \i -> put i >> return (go i)

-- | Like 'sipmle' but the handler also receives the _old_ value that just got replaced, in addition
-- to the new one.
simpleOldNew :: (v -> v -> h) -> CellUpdate' v h
simpleOldNew go = \i -> do
  old <- get
  put i
  return (go old i)

-- | A variant of 'simple' that maps its input to its value using a function.
--
-- We have `simple x0 g == simpleMap x0 id g`
simpleMap :: (i -> v) -> (v -> h) -> CellUpdate i v h
simpleMap f go = \i ->
  let v = f i
   in put v >> return (go v)

-- | Like 'simpleMap' but the handler depends on the _input_, not the mapped value.
--
-- May be useful when the input contains some kind of context for the handler to run, but we don't
-- need to store this.
simplePreMap :: (i -> v) -> (i -> h) -> CellUpdate i v h
simplePreMap f go = \i -> put (f i) >> return (go i)

-- | Just store the input, with no handler.
store :: CellUpdate' v ()
store = simple (const ())

-- | Just store the input, do nothing as handler
storeM :: (Monad m) => CellUpdate' v (m ())
storeM = simple (const $ return ())

-- | Store the input and call the handler _iff_ the input has changed. Otherwise a given default.
unique :: (Eq v) => h -> (v -> h) -> CellUpdate' v h
unique dflt f = \x' -> do
  x <- get
  if
    | x /= x' -> put x' >> return (f x')
    | otherwise -> return $ dflt

-- | Monadic variant of 'unique' that does nothing as a default.
uniqueM :: (Eq v, Monad m) => (v -> m ()) -> CellUpdate' v (m ())
uniqueM = unique (return ())

-- | Store a Maybe value and call the handler _iff_ the input is Just and changed. Otherwise a given
-- default.
--
-- This is useful when Nothing means "invalid". The cell then stores the last valid input and calls
-- back on each new valid input.
uniqueMaybe :: (Eq v) => h -> (v -> h) -> CellUpdate' (Maybe v) h
uniqueMaybe dflt f = \mx' -> do
  mx <- get
  case (mx, mx') of
    (Nothing, Just x') -> put (Just x') >> return (f x')
    (Just x, Just x')
      | x == x' -> put (Just x') >> return (f x')
      | otherwise -> return dflt
    (_, Nothing) -> return dflt

uniqueMaybeM :: (Monad m, Eq v) => (v -> m ()) -> CellUpdate' (Maybe v) (m ())
uniqueMaybeM = uniqueMaybe (return ())

-- | Map a cell's input using a (contravariant) function.
--
-- This conceptually turns `Cell _ v h` into a contravariant functor.
mapInput :: (j -> i) -> CellUpdate i v h -> CellUpdate j v h
mapInput f = (. f)

-- | Map a cell's handler using a (covariant) function.
--
-- This conceptually turns `Cell i _ v` into a functor.
mapHandler :: (h -> g) -> CellUpdate i v h -> CellUpdate i v g
mapHandler f = (fmap f .)

-- | Transform both a cell's input (contravariantly) and a cell's handler (covariantly), and the
-- handler transformation has the original input available.
--
--
-- A common use case is when we wanna give our handler additional context as part of the input,
-- for example:
--
--     mapInputHandler fst (\(i, ctx) h -> h >> do_something_else i ctx)
mapInputHandler :: (j -> i) -> (j -> h -> g) -> CellUpdate i v h -> CellUpdate j v g
mapInputHandler f g up = \x -> fmap (g x) . up . f $ x

-- | Set a handler if there isn't any yet.
setHandler :: h -> CellUpdate i v () -> CellUpdate i v h
setHandler h = mapHandler (safeConst h)

-- | Monadic bind in handlers.
bindHandler :: (Monad m) => (a -> m b) -> CellUpdate i v (m a) -> CellUpdate i v (m b)
bindHandler f = mapHandler (>>= f)

-- | Monadic bind in handlers, ignoring return of the existing handler.
bindHandler_ :: (Monad m) => m b -> CellUpdate i v (m ()) -> CellUpdate i v (m b)
bindHandler_ act = mapHandler (>> act)

{- -- There's probably a smarter way to do this...
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

_k1L :: Lens' (a, (x, y)) (a, x)
_k1L f (a, (x, y)) =
  (\(a', x') -> (a', (x', y))) <$> f (a, x)

_k2L :: Lens' (a, (x, y)) (a, y)
_k2L f (a, (x, y)) =
  (\(a', y') -> (a', (x, y'))) <$> f (a, y) -}

-- | Combine two cells along inputs, handlers, and value.
pair :: CellUpdate i1 v1 h1 -> CellUpdate i2 v2 h2 -> CellUpdate (i1, i2) (v1, v2) (h1, h2)
pair go1 go2 = \(i1, i2) -> do
  h1 <- zoom _1 $ go1 i1
  h2 <- zoom _2 $ go2 i2
  return (h1, h2)

-- | Monadic variant of 'pair' where we execute both handlers.
pairM_ ::
  (Monad m) =>
  CellUpdate i1 v1 (m ()) -> CellUpdate i2 v2 (m ()) -> CellUpdate (i1, i2) (v1, v2) (m ())
pairM_ go1 go2 = mapHandler (uncurry (>>)) $ pair go1 go2

-- | Combine twc cells into a cell that updates one of the two and returns the respective handler.
-- The value is the pair of _both_ values at that point in time.
either2 ::
  CellUpdate i1 v1 h1 -> CellUpdate i2 v2 h2 -> CellUpdate (Either i1 i2) (v1, v2) (Either h1 h2)
either2 go1 go2 = \case
  Left i1 -> fmap Left . zoom _1 $ go1 i1
  Right i2 -> fmap Right . zoom _2 $ go2 i2

-- | Variant of 'either2' where we collapse the two handlers into one.
either2_ ::
  CellUpdate i1 v1 h -> CellUpdate i2 v2 h -> CellUpdate (Either i1 i2) (v1, v2) h
either2_ go1 go2 = mapHandler fromEither $ either2 go1 go2

-- | Accept an 'Either' input and choose the state update and the handler based on whether it's
-- 'Left' or 'Right'. In contrast to 'either2', input always modifies the same state.
either1 ::
  CellUpdate i1 v h1 -> CellUpdate i2 v h2 -> CellUpdate (Either i1 i2) v (Either h1 h2)
either1 go1 go2 = \case
  Left i1 -> fmap Left $ go1 i1
  Right i2 -> fmap Right $ go2 i2

-- | Variant of 'either1' where we collapse the two handlers into one. This is the most common way
-- to have different "kinds" of inputs.
either1_ ::
  CellUpdate i1 v h -> CellUpdate i2 v h -> CellUpdate (Either i1 i2) v h
either1_ go1 go2 = mapHandler fromEither $ either1 go1 go2

-- * Access

-- | Get the value of a Cell field in a state monad
getsValue :: (MonadState s m) => (s -> Cell i v h) -> m v
getsValue f = gets (cValue . f)

-- | Get the value of a lens pointing to a Cell in a state monad
useValue :: (MonadState s m) => Lens' s (Cell i v h) -> m v
useValue l = gets (cValue . view l)

-- * Updating

-- | Update a cell in a state monad.
--
-- NB You usually want to use 'runUpdateLens' instead.
updateCellState :: i -> State (Cell i v h) h
updateCellState x = do
  c <- get
  zoom cValueL $ cUpdate c x

-- | Given a lens pointing to a cell with monadic output type, supply a new value and run the
-- handler action.
--
-- This is how cells are most commonly used.
runUpdateLens :: (MonadState t m) => Lens' t (Cell i v (m a)) -> i -> m a
runUpdateLens l = join . liftState . zoom l . updateCellState

liftState :: (MonadState s m) => State s a -> m a
liftState = state . runState

-- | Apply a pure modification function to a cell inside a lens.
--
-- You typically use this with i == v (i.e., a Cell') but this is not necessary.
runModifyLens :: (MonadState t m) => Lens' t (Cell i v (m b)) -> (v -> i) -> m b
runModifyLens l f = join . liftState . zoom l $ do
  s <- gets cValue
  updateCellState (f s)
