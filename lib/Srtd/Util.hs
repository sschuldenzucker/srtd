-- | Collected simple utilities. Most of them are prob in extra but w/e.
module Srtd.Util where

import Control.Applicative (liftA2, (<|>))
import Control.Monad ((<=<))
import Data.Tree (Forest, Tree (..), foldTree)
import Srtd.Todo (todo)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err = maybe (Left err) Right

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- | This is really defined for anything with a neutral element, in this case the neutral element being `pure ()`
whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust mx f = maybe (pure ()) f mx

-- | The catamorphism for a pair of 'Maybe's.
maybe2 :: p -> (t1 -> p) -> (t2 -> p) -> (t1 -> t2 -> p) -> Maybe t1 -> Maybe t2 -> p
maybe2 n _ _ _ Nothing Nothing = n
maybe2 _ f _ _ (Just x) Nothing = f x
maybe2 _ _ g _ Nothing (Just y) = g y
maybe2 _ _ _ h (Just x) (Just y) = h x y

-- | Take whichever of the two arguments is 'Just' or, if given two, combine them using the given function.
unionMaybeWith :: (t -> t -> t) -> Maybe t -> Maybe t -> Maybe t
unionMaybeWith f a b = liftA2 f a b <|> a <|> b

-- | Changes the default comparison of 'Maybe' so that 'Nothing' is highest, not lowest
compareByNothingLast :: (t1 -> t2 -> Ordering) -> Maybe t1 -> Maybe t2 -> Ordering
compareByNothingLast cmp = maybe2 EQ (const LT) (const GT) cmp

-- | Compose a function n times with itself (0 times being the identity)
composeNTimes :: Int -> (a -> a) -> a -> a
composeNTimes n f = foldr (.) id $ replicate n f

-- | Monadic variant of `composeNTimes`.
composeNTimesM :: (Monad m) => Int -> (a -> m a) -> a -> m a
composeNTimesM n f = foldr (<=<) return $ replicate n f

-- | Choice functions based on ordering. Useful with nonstandard comparisons.
chooseMin, chooseMax :: Ordering -> a -> a -> a
chooseMin c x y = if c == GT then y else x
chooseMax c x y = if c == LT then y else x

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

-- | Forest variant of 'transformTreeDownUp' that maps over each tree independently. See there.
transformForestDownUp :: (Maybe u -> a -> u) -> (u -> a -> [b] -> b) -> Forest a -> Forest b
transformForestDownUp fdown gmake = map (transformTreeDownUp fdown gmake)

-- | Combined top-down and bottom-up transformation function.
--
-- Usage: `transformTreeDownUp fdown gmake` where:
--
-- - `fdown` maps the parent breadcrumb (if any) and the node label to a
--   resulting breadcrumb for the "down" part. The breadcrumb type is inferred.
-- - `gmake` maps the resulting breadcrumb, node label, and resulting child labels to the node's
--   own label.
--
-- Note: In `gmake`, the child labels likely include information derived from the current node. It's
-- the caller's responsibility to avoid any logical loops & unintended consequences resulting from
-- this. (but this will never result in an infinite loop)
--
-- SOMEDAY we could have an intermediate structure for the bottom-up step and a final combining step
-- to avoid the above issue. This could also help eliminate "helper" fields that are only required
-- for this process but not for the result. Right now, it's less convoluted to keep it like this.
--
-- SOMEDAY I could also make this a general folding function (yielding `b` instead of `Tree b`)
-- I think.
transformTreeDownUp :: (Maybe u -> a -> u) -> (u -> a -> [b] -> b) -> Tree a -> Tree b
transformTreeDownUp fdown gmake = _go Nothing
  where
    _go crumbs (Node x cs) =
      let crumb = fdown crumbs x
          crumbs' = Just crumb
          cs' = map (_go crumbs') cs
          clabels' = map rootLabel cs'
       in Node (gmake crumb x clabels') cs'

-- | Combined top-down and bottom-up transformation function. Mutually recursive.
--
-- The given transformation function accepts the result at the parent, the result at the children,
-- and the value at the current node to produce the result at the current node.
--
-- NOTE: It's *very easy* to produce circular dependencies here if you're not being careful, which
-- will lead to hangups. You typically wanna use this with a lazy data structure where one component
-- is inherited downwards (from the parent) and one is inherited upwards (from children), and then
-- maybe you have elements that depend on both results.
--
-- The main benefit of this structure is that you don't need separate data structures for
-- things-inherited-upwards and things-inherited-downwards.
transformTreeDownUpRec :: (Maybe b -> [b] -> a -> b) -> Tree a -> Tree b
transformTreeDownUpRec f = _go Nothing
  where
    _go mpar (Node x cs) =
      -- Mutually recursive group!
      let reslabel = f mpar clabels' x
          cs' = map (_go (Just reslabel)) cs
          clabels' = map rootLabel cs'
       in Node reslabel cs'

-- | See 'transformTreeDownUpRec'.
transformForestDownUpRec :: (Maybe b -> [b] -> a -> b) -> [Tree a] -> [Tree b]
transformForestDownUpRec f = map (transformTreeDownUpRec f)
