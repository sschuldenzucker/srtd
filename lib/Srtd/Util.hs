-- | Collected simple utilities. Most of them are prob in extra but w/e.
module Srtd.Util where

import Control.Applicative (liftA2, (<|>))
import Control.Monad ((<=<))
import Control.Monad.Except
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Forest, Tree (..), foldTree)
import Lens.Micro.Platform (Lens')
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

-- | if-then-else as a function.
--
-- It's really not clear to me why _this_ is the one we don't have by default.
if' :: Bool -> a -> a -> a
if' b x y = if b then x else y

-- | case selection
select :: a -> [(Bool, a)] -> a
select xElse [] = xElse
select _ ((True, x) : _) = x
select xElse ((False, _) : cs) = select xElse cs

-- | Opposite of 'const'
ignore :: a -> b -> b
ignore = flip const

-- Enables regex support for Text

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

for :: [a] -> (a -> b) -> [b]
for = flip map

-- | If you use this for String, that's a good indicator you should probably be using Text instead.
replacePrefix :: (Eq a) => [a] -> [a] -> [a] -> [a]
replacePrefix needle repl haystack
  | needle `isPrefixOf` haystack = repl ++ drop (length needle) haystack
  | otherwise = haystack

-- | Transform a function partial on the empty list (like 'minimum') to a complete one by providing
-- a default.
forEmptyList :: b -> ([a] -> b) -> [a] -> b
forEmptyList dflt _ [] = dflt
forEmptyList _ f xs = f xs

-- | Like 'map' but with different functions for the first and last element.
mapFirstLast :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapFirstLast _ _ _ [] = []
mapFirstLast f g h (x : xs) = f x : mapLast g h xs

-- | Like 'map' but with a different function for the first element.
mapFirst :: (a -> b) -> (a -> b) -> [a] -> [b]
mapFirst _ _ [] = []
mapFirst f h (x : xs) = f x : map h xs

-- | Like 'map' but with a different function for the last element.
mapLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapLast _ _ [] = []
mapLast g _ [x] = [g x]
mapLast g h (x : xs) = h x : mapLast g h xs

-- * Basic tree helpers

-- | The sane `fmap` instance. (the default is the list instance, which isn't normally desired.)
--
-- Helper.
mapForest :: (a -> b) -> Forest a -> Forest b
mapForest f = map (fmap f)

-- | The catamorphism but for forests
foldForest :: (a -> [b] -> b) -> Forest a -> [b]
foldForest f = map (foldTree f)

-- | Number of nodes
treeSize :: Tree a -> Int
treeSize = foldTree $ \_ childSizes -> sum childSizes + 1

-- | Number of nodes
forestSize :: Forest a -> Int
forestSize = sum . map treeSize

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

-- | Top-down transformation. This is a specialization of 'transformForestDownUp'.
transformForestTopDown :: (Maybe b -> a -> b) -> Forest a -> Forest b
transformForestTopDown f = transformForestDownUp f (\res _ _ -> res)

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

leaf :: a -> Tree a
leaf x = Node x []

-- | Run forest modification function on the children of a tree.
onTreeChildren :: ([Tree a] -> [Tree a]) -> Tree a -> Tree a
onTreeChildren f (Node x children) = Node x (f children)

-- | Map forest modification function over each children of each tree. (go one level down)
onForestChildren :: ([Tree a] -> [Tree a]) -> [Tree a] -> [Tree a]
onForestChildren f = map (onTreeChildren f)

-- | All subtrees (with repetitions of children) with breadcrumbs (parents), in preorder.
forestTreesWithBreadcrumbs :: Forest a -> [([a], Tree a)]
forestTreesWithBreadcrumbs = concatMap (goTree [])
 where
  goTree crumbs n@(Node x children) = (crumbs, n) : concatMap (goTree (x : crumbs)) children

-- | Preorder nodes with their respecive levels
-- SOMEDAY unused, remove.
forestFlattenWithLevels :: Forest a -> [(Int, a)]
forestFlattenWithLevels = map extr . forestTreesWithBreadcrumbs
 where
  extr (crumbs, (Node x _)) = (length crumbs, x)

-- | Flatten a forest to a single-level forest where all nodes are toplevel. Preorder.
forestFlatten :: Forest a -> Forest a
forestFlatten = concatMap goTree
 where
  goTree (Node x children) = Node x [] : concatMap goTree children

forestFlattenToList :: Forest a -> [a]
forestFlattenToList = map rootLabel . forestFlatten

-- | Postorder variant of 'forestFlatten'
forestFlattenPostorder :: Forest a -> Forest a
forestFlattenPostorder = concatMap goTree
 where
  goTree (Node x children) = concatMap goTree children ++ [Node x []]

-- * Lens helpers

-- | Helper type to put lenses into data structures without `ImpredicativeTypes` or other weird &
-- dangerous options. (for some reason, `RankNTypes` is not enough for this.)
newtype ALens' a b = ALens' {runALens' :: Lens' a b}

-- We could define instances for Category and a combo function with Lens', but we don't use it rn.

-- * Monad helpers

-- | Lift an Either value into the monad computation of an 'ExceptT'.
pureET :: (Monad m) => Either e a -> ExceptT e m a
pureET ev = (ExceptT $ return ev)

-- * Regex helpers

regexSplitWithMatches :: Regex -> Text -> [(Bool, Text)]
regexSplitWithMatches regex input = go 0 matches
 where
  matches = getAllMatches (match regex input)
  go pos [] =
    if pos < T.length input
      then [(False, T.drop pos input)]
      else []
  go pos ((start, len) : rest) =
    let before_ = T.take (start - pos) (T.drop pos input)
        matched = T.take len (T.drop start input)
     in (if not (T.null before_) then [(False, before_)] else [])
          ++ [(True, matched)]
          ++ go (start + len) rest

-- * Text helpers

unsafeSingleDigitUIntToChar :: Int -> Char
unsafeSingleDigitUIntToChar i = case (show i) of
  [c] | '0' <= c && c <= '9' -> c
  _ -> error $ "unsafeSingleDigitUIntToChar: Expected 0-9, got " ++ (show i)
