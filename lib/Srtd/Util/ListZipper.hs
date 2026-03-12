{-| Helpers for 'Data.List.Zipper'.

The devs didn't make this an instance of the standard classes (Traversable, Foldable) etc., so we
need quite a bit here.
-}
module Srtd.Util.ListZipper where

import Data.List.Zipper qualified as LZ
import Data.Maybe (catMaybes)
import GHC.Stack (HasCallStack)

-- SOMEDAY review how much is actually used. It's kind of a lot.
-- SOMEDAY ... and if needed. E.g., lzMapM should just be traverse or something.
-- SOMEDAY maybe should have some tests for this, though I tested enough in prod.

-- | Map a monad function over a zipper. Effects propagate from the first to the last element.
--
-- 'LZ.Zipper' is unfortunately not an  instance of Traversable, o/w this could just be 'traverse'.
lzMapM :: (Monad m) => (a -> m b) -> LZ.Zipper a -> m (LZ.Zipper b)
lzMapM f z =
  let (back, front) = lzSplit z
      (mback, mfront) = (mapM f back, mapM f front)
   in lzFromFrontBack <$> mback <*> mfront

-- | Like 'catMaybes' but for a zipper. If the current item is nothing and possible, we move to the
-- left. We only return an invalid (post-end) zipper if the result is empty.
lzCatMaybesLeftNonEmpty :: LZ.Zipper (Maybe a) -> LZ.Zipper a
lzCatMaybesLeftNonEmpty = \case
  LZ.Zip rfront (Just x : back) -> LZ.Zip (catMaybes rfront) (x : catMaybes back)
  LZ.Zip rfront (Nothing : back) -> case (catMaybes rfront, catMaybes back) of
    (x : rfront', back') -> LZ.Zip rfront' (x : back')
    ([], back') -> LZ.Zip [] back'
  -- Post-end. This won't be hit in our caller; we do something reasonable (namely, produce another
  -- post-end zipper)
  LZ.Zip rfront [] -> LZ.Zip (catMaybes rfront) []

-- | Like 'LZ.delete' but move left after deletion, if possible. We only return an invalid zipper if
-- the result is empty. Only valid of nonempty zippers.
lzDeleteLeft :: LZ.Zipper a -> LZ.Zipper a
lzDeleteLeft = \case
  LZ.Zip (x : xs) (_ : ys) -> LZ.Zip xs (x : ys)
  LZ.Zip [] (_ : ys) -> LZ.Zip [] ys
  _ -> error "lzDeleteLeft: Invalid zipper"

-- | (part before the cursor, part including and after the cursor)
--
-- SOMEDAY would be easier & faster if I could access the zipper internals -.-
lzSplit :: LZ.Zipper a -> ([a], [a])
lzSplit (LZ.Zip rfront back) = (reverse rfront, back)

-- | (part before cursor, element at cursor, part after cursor). Error if at end.
lzSplit3 :: (HasCallStack) => LZ.Zipper a -> ([a], a, [a])
lzSplit3 (LZ.Zip rfront (cur : back)) = (reverse rfront, cur, back)
lzSplit3 _ = error "lzSplit3: Empty list"

lzFromFrontBack :: [a] -> [a] -> LZ.Zipper a
lzFromFrontBack front back = LZ.Zip (reverse front) back

lzForM :: (Monad m) => LZ.Zipper a -> (a -> m b) -> m (LZ.Zipper b)
lzForM = flip lzMapM

lzCircRight, lzCircLeft :: LZ.Zipper a -> LZ.Zipper a
lzCircRight z =
  let nxt = LZ.right z
   in if LZ.endp nxt then LZ.start z else nxt
lzCircLeft z = if LZ.beginp z then LZ.left (LZ.end z) else LZ.left z

lzSwapRightCirc, lzSwapLeftCirc :: LZ.Zipper a -> LZ.Zipper a
-- I'm feeling the zipper's API isn't all that useful.
lzSwapRightCirc (LZ.Zip rfront (cur : nxt : back)) = LZ.Zip (nxt : rfront) (cur : back)
lzSwapRightCirc (LZ.Zip rfront [cur]) = LZ.Zip [] (cur : reverse rfront)
lzSwapRightCirc z = z
lzSwapLeftCirc (LZ.Zip (prv : rfront) (cur : back)) = LZ.Zip rfront (cur : prv : back)
lzSwapLeftCirc (LZ.Zip [] (cur : back)) = LZ.Zip (reverse back) [cur]
lzSwapLeftCirc z = z

-- | Modify the current element by a function. Error if zipper is at its end.
lzModify :: (a -> a) -> LZ.Zipper a -> LZ.Zipper a
lzModify f z = LZ.replace (f $ LZ.cursor z) z

-- | Find the first position in the *list* where the predicate is true, or return the original
-- zipper unchanged if none.
lzFindBegin :: (a -> Bool) -> LZ.Zipper a -> LZ.Zipper a
lzFindBegin p z =
  let res = lzFind p (LZ.start z)
   in if LZ.endp res then z else res
 where
  -- Find the first following position where the predicate is true, or return the end position
  -- if none.
  lzFind p' z'
    | LZ.endp z' = z'
    | p' (LZ.cursor z') = z'
    | otherwise = lzFind p' (LZ.right z')
