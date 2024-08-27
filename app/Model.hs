{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Model stuff.
--
-- SOMEDAY this model has grown very large and should be split into sub-aspects.
-- Most importantly, tree modification operations should be split out.
module Model where

import Attr
-- Really just a helper here. Should prob not import this for separation
import Brick (suffixLenses)
import Control.Applicative (asum, (<|>))
import Data.Aeson
import Data.Either (fromRight)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (find, sortBy, unfoldr)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Tree
import Data.Tree.Zipper (Empty, Full, TreePos)
import Data.Tree.Zipper qualified as Z
import Data.UUID (UUID)
import GHC.Generics
import GHC.List (uncons)
import Lens.Micro.Platform
import Log
import ModelJSON qualified
import Todo

-- import Data.UUID.V4 (nextRandom)

leaf :: a -> Tree a
leaf x = Node x []

-- TODO custom encoding for EID and Attr to get cleaner JSON?
type MForest = Forest (EID, Attr)

data Model = Model
  { forest :: MForest
  }
  deriving (Show, Generic)

suffixLenses ''Model

-- We do *not* use the generic JSON instance b/c the ToJSON instance of Tree (provided by aeson)
-- makes for kinda messy JSON. It encodes the whole thing as a list (not an object). While we're
-- at it, we also transform the presentation of attr and id. The whole thing is a bit slow b/c we
-- transmogrify the whole structure.

modelToJSONModel :: Model -> ModelJSON.Model
modelToJSONModel (Model forest) = ModelJSON.Model forestJSON
  where
    forestJSON = map treeToJSONTree forest
    treeToJSONTree = foldTree $ \(i, attr) children -> ModelJSON.Tree i attr children

modelFromJSONModel :: ModelJSON.Model -> Model
modelFromJSONModel (ModelJSON.Model forestJSON) = Model forest
  where
    forest = jsonForestToForest forestJSON
    jsonForestToForest = unfoldForest $ \(ModelJSON.Tree i attr children) -> ((i, attr), children)

instance ToJSON Model where
  toJSON = toJSON . modelToJSONModel
  toEncoding = toEncoding . modelToJSONModel

instance FromJSON Model where
  parseJSON = fmap modelFromJSONModel . parseJSON

emptyModel :: Model
emptyModel =
  Model
    [ leaf (Inbox, attrMinimal "INBOX"),
      leaf (Vault, attrMinimal "VAULT")
    ]

-- SOMEDAY put these into a separate module

data InsertLoc id
  = Before id
  | After id
  | FirstChild id
  | LastChild id
  deriving (Show)

-- TODO these should have Either return types b/c it's possible that the loc node was deleted just before we were trying to insert.
-- Currently, insert does nothing in these cases.

-- | *You* need to make sure that the provided uuid is actually new, probably using UUID.nextRandom.
insertNewNormalWithNewId :: UUID -> Attr -> InsertLoc EID -> Model -> Model
insertNewNormalWithNewId uuid attr loc (Model forest) = Model (forestInsertAtId loc (EIDNormal uuid, attr) forest)

-- There's probably some clever monadic way of doing this but I'm not smart enough for that.

splitFind :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
splitFind p xs = case break p xs of
  (before, x : after) -> Just (before, x, after)
  _ -> Nothing

forestInsertAtId :: (Eq id) => InsertLoc id -> (id, a) -> Forest (id, a) -> Forest (id, a)
-- forestInsertAtId (Before tgt) idattr (n@(Node (i, _) _) : rst) | i == tgt = Node idattr [] : n : rst
-- forestInsertAtId (After tgt) idattr (n@(Node (i, _) _) : rst) | i == tgt = n : Node idattr [] : rst
forestInsertAtId loc@(Before tgt) idattr forest = case splitFind (treeHasID tgt) forest of
  Just (before, n, after) -> before ++ (Node idattr [] : n : after)
  Nothing -> map (treeInsertAtId loc idattr) forest
forestInsertAtId loc@(After tgt) idattr forest = case splitFind (treeHasID tgt) forest of
  Just (before, n, after) -> before ++ (n : Node idattr [] : after)
  Nothing -> map (treeInsertAtId loc idattr) forest
forestInsertAtId loc idattr forest = map (treeInsertAtId loc idattr) forest

treeHasID :: (Eq id) => id -> Tree (id, a) -> Bool
treeHasID tgt (Node (i, _) _) = tgt == i

treeInsertAtId :: (Eq id) => InsertLoc id -> (id, a) -> Tree (id, a) -> Tree (id, a)
treeInsertAtId (FirstChild tgt) idattr (Node (i, attr) children) | i == tgt = Node (i, attr) (Node idattr [] : children)
treeInsertAtId (LastChild tgt) idattr (Node (i, attr) children) | i == tgt = Node (i, attr) (children ++ [Node idattr []])
treeInsertAtId loc idattr (Node (i, attr) children) = Node (i, attr) (forestInsertAtId loc idattr children)

onTreeChildren :: ([Tree a] -> [Tree a]) -> Tree a -> Tree a
onTreeChildren f (Node x children) = Node x (f children)

onForestChildren :: ([Tree a] -> [Tree a]) -> [Tree a] -> [Tree a]
onForestChildren f = map (onTreeChildren f)

data MoveLoc
  = PrevSibling
  | NextSibling

moveSubtree :: EID -> MoveLoc -> Model -> Model
moveSubtree tgt loc (Model forest) = Model (forestMoveSubtreeId tgt loc forest)

forestMoveSubtreeId :: (Eq id) => id -> MoveLoc -> Forest (id, a) -> Forest (id, a)
forestMoveSubtreeId tgt loc@NextSibling forest = case splitFind (treeHasID tgt) forest of
  -- Just (prevs@(_ : _), n, nexts) -> let (prevs', p) = splitLast prevs in prevs' ++ [p, n] ++ nexts
  Just (prevs, n, nx : nexts') -> prevs ++ [nx, n] ++ nexts'
  Just _ -> forest
  Nothing -> onForestChildren (forestMoveSubtreeId tgt loc) forest
forestMoveSubtreeId tgt loc@PrevSibling forest = case splitFind (treeHasID tgt) forest of
  Just (prevs@(_ : _), n, nexts) -> let (prevs', p) = splitLast prevs in prevs' ++ [n, p] ++ nexts
  Just _ -> forest
  Nothing -> onForestChildren (forestMoveSubtreeId tgt loc) forest

splitLast :: [a] -> ([a], a)
splitLast xs = case uncons (reverse xs) of
  Just (lst, revinit) -> (reverse revinit, lst)
  Nothing -> error "splitLast: empty list"

deleteSubtree :: EID -> Model -> Model
deleteSubtree eid (Model forest) = Model (filterForest (\(eid', _) -> eid' /= eid) forest)

-- | Only leaves the initial segments of the forest where the predicate all applies.
filterForest :: (a -> Bool) -> Forest a -> Forest a
filterForest p forest = [Node x (filterForest p children) | Node x children <- forest, p x]

modifyAttrByEID :: EID -> (Attr -> Attr) -> Model -> Model
modifyAttrByEID tgt f = forestL %~ map (fmap updateContent)
  where
    updateContent (eid, attr) = (eid, if eid == tgt then f attr else attr)

data Subtree = Subtree
  { breadcrumbs :: [(EID, Attr)],
    root :: EID,
    rootAttr :: Attr,
    stForest :: MForest
  }
  deriving (Show)

suffixLenses ''Subtree

data IdNotFoundError = IdNotFoundError deriving (Show)

modelGetSubtreeBelow :: EID -> Model -> Either IdNotFoundError Subtree
modelGetSubtreeBelow i (Model forest) = forestGetSubtreeBelow i forest

-- SOMEDAY some of the following could probably be further abstracted away by supplying a condition on either the tree labels or subtrees themselves.

-- | All subtrees (with repetitions of children) with breadcrumbs (parents), in preorder.
forestTrees :: Forest a -> [([a], Tree a)]
forestTrees = concatMap (goTree [])
  where
    goTree crumbs n@(Node x children) = (crumbs, n) : concatMap (goTree (x : crumbs)) children

-- | Preorder nodes with their respecive levels
forestFlattenWithLevels :: Forest a -> [(Int, a)]
forestFlattenWithLevels = map extr . forestTrees
  where
    extr (crumbs, (Node x _)) = (length crumbs, x)

forestFindTree :: (Eq id) => id -> Forest (id, a) -> Maybe ([(id, a)], Tree (id, a))
forestFindTree tgt forest = find (\(_, Node (i, _) _) -> i == tgt) $ treesWithIdBreadcrumbs
  where
    -- Mogrify b/c forestTrees also returns the attrs, which we don't care about here.
    treesWithIdBreadcrumbs = forestTrees forest

forestGetSubtreeBelow :: EID -> Forest (EID, Attr) -> Either IdNotFoundError Subtree
forestGetSubtreeBelow tgt forest = case forestFindTree tgt forest of
  Just (crumbs, (Node (i, attr) cs)) -> Right (Subtree crumbs i attr cs)
  Nothing -> Left IdNotFoundError

-- SOMEDAY kinda inefficient, but everything here is, so w/e. Could also use forestTrees but, again, w/e.
forestGetParentsWhere :: (a -> Bool) -> Forest a -> [a]
forestGetParentsWhere p = mapMaybe trans . forestTrees
  where
    trans (par : _, (Node x _)) | p x = Just par
    trans _ = Nothing

listToMaybeId :: [(id, a)] -> Maybe id
listToMaybeId [(i, _)] = Just i
listToMaybeId _ = Nothing

forestGetParentId :: (Eq id) => id -> Forest (id, a) -> Maybe id
forestGetParentId tgt = listToMaybeId . forestGetParentsWhere (\(i, _) -> i == tgt)

forestGetNextSiblingId :: (Eq id) => id -> Forest (id, a) -> Maybe id
forestGetNextSiblingId tgt forest = case splitFind (treeHasID tgt) forest of
  Just (_, _, (Node (ret, _) _) : _) -> Just ret
  Just _ -> Nothing
  Nothing -> asum [forestGetNextSiblingId tgt cs | (Node _ cs) <- forest]

forestGetPrevSiblingId :: (Eq id) => id -> Forest (id, a) -> Maybe id
forestGetPrevSiblingId tgt forest = case splitFind (treeHasID tgt) forest of
  Just (prevs@(_ : _), _, _) -> let (Node (ret, _) _) = last prevs in Just ret
  Just _ -> Nothing
  Nothing -> asum [forestGetPrevSiblingId tgt cs | (Node _ cs) <- forest]

-- --------------------
-- Filter infra
-- --------------------

-- SOMEDAY unclear if this is the right structure.
data Filter = Filter
  { filterName :: String,
    filterRun :: EID -> Model -> Subtree
  }

-- SOMEDAY maybe these should have a name actually. Also for display.
instance Show Filter where
  show f = "<Filter " ++ filterName f ++ ">"

-- SOMEDAY decide on error handling
f_identity :: Filter
f_identity = Filter "all" f
  where
    f i m = fromRight (error "root EID not found") $ modelGetSubtreeBelow i m

-- | Hide completed tasks, top-down
--
-- TODO This is very simplistic: A non-completed task below a completed one is not shown (even though it seems important)
f_hide_completed :: Filter
f_hide_completed =
  let (Filter _ fi) = f_identity
      fi' i m = stFilter p $ fi i m
   in Filter "not done" fi'
  where
    p (_, Attr {status}) = status /= Just Done

stFilter :: ((EID, Attr) -> Bool) -> Subtree -> Subtree
stFilter p = stForestL %~ (filterForest p)

-- --------------------
-- Forest modifications
-- --------------------

-- TODO remove unused functions.

type Label = (EID, Attr)

moveSubtree' :: EID -> MoveWalker Label -> Model -> Model
moveSubtree' eid go = forestL %~ (forestMoveSubtreeId' eid go)

moveSubtreeBelow' :: EID -> EID -> MoveWalker Label -> Model -> Model
moveSubtreeBelow' root tgt go = forestL %~ (forestMoveSubtreeIdBelow' root tgt go)

-- | Does nothing if the EID is not found.
forestMoveSubtreeId' :: (Eq id) => id -> MoveWalker (id, a) -> Forest (id, a) -> Forest (id, a)
forestMoveSubtreeId' tgt go forest = case zFindIdFirst tgt . Z.fromForest $ forest of
  Nothing -> forest
  Just loc -> zFullForest $ mzMove go loc

-- | Like forestMoveSubtreeId' but only consider the subtree below `root`.
forestMoveSubtreeIdBelow' :: (Eq id) => id -> id -> MoveWalker (id, a) -> Forest (id, a) -> Forest (id, a)
forestMoveSubtreeIdBelow' root tgt go forest =
  -- SOMEDAY not really a deep reason to use zippers here other than that it's easy.
  case zFindIdFirst root . Z.fromForest $ forest of
    Nothing -> forest
    Just rootLoc -> zFullForest . Z.modifyTree (onTreeChildren $ forestMoveSubtreeId' tgt go) $ rootLoc

-- | The zipper above the toplevel
mForestZipper :: Model -> TreePos Empty Label
mForestZipper = Z.fromForest . forest

-- TODO unused
zChildList :: TreePos Full a -> [TreePos Full a]
zChildList = zFollowingTrees . Z.children

zFollowingTrees :: TreePos Empty a -> [TreePos Full a]
zFollowingTrees epos = case Z.nextTree epos of
  Nothing -> []
  Just pos -> pos : unfoldr go pos
  where
    go = fmap dup . Z.next
    dup x = (x, x)

class ZDescendants t where
  zDescendants :: TreePos t a -> [TreePos Full a]

instance ZDescendants Full where
  zDescendants pos = pos : zDescendants (Z.children pos)

instance ZDescendants Empty where
  zDescendants epos = zFollowingTrees epos >>= zDescendants

zFindLabelFirst :: (ZDescendants t) => (b -> Bool) -> TreePos t b -> Maybe (TreePos Full b)
zFindLabelFirst lp = zFindFirst $ lp . Z.label
  where
    zFindFirst :: (ZDescendants t) => (TreePos Full a -> Bool) -> TreePos t a -> Maybe (TreePos Full a)
    zFindFirst p = listToMaybe . filter p . zDescendants

-- NB This is probably pretty inefficient b/c we construct and iterate through positions, not trees.
--
-- (the zipper doesn't "know" we're not using intermediate results and we don't have closer access
-- to the underlying data b/c it's not exported :/)
-- SOMEDAY if it ever becomes a bottleneck, copy rosezipper here and add zFindLabelFirst, but more efficiently (or zFindTreeFirst more generally actually) - And then prob also optimized zFullForest etc.
zFindIdFirst :: (ZDescendants t, Eq id) => id -> TreePos t (id, b) -> Maybe (TreePos Full (id, b))
zFindIdFirst tgt = zFindLabelFirst $ \(i, _) -> i == tgt

-- SOMEDAY may be obsolete, or can be simplified (actually vaporized) using ZForestRoot.
class ZFullForest t where
  zFullForest :: TreePos t a -> Forest a

instance ZFullForest Full where
  zFullForest = Z.forest . Z.first . Z.prevSpace . Z.root

instance ZFullForest Empty where
  zFullForest eloc = case Z.parent eloc of
    Just loc -> zFullForest loc
    Nothing -> Z.forest . Z.first $ eloc

class ZForestRoot t where
  -- | The "root" of the forest, i.e., the space before the first child at the toplevel.
  zForestRoot :: TreePos t a -> TreePos Empty a

instance ZForestRoot Full where
  zForestRoot = Z.first . Z.prevSpace . Z.root

instance ZForestRoot Empty where
  zForestRoot eloc = case Z.parent eloc of
    Just loc -> zForestRoot loc
    Nothing -> Z.first eloc

-- SOMEDAY clean up the above manual code which should then be unused.
-- SOMEDAY also implement deletion using this infra.
-- SOMEDAY single-deletion and move-single.
-- For single-deletion, maybe I need to fork the zipper (i.e., copy the module). Could also help implementing better find-at ops.
--
-- Do we even *need* move-single? Or do we really need just a specific slightly strange operation?
-- What is the equivalent for a "normal" nested list?
-- -> Don't implement for now except for *maybe* indent/dedent. Then review later based on usage.

-- | Take the tree out of the current position, walk from the hole left via the given fct, and put it there.
-- Returns the new position that we've moved to.
zMove :: (TreePos Empty a -> TreePos Empty a) -> TreePos Full a -> TreePos Full a
zMove go pos = Z.insert (Z.tree pos) . go . Z.delete $ pos

-- | Like `zMove`, but the walking function may fail and then we do nothing.
mzMove :: (TreePos Empty a -> Maybe (TreePos Empty a)) -> TreePos Full a -> TreePos Full a
mzMove go pos = case go . Z.delete $ pos of
  Nothing -> pos
  Just epos' -> Z.insert (Z.tree pos) epos'

-- | Walks from an empty position (of a just-removed node) to a new empty position (where the node is to be moved), and may fail at that.
type MoveWalker a = TreePos Empty a -> Maybe (TreePos Empty a)

toNextSibling, toPrevSibling, toBeforeParent, toAfterParent, toFirstChildOfNext, toFirstChildOfPrev, toLastChildOfNext, toLastChildOfPrev :: MoveWalker a
toNextSibling = Z.next
toPrevSibling = Z.prev
toBeforeParent = fmap Z.prevSpace . Z.parent
toAfterParent = fmap Z.nextSpace . Z.parent
toFirstChildOfNext = fmap Z.children . Z.nextTree
toFirstChildOfPrev = fmap Z.children . Z.prevTree
toLastChildOfNext = fmap (Z.last . Z.children) . Z.nextTree
toLastChildOfPrev = fmap (Z.last . Z.children) . Z.prevTree

-- Not sure if "preorder" is the right wording here; it doesn't actually recur upwards.
-- SOMEDAY I'm sure there's a monad or something that does these alternatives.
toBeforeNextPreorder, toAfterPrevPreorder :: MoveWalker a
toBeforeNextPreorder eloc =
  toBeforeFirstChildOfNext eloc
    <|> Z.next eloc
    <|> Z.nextSpace <$> Z.parent eloc
  where
    -- Like toFirstChildOfNext but fails if there is no first child. This is intuitive for some use cases.
    toBeforeFirstChildOfNext eloc = do
      nxt <- Z.nextTree eloc
      fc <- Z.firstChild nxt
      return $ Z.prevSpace fc
toAfterPrevPreorder eloc =
  toAfterLastChildOfPrev eloc
    <|> Z.prev eloc
    <|> Z.prevSpace <$> Z.parent eloc
  where
    -- Like toLastChildOfPrev but fails if there is no first child. This is intuitive for some use cases.
    toAfterLastChildOfPrev eloc = do
      nxt <- Z.prevTree eloc
      fc <- Z.lastChild nxt
      return $ Z.nextSpace fc

-- | Walks from a node position to another node position and may fail at that.
type GoWalker a = TreePos Full a -> Maybe (TreePos Full a)

-- The following are a trivial abstraction and could be eliminated, but we keep them for now to
-- abstract the tree implementation away.
goNextSibling, goPrevSibling, goParent :: GoWalker a
goNextSibling = Z.next
goPrevSibling = Z.prev
goParent = Z.parent

-- | Walks from a node position to an empty position where a new tree should be inserted.
--
-- TODO is it required that this can fail, or can we simplify?
type InsertWalker a = TreePos Full a -> Maybe (TreePos Empty a)

-- The following are a trivial abstraction and could be eliminated, but we keep them for now to
-- abstract the tree implementation away.
insBefore, insAfter, insFirstChild, insLastChild :: InsertWalker a
insBefore = Just . Z.prevSpace
insAfter = Just . Z.nextSpace
insFirstChild = Just . Z.children
insLastChild = Just . Z.last . Z.children

-- | See `forestMoveSubtreeRelFromForest`
moveSubtreeRelFromForest :: EID -> GoWalker (EID, a) -> InsertWalker Label -> Forest (EID, a) -> Model -> Model
moveSubtreeRelFromForest tgt go ins haystack = forestL %~ (forestMoveSubtreeRelFromForest tgt go ins haystack)

-- | When called like `forestMoveSubtreeRelFromForest tgt go ins haystack forest`, this doesn the following:
--
-- 1. Apply `go` to the position of `tgt` in `haystack` to find an `anchor` node, identified by its ID.
-- 2. Consider that ID in `forest` and apply `ins` to find a position to which `tgt` should be moved.
-- 3. Move `tgt` there in `forest` and return the result.
--
-- This silently fails if one of the IDs cannot be found OR if one of the walkers fails.
--
-- SOMEDAY the former is actually an error. The latter is not.
forestMoveSubtreeRelFromForest :: (Eq id) => id -> GoWalker (id, a) -> InsertWalker (id, b) -> Forest (id, a) -> Forest (id, b) -> Forest (id, b)
forestMoveSubtreeRelFromForest tgt go ins haystack forest = fromMaybe forest $ do
  tgtLoc <- zFindIdFirst tgt haystackLoc
  anchorLoc <- go tgtLoc
  let (anchorId, _) = Z.label anchorLoc
  return $ forestMoveSubtreeIdRelToAnchor tgt anchorId ins forest
  where
    haystackLoc = Z.fromForest haystack

-- | Given a `tgt` and an `anchor` and a walker to go from the anchor to an insert position, move `tgt` there.
--
-- `anchor` cannot be `tgt` or one of its descendants. This will silently fail.
--
-- SOMEDAY Instead of silent failure, it's actually an *error* if any of the nodes are not found (except maybe in `go`)
forestMoveSubtreeIdRelToAnchor :: (Eq id) => id -> id -> InsertWalker (id, a) -> Forest (id, a) -> Forest (id, a)
forestMoveSubtreeIdRelToAnchor tgt anchor go forest = fromMaybe forest $ do
  tgtLoc <- zFindIdFirst tgt forestLoc
  let tgtTree = Z.tree tgtLoc
  let forestLoc' = zForestRoot . Z.delete $ tgtLoc
  anchorLoc <- zFindIdFirst anchor forestLoc'
  insertLoc <- go anchorLoc
  let forest'' = Z.forest . zForestRoot . Z.insert tgtTree $ insertLoc
  return forest''
  where
    forestLoc = Z.fromForest forest

-- ------------------
-- Sorting (physical)
-- ------------------

onForestBelowId :: (Eq id) => id -> (Forest (id, a) -> Forest (id, a)) -> Forest (id, a) -> Forest (id, a)
-- This is probably a bit inefficient but it was there so w/e
-- SOMEDAY it's actually an error if root is not found.
onForestBelowId root f forest = case zFindIdFirst root . Z.fromForest $ forest of
  Nothing -> forest
  Just rootLoc -> Z.forest . zForestRoot . Z.modifyTree (onTreeChildren f) $ rootLoc

sortShallowBelow :: (Attr -> Attr -> Ordering) -> EID -> Model -> Model
sortShallowBelow ord root = forestL %~ onForestBelowId root (sortBy ord')
  where
    ord' (Node (_, attr1) _) (Node (_, attr2) _) = ord attr1 attr2

sortDeepBelow :: (Attr -> Attr -> Ordering) -> EID -> Model -> Model
sortDeepBelow ord root = forestL %~ onForestBelowId root deepSortByOrd
  where
    -- prob kinda inefficient but I got <= 10 levels or something.
    deepSortByOrd = onForestChildren deepSortByOrd . sortBy ord'
    ord' (Node (_, attr1) _) (Node (_, attr2) _) = ord attr1 attr2
