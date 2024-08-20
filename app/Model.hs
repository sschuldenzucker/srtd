{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

import Attr
-- Really just a helper here. Should prob not import this for separation
import Brick (suffixLenses)
import Control.Applicative (asum)
import Data.Aeson
import Data.Either (fromRight)
import Data.List (find, unfoldr)
import Data.Maybe (listToMaybe, mapMaybe)
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

-- SOMEDAY unclear if this is the right structure.
newtype Filter = Filter {runFilter :: EID -> Model -> Subtree}

-- SOMEDAY maybe these should have a name actually. Also for display.
instance Show Filter where
  show _ = "<Filter>"

-- SOMEDAY decide on error handling
f_identity :: Filter
f_identity = Filter (\i m -> fromRight (error "root EID not found") $ modelGetSubtreeBelow i m)

-- --------------------
-- Forest modifications
-- --------------------

-- TODO remove unused functions.

type Label = (EID, Attr)

moveSubtree' :: EID -> MWalker Label -> Model -> Model
moveSubtree' eid go = forestL %~ (forestMoveSubtreeId' eid go)

-- | Does nothing if the EID is not found.
forestMoveSubtreeId' :: (Eq id, Show id, Show a) => id -> MWalker (id, a) -> Forest (id, a) -> Forest (id, a)
forestMoveSubtreeId' tgt go forest = case zFindIdFirst tgt . Z.fromForest $ forest of
  Nothing -> forest
  Just loc -> zFullForest $ mzMove go loc

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

class ZFullForest t where
  zFullForest :: TreePos t a -> Forest a

instance ZFullForest Full where
  zFullForest = Z.forest . Z.first . Z.prevSpace . Z.root

instance ZFullForest Empty where
  zFullForest eloc = case Z.parent eloc of
    Just loc -> zFullForest loc
    Nothing -> Z.forest . Z.first $ eloc

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

type MWalker a = TreePos Empty a -> Maybe (TreePos Empty a)

toNextSibling, toPrevSibling, toBeforeParent, toAfterParent, toFirstChildOfNext, toFirstChildOfPrev, toLastChildOfNext, toLastChildOfPrev :: MWalker a
toNextSibling = Z.next
toPrevSibling = Z.prev
toBeforeParent = fmap Z.prevSpace . Z.parent
toAfterParent = fmap Z.nextSpace . Z.parent
toFirstChildOfNext = fmap Z.children . Z.nextTree
toFirstChildOfPrev = fmap Z.children . Z.prevTree
toLastChildOfNext = fmap (Z.last . Z.children) . Z.nextTree
toLastChildOfPrev = fmap (Z.last . Z.children) . Z.prevTree

-- Not sure if "preorder" is the right wording here; it doesn't actually recur upwards.
toBeforeNextPreorder, toAfterPrevPreorder :: MWalker a
toBeforeNextPreorder eloc = case Z.next eloc of
  res@(Just _eloc') -> res
  Nothing -> case Z.parent eloc of
    Just par -> Just $ Z.nextSpace par
    Nothing -> Nothing
toAfterPrevPreorder eloc = case Z.prev eloc of
  res@(Just _eloc') -> res
  Nothing -> case Z.parent eloc of
    Just par -> Just $ Z.prevSpace par
    Nothing -> Nothing
