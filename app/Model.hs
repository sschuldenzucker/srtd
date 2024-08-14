{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

import Attr
-- Really just a helper here. Should prob not import this for separation
import Brick (suffixLenses)
import Data.Aeson
import Data.Either (fromRight)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Tree
import Data.UUID (UUID)
import GHC.Generics
import Lens.Micro.Platform
import Log
import ModelJSON qualified

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

forestGetParentId :: (Eq id) => id -> Forest (id, a) -> Maybe id
forestGetParentId tgt forest = case forestGetParentsWhere (\(i, _) -> i == tgt) forest of
  [(res, _)] -> Just res
  _ -> Nothing

-- SOMEDAY unclear if this is the right structure.
newtype Filter = Filter {runFilter :: EID -> Model -> Subtree}

-- SOMEDAY maybe these should have a name actually. Also for display.
instance Show Filter where
  show _ = "<Filter>"

-- SOMEDAY decide on error handling
f_identity :: Filter
f_identity = Filter (\i m -> fromRight (error "root EID not found") $ modelGetSubtreeBelow i m)
