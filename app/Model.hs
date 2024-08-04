{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import Control.Concurrent.STM
import Data.Aeson
import Data.Aeson.Types qualified as AT
import Data.Either (fromRight)
import Data.List (find)
import Data.Text qualified as Text
import Data.Tree
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics

-- import Data.UUID.V4 (nextRandom)

data EID = Inbox | Vault | EIDNormal (UUID) deriving (Eq, Ord, Show)

data Attr = Attr
  { name :: String
  }
  deriving (Show, Generic)

leaf :: a -> Tree a
leaf x = Node x []

-- TODO custom encoding for EID and Attr to get cleaner JSON?
type MForest = Forest (EID, Attr)

data Model = Model
  { forest :: MForest
  }
  deriving (Show, Generic)

instance ToJSON Attr where
  toEncoding = genericToEncoding defaultOptions

-- LATER As soon as we add more here, we prob want an implementation with optional fields (which can be configured somehow abstractly)
instance FromJSON Attr

-- We use a custom instance here to get more readable JSON.
instance ToJSON EID where
  -- SOMEDAY implement toEncoding, I can't be bothered rn.
  toJSON Inbox = String "INBOX"
  toJSON Vault = String "VAULT"
  toJSON (EIDNormal uuid) = String (UUID.toText uuid)

instance FromJSON EID where
  parseJSON (String txt)
    | txt == "INBOX" = return Inbox
    | txt == "VAULT" = return Vault
    | otherwise = case UUID.fromText txt of
        Just uuid -> return $ EIDNormal uuid
        Nothing -> fail $ "Invalid UUID: " ++ Text.unpack txt
  parseJSON val = AT.typeMismatch "String" val

instance ToJSON Model where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Model

emptyModel :: Model
emptyModel =
  Model
    [ leaf (Inbox, Attr "INBOX"),
      leaf (Vault, Attr "VAULT")
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

data Subtree = Subtree
  { breadcrumbs :: [EID],
    root :: EID,
    rootAttr :: Attr,
    stForest :: MForest
  }
  deriving (Show)

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

forestFindTree :: (Eq id) => id -> Forest (id, a) -> Maybe ([id], Tree (id, a))
forestFindTree tgt forest = find (\(_, Node (i, _) _) -> i == tgt) $ treesWithIdBreadcrumbs
  where
    -- Mogrify b/c forestTrees also returns the attrs, which we don't care about here.
    treesWithIdBreadcrumbs = map (\(bs, t) -> (map fst bs, t)) $ forestTrees forest

forestGetSubtreeBelow :: EID -> Forest (EID, Attr) -> Either IdNotFoundError Subtree
forestGetSubtreeBelow tgt forest = case forestFindTree tgt forest of
  Just (crumbs, (Node (i, attr) cs)) -> Right (Subtree crumbs i attr cs)
  Nothing -> Left IdNotFoundError

-- SOMEDAY unclear if this is the right structure.
newtype Filter = Filter {runFilter :: EID -> Model -> Subtree}

-- SOMEDAY maybe these should have a name actually. Also for display.
instance Show Filter where
  show _ = "<Filter>"

-- SOMEDAY decide on error handling
f_identity :: Filter
f_identity = Filter (\i m -> fromRight (error "root EID not found") $ modelGetSubtreeBelow i m)

-- SOMEDAY put this into a separate module

-- There's not actually a server here but we *may* want to make it one later.
newtype ModelServer = ModelServer (TVar Model)

instance Show ModelServer where
  show _ = "<ModelServer>"

getModel :: ModelServer -> IO Model
getModel (ModelServer mv) = readTVarIO mv

-- TODO this prob shouldn't exist
modifyModelOnServer :: ModelServer -> (Model -> Model) -> IO ()
modifyModelOnServer (ModelServer mv) f = atomically $ modifyTVar' mv f

-- TODO dummy
startModelServer :: IO ModelServer
startModelServer = do
  mv <- newTVarIO emptyModel
  return (ModelServer mv)
