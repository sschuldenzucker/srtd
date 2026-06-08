module Test.ModelMoveTests (modelMoveTests) where

import Data.Maybe (fromMaybe)
import Data.Time
import Data.Tree (Forest, Tree (..))
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Srtd.Attr
import Srtd.Data.IdTree
import Srtd.Data.TreeZipper
import Srtd.Model
import Test.Tasty
import Test.Tasty.HUnit

modelMoveTests :: TestTree
modelMoveTests =
  testGroup
    "Model move"
    [ testCase "moveSubtreeRelToAnchor supports child and sibling positions" testMoveSubtreeRelToAnchorPositions
    , testCase "moveSubtreeRelToAnchor no-ops for self and descendant anchors" testMoveSubtreeRelToAnchorInvalidTargets
    ]

testMoveSubtreeRelToAnchorPositions :: Assertion
testMoveSubtreeRelToAnchorPositions = do
  let asLastChild = update (moveSubtreeRelToAnchor eidA eidC insLastChild) baseModel
  childIds Vault asLastChild @?= [eidC]
  childIds eidC asLastChild @?= [eidA]

  let asFirstChild = update (moveSubtreeRelToAnchor eidC eidA insFirstChild) baseModel
  childIds Vault asFirstChild @?= [eidA]
  childIds eidA asFirstChild @?= [eidC, eidB]

  let beforeAnchor = update (moveSubtreeRelToAnchor eidC eidA insBefore) baseModel
  childIds Vault beforeAnchor @?= [eidC, eidA]

  let afterAnchor = update (moveSubtreeRelToAnchor eidA eidC insAfter) baseModel
  childIds Vault afterAnchor @?= [eidC, eidA]

testMoveSubtreeRelToAnchorInvalidTargets :: Assertion
testMoveSubtreeRelToAnchorInvalidTargets = do
  let belowSelf = update (moveSubtreeRelToAnchor eidA eidA insLastChild) baseModel
  modelSummary belowSelf @?= modelSummary baseModel

  let belowDescendant = update (moveSubtreeRelToAnchor eidA eidB insLastChild) baseModel
  modelSummary belowDescendant @?= modelSummary baseModel

mkModel :: DiskModel -> Model
mkModel diskModel =
  let ?mue = testMUE
   in diskModelToModel diskModel

update :: ((?mue :: ModelUpdateEnv) => Model -> Model) -> Model -> Model
update f model =
  let ?mue = testMUE
   in f model

testMUE :: ModelUpdateEnv
testMUE = ModelUpdateEnv (hoursToTimeZone 0)

baseModel :: Model
baseModel =
  mkModel . DiskModel . IdForest $
    [ leaf' Inbox "INBOX" []
    , leaf' Vault "VAULT" [leaf' eidA "A" [leaf' eidB "B" []], leaf' eidC "C" []]
    , leaf' Clipboard "CLIPBOARD" []
    ]

leaf' :: EID -> String -> Forest (EID, Attr) -> Tree (EID, Attr)
leaf' eid label children = Node (eid, attr label) children

attr :: String -> Attr
attr = attrMinimal fixedTime

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2024 1 1) 0

childIds :: EID -> Model -> [EID]
childIds eid (Model forest) =
  case forestFindTree eid forest of
    Just (Node _ children) -> [eid' | Node (eid', _) _ <- children]
    Nothing -> []

modelSummary :: Model -> [TreeSummary]
modelSummary (Model (IdForest forest)) = map treeSummary forest

data TreeSummary = TreeSummary EID String [TreeSummary]
  deriving (Eq, Show)

treeSummary :: Tree IdLabel -> TreeSummary
treeSummary (Node (eid, (attr', _)) children) = TreeSummary eid (name attr') (map treeSummary children)

eidA, eidB, eidC :: EID
eidA = EIDNormal uuidA
eidB = EIDNormal uuidB
eidC = EIDNormal uuidC

uuidA, uuidB, uuidC :: UUID
uuidA = uuid "00000000-0000-0000-0000-000000000001"
uuidB = uuid "00000000-0000-0000-0000-000000000002"
uuidC = uuid "00000000-0000-0000-0000-000000000003"

uuid :: String -> UUID
uuid = fromMaybe (error "invalid test UUID") . UUID.fromString
