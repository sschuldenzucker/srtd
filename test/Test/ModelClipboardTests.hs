module Test.ModelClipboardTests (modelClipboardTests) where

import Data.Aeson (decode, encode)
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

modelClipboardTests :: TestTree
modelClipboardTests =
  testGroup
    "Model clipboard"
    [ testCase "CLIPBOARD EID JSON round-trips" $
        (decode . encode $ Clipboard) @?= Just Clipboard
    , testCase "normalizes older disk models with missing CLIPBOARD" $
        case decode (encode oldDiskModel) of
          Just diskModel -> hasTopLevel Clipboard (mkModel diskModel) @?= True
          Nothing -> assertFailure "failed to decode old disk model"
    , testCase "clipboardSize counts direct clipboard children" testClipboardSize
    , testCase "copy appends clone with refreshed normal IDs" testCopyRefreshesIds
    , testCase "copy ignores extra fresh IDs" testCopyIgnoresExtraIds
    , testCase "copy with too few fresh IDs is a no-op" testCopyTooFewIdsNoops
    , testCase "cut appends original subtree with IDs preserved" testCutPreservesIds
    , testCase "paste consumes FIFO entry after current item" testPasteAfterConsumesFifo
    , testCase "paste supports before and child positions" testPastePositions
    , testCase "empty clipboard paste is a no-op" testEmptyClipboardPaste
    ]

testClipboardSize :: Assertion
testClipboardSize = do
  clipboardSize baseModel @?= 0
  clipboardSize pasteModel @?= 2
  clipboardSize nestedClipboardModel @?= 1

testCopyRefreshesIds :: Assertion
testCopyRefreshesIds = do
  let model' = update (copySubtreeToClipboardWithNewIds [newA, newB] eidA) baseModel
  childIds Vault model' @?= [eidA, eidC]
  childIds Clipboard model' @?= [eidNewA]
  treeSummaryFor eidNewA model' @?= Just (TreeSummary eidNewA "A" [TreeSummary eidNewB "B" []])

testCopyIgnoresExtraIds :: Assertion
testCopyIgnoresExtraIds = do
  let model' = update (copySubtreeToClipboardWithNewIds [newA, newB, newC] eidA) baseModel
  childIds Clipboard model' @?= [eidNewA]
  treeSummaryFor eidNewA model' @?= Just (TreeSummary eidNewA "A" [TreeSummary eidNewB "B" []])

testCopyTooFewIdsNoops :: Assertion
testCopyTooFewIdsNoops = do
  let model' = update (copySubtreeToClipboardWithNewIds [newA] eidA) baseModel
  modelSummary model' @?= modelSummary baseModel

testCutPreservesIds :: Assertion
testCutPreservesIds = do
  let model' = update (cutSubtreeToClipboard eidA) baseModel
  childIds Vault model' @?= [eidC]
  childIds Clipboard model' @?= [eidA]
  treeSummaryFor eidA model' @?= Just (TreeSummary eidA "A" [TreeSummary eidB "B" []])

testPasteAfterConsumesFifo :: Assertion
testPasteAfterConsumesFifo = do
  let model' = update (pasteFirstClipboardEntryRelTo eidC insAfter) pasteModel
  childIds Clipboard model' @?= [eidD]
  childIds Vault model' @?= [eidC, eidA]

testPastePositions :: Assertion
testPastePositions = do
  let beforeModel = update (pasteFirstClipboardEntryRelTo eidC insBefore) pasteModel
  childIds Vault beforeModel @?= [eidA, eidC]

  let firstChildModel = update (pasteFirstClipboardEntryRelTo eidC insFirstChild) pasteModel
  childIds eidC firstChildModel @?= [eidA]

  let lastChildModel = update (pasteFirstClipboardEntryRelTo eidC insLastChild) pasteModel
  childIds eidC lastChildModel @?= [eidA]

testEmptyClipboardPaste :: Assertion
testEmptyClipboardPaste = do
  let model' = update (pasteFirstClipboardEntryRelTo eidC insAfter) baseModel
  modelSummary model' @?= modelSummary baseModel

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

oldDiskModel :: DiskModel
oldDiskModel =
  DiskModel . IdForest $
    [ leaf' Inbox "INBOX" []
    , leaf' Vault "VAULT" []
    ]

baseModel :: Model
baseModel = mkModel baseDiskModel

baseDiskModel :: DiskModel
baseDiskModel =
  DiskModel . IdForest $
    [ leaf' Inbox "INBOX" []
    , leaf' Vault "VAULT" [leaf' eidA "A" [leaf' eidB "B" []], leaf' eidC "C" []]
    , leaf' Clipboard "CLIPBOARD" []
    ]

pasteModel :: Model
pasteModel =
  mkModel . DiskModel . IdForest $
    [ leaf' Inbox "INBOX" []
    , leaf' Vault "VAULT" [leaf' eidC "C" []]
    , leaf' Clipboard "CLIPBOARD" [leaf' eidA "A" [], leaf' eidD "D" []]
    ]

nestedClipboardModel :: Model
nestedClipboardModel =
  mkModel . DiskModel . IdForest $
    [ leaf' Inbox "INBOX" []
    , leaf' Vault "VAULT" []
    , leaf' Clipboard "CLIPBOARD" [leaf' eidA "A" [leaf' eidB "B" []]]
    ]

leaf' :: EID -> String -> Forest (EID, Attr) -> Tree (EID, Attr)
leaf' eid label children = Node (eid, attr label) children

attr :: String -> Attr
attr = attrMinimal fixedTime

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2024 1 1) 0

hasTopLevel :: EID -> Model -> Bool
hasTopLevel eid (Model (IdForest forest)) = any (\(Node (eid', _) _) -> eid == eid') forest

childIds :: EID -> Model -> [EID]
childIds eid (Model forest) =
  case forestFindTree eid forest of
    Just (Node _ children) -> [eid' | Node (eid', _) _ <- children]
    Nothing -> []

treeSummaryFor :: EID -> Model -> Maybe TreeSummary
treeSummaryFor eid (Model forest) = treeSummary <$> forestFindTree eid forest

modelSummary :: Model -> [TreeSummary]
modelSummary (Model (IdForest forest)) = map treeSummary forest

data TreeSummary = TreeSummary EID String [TreeSummary]
  deriving (Eq, Show)

treeSummary :: Tree IdLabel -> TreeSummary
treeSummary (Node (eid, (attr', _)) children) = TreeSummary eid (name attr') (map treeSummary children)

eidA, eidB, eidC, eidD, eidNewA, eidNewB :: EID
eidA = EIDNormal uuidA
eidB = EIDNormal uuidB
eidC = EIDNormal uuidC
eidD = EIDNormal uuidD
eidNewA = EIDNormal newA
eidNewB = EIDNormal newB

uuidA, uuidB, uuidC, uuidD, newA, newB, newC :: UUID
uuidA = uuid "00000000-0000-0000-0000-000000000001"
uuidB = uuid "00000000-0000-0000-0000-000000000002"
uuidC = uuid "00000000-0000-0000-0000-000000000003"
uuidD = uuid "00000000-0000-0000-0000-000000000004"
newA = uuid "00000000-0000-0000-0000-000000000101"
newB = uuid "00000000-0000-0000-0000-000000000102"
newC = uuid "00000000-0000-0000-0000-000000000103"

uuid :: String -> UUID
uuid = fromMaybe (error "invalid test UUID") . UUID.fromString
