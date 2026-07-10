{-# LANGUAGE MultilineStrings #-}

module Test.ModelLocalActionabilityTests (modelLocalActionabilityTests) where

import Data.Char (isSpace)
import Data.List (find, group, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time (
  LocalTime (..),
  TimeOfDay (..),
  UTCTime (..),
  ZonedTime (..),
  fromGregorian,
  hoursToTimeZone,
 )
import Data.Tree (Forest, Tree (..))
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Srtd.Attr
import Srtd.Data.IdTree
import Srtd.Model
import Test.Tasty
import Test.Tasty.HUnit

modelLocalActionabilityTests :: TestTree
modelLocalActionabilityTests =
  testGroup
    "Model local actionability"
    [ testCase "fixture has unique UUIDs" $ do
        duplicateNormalUUIDs testDiskModel @?= []
    , testCase "Someday masks Waiting through None (regression)" $
        withRootSubtree rootEID $
          assertLocalActionability "waiting through none" Someday
    , testCase "Someday masks Next through None (regression)" $
        withRootSubtree rootEID $
          assertLocalActionability "next through none" Someday
    , testCase "Someday masks direct Waiting and Next children" $
        withRootSubtree rootEID $ \st -> do
          assertLocalActionability "direct waiting" Someday st
          assertLocalActionability "direct next" Someday st
    , testCase "Someday masks Waiting and Next through Project" $
        withRootSubtree rootEID $ \st -> do
          assertLocalActionability "waiting through project" Someday st
          assertLocalActionability "next through project" Someday st
    , testCase "hoisting into Someday removes the Someday mask" $
        withRootSubtree somedayEID $ \st -> do
          assertLocalActionability "waiting through none" Waiting st
          assertLocalActionability "next through none" Next st
          assertLocalActionability "waiting through project" Waiting st
          assertLocalActionability "next through project" Next st
    , testCase "Project and Open are transparent to actionable children" $
        withRootSubtree rootEID $ \st -> do
          assertLocalActionability "next through root project" Next st
          assertLocalActionability "next through open" Next st
          assertLocalActionability "later through open" Later st
    , testCase "Waiting masks Next children" $
        withRootSubtree rootEID $
          assertLocalActionability "next through waiting" Waiting
    , testCase "Nones bubble up actionability transparently" $
        withRootSubtree rootEID $
          assertLocalActionability "root project with indirect none" Next
    , testCase "None leaves do not inherit local parent actionability" $
        withRootSubtree rootEID $ \st -> do
          assertLocalActionability "note under next" None st
          assertLocalActionability "note under waiting" None st
          assertLocalActionability "note under open" None st
          assertLocalActionability "note under someday" None st
    , testCase "None ancestors inherit parent actionability without changing child-derived actionability" $
        withRootSubtree rootEID $ \st -> do
          assertParentActionability "someday transparent none" Someday st
          assertGlobalActionability "someday transparent none" Next
          assertLocalActionability "someday transparent none" Someday st
          assertParentActionability "note under someday" Someday st
          assertLocalActionability "note under someday" None st
          assertParentActionability "open transparent none" Open st
          assertGlobalActionability "open transparent none" Next
          assertLocalActionability "open transparent none" Next st
          assertParentActionability "note under open" Open st
          assertLocalActionability "note under open" None st
    , testCase "hoisting into None removes masks from higher ancestors" $
        withNamedRootSubtree "someday transparent none" $ \st -> do
          assertLocalActionability "waiting through none" Waiting st
          assertLocalActionability "next through none" Next st
          assertLocalActionability "delayed empty project" None st
    , testCase "non-delayed filter excludes None notes under actionable parents" testNonDelayedFilterNoneNotes
    , testCase "Open remains semi-transparent through None ancestors" $
        withRootSubtree rootEID $ \st -> do
          assertLocalActionability "next through open and transparent none" Next st
          assertLocalActionability "later through open and transparent none" Later st
    , testCase "Waiting and Later remain non-transparent through None ancestors" $
        withRootSubtree rootEID $ \st -> do
          assertLocalActionability "next through waiting and transparent none" Waiting st
          assertLocalActionability "next through later and transparent none" Later st
    , testCase "empty projects with no children have None actionability" $
        assertGlobalActionability "neutral empty project" None
    , testCase "empty projects with only empty subprojects are None" $
        assertGlobalActionability "neutral project with empty subproject" None
    , testCase "empty subprojects of empty projects are None" $
        assertGlobalActionability "subproject of neutral project" None
    , testCase "stalled projects filter follows current None transparency" testStalledProjectsFilter
    ]

withRootSubtree :: EID -> (Subtree -> Assertion) -> Assertion
withRootSubtree eid assertSubtree = case modelGetSubtreeBelow eid testModel of
  Left IdNotFoundError -> assertFailure $ "could not find subtree root: " ++ showEIDShort eid
  Right st -> assertSubtree st

withNamedRootSubtree :: String -> (Subtree -> Assertion) -> Assertion
withNamedRootSubtree nodeName assertSubtree = case findGlobalNode nodeName testModel of
  Nothing -> assertFailure $ "could not find global node: " ++ nodeName
  Just node -> withRootSubtree (gEID node) assertSubtree

assertLocalActionability :: String -> Status -> Subtree -> Assertion
assertLocalActionability nodeName expected st = case findLocalNode nodeName st of
  Nothing -> assertFailure $ "could not find local node: " ++ nodeName
  Just node -> gLocalActionability node @?= expected

assertParentActionability :: String -> Status -> Subtree -> Assertion
assertParentActionability nodeName expected st = case findLocalNode nodeName st of
  Nothing -> assertFailure $ "could not find local node: " ++ nodeName
  Just node -> gParentActionability node @?= expected

assertGlobalActionability :: String -> Status -> Assertion
assertGlobalActionability nodeName expected = case findGlobalNode nodeName testModel of
  Nothing -> assertFailure $ "could not find global node: " ++ nodeName
  Just node -> gGlobalActionability node @?= expected

findLocalNode :: String -> Subtree -> Maybe LocalIdLabel
findLocalNode nodeName st = find ((== nodeName) . gName) (flattenSTForest $ stForest st)

findGlobalNode :: String -> Model -> Maybe IdLabel
findGlobalNode nodeName (Model forest) = find ((== nodeName) . gName) (flattenMForest forest)

flattenMForest :: MForest -> [IdLabel]
flattenMForest (IdForest forest) = concatMap flattenTree forest
 where
  flattenTree tree =
    case tree of
      Node idLabel children -> idLabel : concatMap flattenTree children

flattenSTForest :: STForest -> [LocalIdLabel]
flattenSTForest (IdForest forest) = concatMap flattenTree forest
 where
  flattenTree tree =
    case tree of
      Node localIdLabel children -> localIdLabel : concatMap flattenTree children

testStalledProjectsFilter :: Assertion
testStalledProjectsFilter = do
  names <- namesFromFilter f_stalledProjects
  assertContains "neutral empty project" names
  assertContains "neutral project with empty subproject" names
  assertContains "subproject of neutral project" names
  assertNotContains "delayed empty project" names

testNonDelayedFilterNoneNotes :: Assertion
testNonDelayedFilterNoneNotes = do
  names <- namesFromFilter f_NotDelayedByLastModified
  assertNotContains "note under next" names
  assertNotContains "note under waiting" names
  assertNotContains "note under open" names
  assertNotContains "note under someday" names

namesFromFilter :: Filter -> IO [String]
namesFromFilter filter' = case runFilter testFilterContext filter' rootEID testModel of
  Left IdNotFoundError -> assertFailure "could not run filter below local actionability root"
  Right st -> return . map gName . flattenSTForest $ stForest st

testFilterContext :: FilterContext
testFilterContext =
  FilterContext $
    ZonedTime
      (LocalTime (fromGregorian 2024 1 2) (TimeOfDay 0 0 0))
      (hoursToTimeZone 0)

assertContains :: String -> [String] -> Assertion
assertContains needle haystack = assertBool ("expected to find " ++ show needle) (needle `elem` haystack)

assertNotContains :: String -> [String] -> Assertion
assertNotContains needle haystack = assertBool ("expected not to find " ++ show needle) (needle `notElem` haystack)

duplicateNormalUUIDs :: DiskModel -> [UUID]
duplicateNormalUUIDs (DiskModel forest) = mapMaybe normalUUID $ duplicateIdsInForest forest

duplicateIdsInForest :: (Ord id) => IdForest id a -> [id]
duplicateIdsInForest = duplicates . idsInForest

idsInForest :: IdForest id a -> [id]
idsInForest (IdForest forest) = concatMap treeIds forest
 where
  treeIds tree =
    case tree of
      Node (eid', _) children -> eid' : concatMap treeIds children

duplicates :: (Ord a) => [a] -> [a]
duplicates = mapMaybe duplicateValue . group . sort
 where
  duplicateValue (x : _ : _) = Just x
  duplicateValue _ = Nothing

normalUUID :: EID -> Maybe UUID
normalUUID (EIDNormal uuid') = Just uuid'
normalUUID _ = Nothing

parseIndentedDiskModel :: String -> Either String DiskModel
parseIndentedDiskModel src = do
  parsedNodes <- traverse parseIndentedNode nonblankLines
  forest <- parsedNodesToForest parsedNodes
  return . DiskModel $ IdForest forest
 where
  nonblankLines = filter (not . all isSpace . snd) (zip [1 ..] $ lines src)

data ParsedNode = ParsedNode Int Int EID Status String

parseIndentedNode :: (Int, String) -> Either String ParsedNode
parseIndentedNode (lineNo, line) = do
  level <- parseIndent lineNo indent
  (nodeStatus, nodeName) <- parseNodeText lineNo rest
  return $ ParsedNode lineNo level (eidFromInt lineNo) nodeStatus nodeName
 where
  (indent, rest) = span (== ' ') line

parseIndent :: Int -> String -> Either String Int
parseIndent lineNo indent
  | length indent `mod` 2 == 0 = Right $ length indent `div` 2
  | otherwise = Left $ "line " ++ show lineNo ++ ": indentation must use multiples of two spaces"

parseNodeText :: Int -> String -> Either String (Status, String)
parseNodeText lineNo line = case line of
  statusChar : ' ' : nodeName
    | all isSpace nodeName -> Left $ "line " ++ show lineNo ++ ": node name must not be empty"
    | otherwise -> do
        nodeStatus <- parseStatusSymbol lineNo statusChar
        return (nodeStatus, nodeName)
  _ -> Left $ "line " ++ show lineNo ++ ": expected '<status-symbol> <name>'"

parseStatusSymbol :: Int -> Char -> Either String Status
parseStatusSymbol lineNo statusChar = case statusChar of
  '*' -> Right Next
  '<' -> Right Waiting
  '>' -> Right Project
  '/' -> Right Later
  '~' -> Right Someday
  '+' -> Right Done
  'x' -> Right Canceled
  'o' -> Right Open
  '-' -> Right None
  _ -> Left $ "line " ++ show lineNo ++ ": unknown status symbol " ++ show statusChar

parsedNodesToForest :: [ParsedNode] -> Either String (Forest (EID, Attr))
parsedNodesToForest nodes = do
  (forest, rest) <- parseForestAtLevel 0 nodes
  case rest of
    [] -> Right forest
    ParsedNode lineNo _ _ _ _ : _ -> Left $ "line " ++ show lineNo ++ ": could not parse forest"

parseForestAtLevel :: Int -> [ParsedNode] -> Either String (Forest (EID, Attr), [ParsedNode])
parseForestAtLevel _ [] = Right ([], [])
parseForestAtLevel level nodes@(ParsedNode lineNo nodeLevel nodeEID nodeStatus nodeName : rest)
  | nodeLevel < level = Right ([], nodes)
  | nodeLevel > level = Left $ "line " ++ show lineNo ++ ": indentation jumps too far"
  | otherwise = do
      (children, restAfterChildren) <- parseForestAtLevel (level + 1) rest
      (siblings, restAfterSiblings) <- parseForestAtLevel level restAfterChildren
      return (Node (nodeEID, attr nodeName nodeStatus) children : siblings, restAfterSiblings)

attr :: String -> Status -> Attr
attr nodeName nodeStatus = (attrMinimal fixedTime nodeName) {status = nodeStatus}

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2024 1 1) 0

testDiskModel :: DiskModel
testDiskModel = case parseIndentedDiskModel localActionabilityFixture of
  Left err -> error err
  Right diskModel -> diskModel

localActionabilityFixture :: String
localActionabilityFixture =
  """
  - local actionability root
    ~ someday
      < direct waiting
      * direct next
      - note under someday
      - someday transparent none
        < waiting through none
        * next through none
        > delayed empty project
      > transparent project
        < waiting through project
        * next through project
    > root project
      * next through root project
    > root project with indirect none
      - project transparent none
        * next through project and transparent none
    o open
      * next through open
      / later through open
      - note under open
      - open transparent none
        * next through open and transparent none
        / later through open and transparent none
    < waiting
      * next through waiting
      - note under waiting
      - waiting transparent none
        * next through waiting and transparent none
    / later
      - later transparent none
        * next through later and transparent none
    * next parent
      - note under next
    > neutral empty project
    > neutral project with empty subproject
      > subproject of neutral project
  """

testModel :: Model
testModel = mkModel testDiskModel

mkModel :: DiskModel -> Model
mkModel diskModel =
  let ?mue = ModelUpdateEnv (hoursToTimeZone 0)
   in diskModelToModel diskModel

rootEID, somedayEID :: EID
rootEID = eid "00000000-0000-0000-0000-000000000001"
somedayEID = eid "00000000-0000-0000-0000-000000000002"

eid :: String -> EID
eid = EIDNormal . uuid

uuid :: String -> UUID
uuid = fromMaybe (error "invalid test UUID") . UUID.fromString

eidFromInt :: Int -> EID
eidFromInt n = eid $ "00000000-0000-0000-0000-" ++ padded
 where
  decimal = show n
  padded = replicate (12 - length decimal) '0' ++ decimal
