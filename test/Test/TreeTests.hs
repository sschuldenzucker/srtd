module Test.TreeTests (treeTests, regexTests) where

import Data.Set qualified as Set
import Data.Tree (Tree (..))
import Srtd.Data.IdTree (IdForest (..), transformIdForestDownUpRec)
import Srtd.Util
import Test.Tasty
import Test.Tasty.HUnit
import Text.Regex.TDFA
import Text.Regex.TDFA.Text (compile)

treeTests :: TestTree
treeTests =
  testGroup
    "Test tree transformations"
    [testTransformTreeDownUpRec, testTransformIdForestDownUpRec]

testTransformTreeDownUpRec :: TestTree
testTransformTreeDownUpRec =
  testGroup
    "Test transformTreeDownUpRec"
    [ testCase "Down-up sum" $
        let go :: Maybe (Int, Int) -> [(Int, Int)] -> Int -> (Int, Int)
            go mpar cps x = (maybe 0 fst mpar + x, sum (map snd cps) + x)
            res = transformTreeDownUpRec go tree1
            expd =
              Node
                (1, 227)
                [ Node
                    (12, 214)
                    [ Node (113, 101) []
                    , Node (114, 102) []
                    ]
                , Node (13, 12) []
                ]
         in res @?= expd
    ]
 where
  tree1 :: Tree Int
  tree1 =
    Node
      1
      [ Node
          11
          [ Node 101 []
          , Node 102 []
          ]
      , Node 12 []
      ]

testTransformIdForestDownUpRec :: TestTree
testTransformIdForestDownUpRec =
  testGroup
    "Test transformIdForestDownUpRec"
    [ testCase "Downsum, upsum, and down-up-sum" $
        let go :: Maybe (Int, Int, Int) -> [(Int, Int, Int)] -> Int -> (Int, Int, Int)
            go mpar cps x =
              let downsum = maybe 0 fst3 mpar + x
                  upsum = sum (map snd3 cps) + x
               in (downsum, upsum, downsum + upsum - x)
            res = transformIdForestDownUpRec go forest1
            expd =
              IdForest $
                [ Node
                    ("A", (1, 227, 227))
                    [ Node
                        ("B", (12, 214, 215))
                        [ Node ("C", (113, 101, 113)) []
                        , Node ("D", (114, 102, 114)) []
                        ]
                    , Node ("E", (13, 12, 13)) []
                    ]
                ]
         in res @?= expd
    ]
 where
  forest1 :: IdForest String Int
  forest1 =
    IdForest $
      [ Node
          ("A", 1)
          [ Node
              ("B", 11)
              [ Node ("C", 101) []
              , Node ("D", 102) []
              ]
          , Node ("E", 12) []
          ]
      ]
  fst3 (x, _, _) = x
  snd3 (_, x, _) = x

regexTests :: TestTree
regexTests =
  testGroup
    "Test regex helpers"
    [ testRegexSplitWithMatches
    , textRegexSplitsWithMatchesOverlap
    ]

testRegexSplitWithMatches :: TestTree
testRegexSplitWithMatches =
  testGroup
    "Test regexSplitWithMatches"
    [ testCase "1" $
        let (Right rx) = compile defaultCompOpt defaultExecOpt "oo"
            res = regexSplitWithMatches rx "foofoobar"
            expd = [(False, "f"), (True, "oo"), (False, "f"), (True, "oo"), (False, "bar")]
         in res @?= expd
    , testCase "2" $
        let (Right rx) = compile (defaultCompOpt {caseSensitive = False}) defaultExecOpt "o+"
            res = regexSplitWithMatches rx "foOfOoBar"
            expd = [(False, "f"), (True, "oO"), (False, "f"), (True, "Oo"), (False, "Bar")]
         in res @?= expd
    ]

textRegexSplitsWithMatchesOverlap :: TestTree
textRegexSplitsWithMatchesOverlap =
  testGroup
    "Test regexSplitsWithMatchesOverlap"
    [ testCase "url with trailing match" $
        let (Right rx) = compile defaultCompOpt defaultExecOpt "the h|om foo"
            haystack = "the https://google.com foobar"
            res =
              regexSplitsWithMatchesOverlap [(urlRegex, Set.singleton "url"), (rx, Set.singleton "rx")] haystack
            expd =
              [ (Set.singleton "rx", "the ")
              , (Set.fromList ["url", "rx"], "h")
              , (Set.singleton "url", "ttps://google.c")
              , (Set.fromList ["url", "rx"], "om")
              , (Set.singleton "rx", " foo")
              , (Set.empty, "bar")
              ]
         in res @?= expd
    , testCase "url with trailing match, with ordering" $
        let (Right rx) = compile defaultCompOpt defaultExecOpt "the h|om foo"
            haystack = "the https://google.com foobar"
            res =
              regexSplitsWithMatchesOverlap [(rx, ["rx"]), (urlRegex, ["url"])] haystack
            expd =
              [ (["rx"], "the ")
              , (["rx", "url"], "h")
              , (["url"], "ttps://google.c")
              , (["rx", "url"], "om")
              , (["rx"], " foo")
              , ([], "bar")
              ]
         in res @?= expd
    ]
