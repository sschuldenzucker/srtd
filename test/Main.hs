-- SOMEDAY split these tests into separate modules

module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time
import Data.Tree (Tree (..))
import Srtd.Data.IdTree (IdForest (..), transformIdForestDownUpRec)
import Srtd.Dates
import Srtd.Util
import Srtd.Query
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec as Parsec
import Text.Regex.TDFA
import Text.Regex.TDFA.Text (compile)

unit_additionTest :: TestTree
unit_additionTest =
  testCase "Addition works correctly" $
    2 + 2 @?= 4

testTests :: TestTree
testTests = testGroup "Test tests" [unit_additionTest]

dateTests :: TestTree
dateTests =
  testGroup
    "Date"
    [parseHumanDateOrTimeTests, nextWeekdayTests, interpretHumanDateOrTimeTests, dateRenderingTests]

parseE p = mapLeft Parsec.errorBundlePretty . Parsec.parse p "none"

parseDateE :: Text -> Either String HumanDateOrTime
parseDateE = mapLeft Parsec.errorBundlePretty . Parsec.parse pHumanDateOrTime "none"

parseAndInterpretDateE :: Text -> ZonedTime -> Either String DateOrTime
parseAndInterpretDateE s now = do
  hdt <- parseDateE s
  res <- interpretHumanDateOrTime hdt now
  return res

-- Don't wanna depend on extra right now.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft f (Right y) = Right y

dropLeft :: Either a b -> Either () b
dropLeft = mapLeft (const ())

-- | Better than raw `@?=` b/c multi-line errors are formatted better (e.g. for parsers).
shouldBeRight (Left errstr) _ = assertFailure errstr
shouldBeRight (Right x) expd = x @?= expd

parseHumanDateOrTimeTests =
  testGroup
    "Parse HumanDateOrTime"
    [ testCase "ISO date" $
        parseDateE "2024-03-07" `shouldBeRight` (HDateMaybeTime (HDAbs $ fromGregorian 2024 3 7) Nothing)
    , testCase "ISO short form" $
        parseDateE "24-03-07" `shouldBeRight` (HDateMaybeTime (HDAbs $ fromGregorian 2024 3 7) Nothing)
    , testCase "anchor to day of week" $
        parseDateE "mon" `shouldBeRight` (HDateMaybeTime (HDToNext 1 $ AnchorDayOfWeek Monday) Nothing)
    , testCase "int-shifted anchor to day of week" $
        parseDateE "in 2 mon" `shouldBeRight` (HDateMaybeTime (HDToNext 2 $ AnchorDayOfWeek Monday) Nothing)
    , testCase "day offset" $
        parseDateE "in 2 days" `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 0 2) Nothing)
    , testCase "month offset" $
        parseDateE "in 2 months" `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 2 0) Nothing)
    , testCase "day offset short form 1" $
        parseDateE "in 2d" `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 0 2) Nothing)
    , testCase "today" $
        parseDateE "tod" `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 0 0) Nothing)
    , testCase "tomorrow" $
        parseDateE "tom" `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 0 1) Nothing)
    , testCase "day offset short form 2" $
        parseDateE "2d" `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 0 2) Nothing)
    , testCase "day offset with time" $
        parseDateE "in 2 days 17:00"
          `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 0 2) (Just $ TimeOfDay 17 0 0))
    , testCase "absolute time only" $
        parseDateE "17:00" `shouldBeRight` (HTimeOnly (TimeOfDay 17 0 0))
    , testCase "absolute time only, short form" $
        parseDateE "17t" `shouldBeRight` (HTimeOnly (TimeOfDay 17 0 0))
    , testCase "relative time only, hour" $
        parseDateE "in 1 hour" `shouldBeRight` (HDiffTime $ fromInteger (60 * 60))
    , testCase "relative time only, hour short" $
        parseDateE "1h" `shouldBeRight` (HDiffTime $ fromInteger (60 * 60))
    , testCase "relative time only, minute short" $
        parseDateE "2min" `shouldBeRight` (HDiffTime $ fromInteger (60 * 2))
    ]

nextWeekdayTests =
  testGroup
    "Next weekdays"
    [ testCase "Next weekday 1" $
        nextWeekday Tuesday (fromGregorian 2024 8 30) @?= fromGregorian 2024 9 3
    , testCase "Next weekday 2" $
        nextWeekday Tuesday (fromGregorian 2024 9 2) @?= fromGregorian 2024 9 3
    , testCase "Next weekday 3" $
        nextWeekday Sunday (fromGregorian 2024 9 1) @?= fromGregorian 2024 9 8
    ]

interpretHumanDateOrTimeTests =
  testGroup
    "Parse & interpret HumanDateOrTime"
    [ testCase "Next 13th" $
        parseAndInterpretDateE "13th" now1 `shouldBeRight` (DateOnly $ fromGregorian 2024 8 13)
    , testCase "Next 9th" $
        parseAndInterpretDateE "9th" now1 `shouldBeRight` (DateOnly $ fromGregorian 2024 9 9)
    , testCase "Next 10th" $
        parseAndInterpretDateE "10th" now1 `shouldBeRight` (DateOnly $ fromGregorian 2024 9 10)
    , testCase "Next mon" $
        parseAndInterpretDateE "mon" now1 `shouldBeRight` (DateOnly $ fromGregorian 2024 8 12)
    , testCase "Next year-month" $
        parseAndInterpretDateE "11-10" now1 `shouldBeRight` (DateOnly $ fromGregorian 2024 11 10)
    , testCase "Next year-month before" $
        parseAndInterpretDateE "08-09" now1 `shouldBeRight` (DateOnly $ fromGregorian 2025 8 9)
    , testCase "In 2 days" $
        parseAndInterpretDateE "in 2 days" now1 `shouldBeRight` (DateOnly $ fromGregorian 2024 8 12)
    , testCase "In 2 months" $
        parseAndInterpretDateE "2 months" now1 `shouldBeRight` (DateOnly $ fromGregorian 2024 10 10)
    , testCase "In 2 15th" $
        parseAndInterpretDateE "2 15th" now1 `shouldBeRight` (DateOnly $ fromGregorian 2024 9 15)
    , testCase "In 2 9th" $
        parseAndInterpretDateE "2 9th" now1 `shouldBeRight` (DateOnly $ fromGregorian 2024 10 9)
    , testCase "In 2 10th" $
        parseAndInterpretDateE "2 10th" now1 `shouldBeRight` (DateOnly $ fromGregorian 2024 10 10)
    , testCase "In 3 31st" $
        parseAndInterpretDateE "3 31st" now1 `shouldBeRight` (DateOnly $ fromGregorian 2024 10 31)
    , testCase "Time only 1" $
        parseAndInterpretDateE "17:00" now1
          `shouldBeRight` (DateAndTime $ UTCTime (fromGregorian 2024 8 10) (secondsToDiffTime $ hours 16))
    , -- SOMEDAY This is a behavior where I'm not sure I want it rn: Going to an earlier time in the day doesn't leap to the next day!
      testCase "Time only 2" $
        parseAndInterpretDateE "8:00" now1
          `shouldBeRight` (DateAndTime $ UTCTime (fromGregorian 2024 8 10) (secondsToDiffTime $ hours 7))
    , testCase "Date and time" $
        parseAndInterpretDateE "mon 17:00" now1
          `shouldBeRight` (DateAndTime $ UTCTime (fromGregorian 2024 8 12) (secondsToDiffTime $ hours 16))
    ]
 where
  now1 = ZonedTime (LocalTime (fromGregorian 2024 8 10) (TimeOfDay 8 0 0)) cet
  cet = hoursToTimeZone 1
  hours n = n * 60 * 60

dateRenderingTests =
  testGroup
    "Date & Time Rendering"
    [prettyPastStrictRelativeAdaptiveTests]

prettyPastStrictRelativeAdaptiveTests =
  testGroup
    "Past Dates"
    [ testCase "Today" $
        prettyPastStrictRelativeAdaptive False now1 (DateOnly $ fromGregorian 2024 8 10) @?= "today"
    , testCase "Yesterday" $
        prettyPastStrictRelativeAdaptive False now1 (DateOnly $ fromGregorian 2024 8 9) @?= "yesterday"
    , testCase "1 month" $
        prettyPastStrictRelativeAdaptive False now1 (DateOnly $ fromGregorian 2024 7 10) @?= "1 month"
    , testCase "1.5 months" $
        prettyPastStrictRelativeAdaptive False now1 (DateOnly $ fromGregorian 2024 7 1) @?= "1 month"
    , testCase "1.5 months 2" $
        prettyPastStrictRelativeAdaptive False now1 (DateOnly $ fromGregorian 2024 6 29) @?= "1 month"
    , testCase "almost 2 months" $
        prettyPastStrictRelativeAdaptive False now1 (DateOnly $ fromGregorian 2024 6 11) @?= "1 month"
    , testCase "2 months" $
        prettyPastStrictRelativeAdaptive False now1 (DateOnly $ fromGregorian 2024 6 10) @?= "2 months"
    , testCase "1 year" $
        prettyPastStrictRelativeAdaptive False now1 (DateOnly $ fromGregorian 2023 8 10) @?= "1 year"
    , testCase "2 years" $
        prettyPastStrictRelativeAdaptive False now1 (DateOnly $ fromGregorian 2022 8 10) @?= "2 years"
    , testCase "future fallback" $
        prettyPastStrictRelativeAdaptive False now1 (DateOnly $ fromGregorian 2025 8 10) @?= "2025-08-10"
    ]
 where
  now1 = ZonedTime (LocalTime (fromGregorian 2024 8 10) (TimeOfDay 8 0 0)) cet
  cet = hoursToTimeZone 1

parseQueryE :: Text -> Either String ParsedQuery
parseQueryE = mapLeft Parsec.errorBundlePretty . Parsec.parse pQuery "none"

queryTests =
  testGroup
    "Query"
      [parseQueryTests]

parseQueryTests =
  testGroup
    "Parsing"
      [parseQueryRegexTests]

parseQueryRegexTests = 
  testGroup
    "Parse Regex Queries"
    [ testCase "single unfenced" $
        parseQueryE "ab.cd" `shouldBeRight` (ParsedQueryRegexParts ["ab.cd"])
    , testCase "multi unfenced with whitespace" $
        parseQueryE "  ab   cd ef" `shouldBeRight` (ParsedQueryRegexParts ["ab", "cd", "ef"])
    , testCase "mixed fenced" $
        parseQueryE "xy /ab cd/ ef /g h/" `shouldBeRight` (ParsedQueryRegexParts ["xy", "ab cd", "ef", "g h"])
    , testCase "error on unfinished delimiter" $
        (dropLeft $ parseQueryE "xy /ab cd") @?= Left ()
    ]

treeTests =
  testGroup
    "Test tree transformations"
    [testTransformTreeDownUpRec, testTransformIdForestDownUpRec]

-- These are *mainly* tests to make sure these things don't hang.
testTransformTreeDownUpRec =
  testGroup
    "Test transformTreeDownUpRec"
    [ testCase "Down-up sum" $
        -- Computes (sum top-down, sum bottom-up)
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

testTransformIdForestDownUpRec =
  testGroup
    "Test transformIdForestDownUpRec"
    [ testCase "Downsum, upsum, and down-up-sum" $
        -- like above but with IDs
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

regexTests =
  testGroup
    "Test regex helpers"
    [ testRegexSplitWithMatches
    , textRegexSplitsWithMatchesOverlap
    ]

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

tests :: TestTree
tests = testGroup "Tests" [testTests, dateTests, queryTests, treeTests, regexTests]

main :: IO ()
main = defaultMain tests
