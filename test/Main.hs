{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Data.Time
import Srtd.Dates
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec as Parsec

unit_additionTest :: TestTree
unit_additionTest =
  testCase "Addition works correctly" $
    2 + 2 @?= 4

testTests :: TestTree
testTests = testGroup "Test tests" [unit_additionTest]

dateTests :: TestTree
dateTests = testGroup "Date" [parseHumanDateOrTimeTests]

parseE p = mapLeft Parsec.errorBundlePretty . Parsec.parse p "none"

parseDateE :: Text -> Either String HumanDateOrTime
parseDateE = mapLeft Parsec.errorBundlePretty . Parsec.parse pHumanDateOrTime "none"

-- Don't wanna depend on extra right now.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft f (Right y) = Right y

-- | Better than raw `@?=` b/c multi-line errors are formatted better (e.g. for parsers).
shouldBeRight (Left errstr) _ = assertFailure errstr
shouldBeRight (Right x) expd = x @?= expd

parseHumanDateOrTimeTests =
  testGroup
    "Parse HumanDateOrTime"
    [ testCase "ISO date" $
        parseDateE "2024-03-07" `shouldBeRight` (HDateMaybeTime (HDAbs $ fromGregorian 2024 3 7) Nothing),
      testCase "ISO short form" $
        parseDateE "24-03-07" `shouldBeRight` (HDateMaybeTime (HDAbs $ fromGregorian 2024 3 7) Nothing),
      testCase "anchor to day of week" $
        parseDateE "mon" `shouldBeRight` (HDateMaybeTime (HDToNext 1 $ AnchorDayOfWeek Monday) Nothing),
      testCase "int-shifted anchor to day of week" $
        parseDateE "in 2 mon" `shouldBeRight` (HDateMaybeTime (HDToNext 2 $ AnchorDayOfWeek Monday) Nothing),
      testCase "day offset" $
        parseDateE "in 2 days" `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 0 2) Nothing),
      testCase "month offset" $
        parseDateE "in 2 months" `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 2 0) Nothing),
      testCase "day offset short form 1" $
        parseDateE "in 2d" `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 0 2) Nothing),
      testCase "today" $
        parseDateE "tod" `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 0 0) Nothing),
      testCase "tomorrow" $
        parseDateE "tom" `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 0 1) Nothing),
      testCase "day offset short form 2" $
        parseDateE "2d" `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 0 2) Nothing),
      testCase "day offset with time" $
        parseDateE "in 2 days 17:00"
          `shouldBeRight` (HDateMaybeTime (HDDiff $ CalendarDiffDays 0 2) (Just $ TimeOfDay 17 0 0)),
      testCase "absolute time only" $
        parseDateE "17:00" `shouldBeRight` (HTimeOnly (TimeOfDay 17 0 0)),
      testCase "absolute time only, short form" $
        parseDateE "17t" `shouldBeRight` (HTimeOnly (TimeOfDay 17 0 0)),
      testCase "relative time only, hour" $
        parseDateE "in 1 hour" `shouldBeRight` (HDiffTime $ fromInteger (60 * 60)),
      testCase "relative time only, hour short" $
        parseDateE "1h" `shouldBeRight` (HDiffTime $ fromInteger (60 * 60)),
      testCase "relative time only, minute short" $
        parseDateE "2min" `shouldBeRight` (HDiffTime $ fromInteger (60 * 2))
    ]

tests :: TestTree
tests = testGroup "Tests" [testTests, dateTests]

main :: IO ()
main = defaultMain tests
