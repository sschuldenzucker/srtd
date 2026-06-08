module Test.DateTests (dateTests) where

import Data.Text (Text)
import Data.Time
import Srtd.Dates
import Test.Support
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec as Parsec

dateTests :: TestTree
dateTests =
  testGroup
    "Date"
    [parseHumanDateOrTimeTests, nextWeekdayTests, interpretHumanDateOrTimeTests, dateRenderingTests]

parseDateE :: Text -> Either String HumanDateOrTime
parseDateE = mapLeft Parsec.errorBundlePretty . Parsec.parse pHumanDateOrTime "none"

parseAndInterpretDateE :: Text -> ZonedTime -> Either String DateOrTime
parseAndInterpretDateE s now = do
  hdt <- parseDateE s
  interpretHumanDateOrTime hdt now

parseHumanDateOrTimeTests :: TestTree
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

nextWeekdayTests :: TestTree
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

interpretHumanDateOrTimeTests :: TestTree
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
    , testCase "Time only 2" $
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

dateRenderingTests :: TestTree
dateRenderingTests =
  testGroup
    "Date & Time Rendering"
    [prettyPastStrictRelativeAdaptiveTests]

prettyPastStrictRelativeAdaptiveTests :: TestTree
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
