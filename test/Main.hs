{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Srtd.Dates
import Srtd.Main qualified
import Test.Tasty
import Test.Tasty.HUnit

unit_additionTest :: TestTree
unit_additionTest =
  testCase "Addition works correctly" $
    2 + 2 @?= 4

testTests :: TestTree
testTests = testGroup "Test tests" [unit_additionTest]

dateTests :: TestTree
dateTests = testGroup "Date" [parseHumanDateOrTimeTests]

parseHumanDateOrTimeTests =
  testGroup
    "Parse HumanDateOrTime"
    [ testCase "Parse ISO date" $
        parseHumanDateOrTime "2024-03-07" @?= Just (HumanDateOrTime (Just $ HDAbsolute (Just 2024) (Just 3) (Just 7)) Nothing)
    ]

tests :: TestTree
tests = testGroup "Tests" [testTests, dateTests]

main :: IO ()
main = defaultMain tests
