module Main (main) where

import Test.DateTests
import Test.ModelClipboardTests
import Test.QueryTests
import Test.Tasty
import Test.Tasty.HUnit
import Test.TreeTests

unit_additionTest :: TestTree
unit_additionTest =
  testCase "Addition works correctly" $
    2 + 2 @?= 4

testTests :: TestTree
testTests = testGroup "Test tests" [unit_additionTest]

tests :: TestTree
tests = testGroup "Tests" [testTests, dateTests, queryTests, treeTests, regexTests, modelClipboardTests]

main :: IO ()
main = defaultMain tests
