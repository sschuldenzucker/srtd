module Main (main) where

import Srtd.Main qualified -- import test
import Test.Tasty
import Test.Tasty.HUnit

unit_additionTest :: TestTree
unit_additionTest =
  testCase "Addition works correctly" $
    2 + 2 @?= 4

tests :: TestTree
tests = testGroup "Simple tests" [unit_additionTest]

main :: IO ()
main = defaultMain tests
