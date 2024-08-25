module MainTest (main) where

import Main qualified
import Test.HUnit.Tasty
import Test.Tasty

unit_additionTest :: TestTree
unit_additionTest =
  testCase "Addition works correctly" $
    2 + 2 @?= 4

tests :: TestTree
tests = testGroup "Simple tests" [unit_additionTest]

main :: IO ()
main = defaultTest tests
