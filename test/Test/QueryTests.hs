module Test.QueryTests (queryTests) where

import Data.Text (Text)
import Srtd.Query
import Test.Support
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec as Parsec

parseQueryE :: Text -> Either String ParsedQuery
parseQueryE = mapLeft Parsec.errorBundlePretty . Parsec.parse pQuery "none"

queryTests :: TestTree
queryTests =
  testGroup
    "Query"
    [parseQueryTests]

parseQueryTests :: TestTree
parseQueryTests =
  testGroup
    "Parsing"
    [parseQueryRegexTests]

parseQueryRegexTests :: TestTree
parseQueryRegexTests =
  testGroup
    "Parse Regex Queries"
    [ testCase "single unfenced" $
        parseQueryE "ab.cd" `shouldBeRight` (ParsedQueryRegexParts ["ab.cd"])
    , testCase "multi unfenced with whitespace" $
        parseQueryE "  ab   cd ef" `shouldBeRight` (ParsedQueryRegexParts ["ab", "cd", "ef"])
    , testCase "single fenced" $
        parseQueryE "/a b/" `shouldBeRight` (ParsedQueryRegexParts ["a b"])
    , testCase "mixed fenced" $
        parseQueryE "xy /ab cd/ ef /g h/" `shouldBeRight` (ParsedQueryRegexParts ["xy", "ab cd", "ef", "g h"])
    , testCase "error on unfinished delimiter" $
        (dropLeft $ parseQueryE "xy /ab cd") @?= Left ()
    , testCase "escaping '/' outside fence" $
        parseQueryE "x\\/y" `shouldBeRight` (ParsedQueryRegexParts ["x/y"])
    , testCase "escaping '\\' outside fence" $
        parseQueryE "x\\\\y" `shouldBeRight` (ParsedQueryRegexParts ["x\\y"])
    , testCase "escaping '/' inside fence" $
        parseQueryE "/x \\/ y/ z" `shouldBeRight` (ParsedQueryRegexParts ["x / y", "z"])
    , testCase "escaping space" $
        parseQueryE "x\\ y a\\ b" `shouldBeRight` (ParsedQueryRegexParts ["x y", "a b"])
    ]
