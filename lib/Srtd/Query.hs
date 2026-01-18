{-| A fuzzy-ish match query that matches nodes and hierarchy.

Right now, it can only match nodes
-}
module Srtd.Query (
  Query (..),
  ParsedQuery (..),
  parseAndCompileQuery,
  parseQuery,
  compileQuery,
  pQuery,
) where

import Control.Monad ((>=>))
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Srtd.Util (eitherToMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Regex.TDFA (CompOption (..), ExecOption (..), Regex, defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.Text (compile)

-- | A compiled query
data Query = QueryRegexParts
  { qrxParts :: [Regex]
  }

-- * Parsing

-- | Parse a Text into a query.
--
-- SOMEDAY better error messages?
parseAndCompileQuery :: Text -> Maybe Query
parseAndCompileQuery = parseQuery >=> compileQuery

parseQuery :: Text -> Maybe ParsedQuery
parseQuery = parseMaybe pQuery

type Parser = Parsec Void Text

type MyParseErrorBundle = ParseErrorBundle Text Void

data ParsedQuery = ParsedQueryRegexParts [Text]
  deriving (Eq, Show)

compileQuery :: ParsedQuery -> Maybe Query
compileQuery (ParsedQueryRegexParts parts) =
  QueryRegexParts <$> mapM (eitherToMaybe . compile myCompOpt myExecOpt) parts

myCompOpt :: CompOption
myCompOpt = defaultCompOpt {caseSensitive = False}

myExecOpt :: ExecOption
myExecOpt = defaultExecOpt {captureGroups = False}

pQuery :: Parser ParsedQuery
pQuery = space >> pQuery' <* (space >> eof)

pQuery' :: Parser ParsedQuery
pQuery' = ParsedQueryRegexParts <$> pChunks
 where
  -- NO `try` on the first branch since we want it to fail on unclosed '/'.
  pChunks = (pSlashFencedRegex <|> pUnfencedRegex) `sepBy` space1

pSlashFencedRegex :: Parser Text
pSlashFencedRegex = single '/' >> takeWhile1P (Just "/") (/= '/') <* single '/'

pUnfencedRegex :: Parser Text
pUnfencedRegex = takeWhile1P (Just "non-space") (not . isSpace)
