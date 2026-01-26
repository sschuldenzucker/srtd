{-| A fuzzy-ish match query that matches nodes and hierarchy.

Right now, it can only match nodes
-}
module Srtd.Query (
  Query,
  SingleItemQuery (..),
  ParsedQuery (..),
  ParsedSingleItemQuery,
  parseAndCompileQuery,
  parseAndCompileSingleItemQuery,
  parseQuery,
  compileQuery,
  parseSingleItemQuery,
  compileSingleItemQuery,
  pQuery,
) where

import Control.Monad ((>=>))
import Data.Char (isSpace)
import Data.Functor (void)
import Data.List (foldl1', sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Srtd.Util (eitherToMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Regex.TDFA (
  AllMatches (..),
  CompOption (..),
  ExecOption (..),
  Regex,
  defaultCompOpt,
  defaultExecOpt,
 )
import Text.Regex.TDFA qualified as RX
import Text.Regex.TDFA.Text (compile)

-- | A compiled query
--
-- To be extended SOMEDAY to queries that go across hierarchy
type Query = SingleItemQuery

-- | A compiled query for a single item
data SingleItemQuery = QueryRegexParts
  { qrxParts :: [Regex]
  }

-- * Parsing

-- | Parse a Text into a query.
--
-- SOMEDAY better error messages?
parseAndCompileQuery :: Text -> Maybe Query
parseAndCompileQuery = parseQuery >=> compileQuery

parseAndCompileSingleItemQuery :: Text -> Maybe SingleItemQuery
parseAndCompileSingleItemQuery = parseAndCompileQuery

parseQuery :: Text -> Maybe ParsedQuery
parseQuery = parseMaybe pQuery

parseSingleItemQuery :: Text -> Maybe ParsedSingleItemQuery
parseSingleItemQuery = parseQuery

type Parser = Parsec Void Text

type MyParseErrorBundle = ParseErrorBundle Text Void

type ParsedSingleItemQuery = ParsedQuery

data ParsedQuery = ParsedQueryRegexParts [Text]
  deriving (Eq, Show)

compileQuery :: ParsedQuery -> Maybe Query
compileQuery (ParsedQueryRegexParts parts) =
  QueryRegexParts <$> mapM (eitherToMaybe . compile myCompOpt myExecOpt) parts

compileSingleItemQuery :: ParsedSingleItemQuery -> Maybe Query
compileSingleItemQuery = compileQuery

myCompOpt :: CompOption
myCompOpt = defaultCompOpt {caseSensitive = False}

myExecOpt :: ExecOption
myExecOpt = defaultExecOpt {captureGroups = False}

-- * Compatibility

-- We _could_ make SingleItemQuery an instance of RegexLike but it doesn't really quite fit: it doesn't necessarily match a contiguous string, so `matchOnce` is kinda ill-defined.

-- TODO draft:

{- -- | Like `getAllMatches . match regex` specialized to a list of (start, len)
--
-- Overlaps are collapsed and matches are in order.
singleItemQueryAllMatches :: SingleItemQuery -> Text -> [(Int, Int)]
singleItemQueryAllMatches (QueryRegexParts rxs) input = _todo
 where
  -- TODO is this even the right thing?!?
  -- I think there's a deeper conceptual question here.
  matchesWithOverlaps :: [(Int, Int)]
  matchesWithOverlaps = sortBy (comparing fst) . flip concatMap rxs $ \rx ->
    getAllMatches (RX.match rx input)
  dropOverlaps :: [(Int, Int)] -> [(Int, Int)]
  dropOverlaps = _todo

-- | Like 'regexSplitsWithMatchesOverlap' but for queries.
-- SOMEDAY not clear to me how long this can be here b/c I may also match other things (like metadata)
querySplitsWithMatchesOverlap :: (Monoid a) => [(SingleItemQuery, a)] -> Text -> [(a, Text)]
querySplitsWithMatchesOverlap = _todo -}

-- * Parsing

pQuery :: Parser ParsedQuery
pQuery = space >> pQuery' <* (space >> eof)

pQuery' :: Parser ParsedQuery
pQuery' = ParsedQueryRegexParts <$> pChunks
 where
  -- TODO I don't think we need fencing b/c people can just escape space.
  -- NO `try` on the first branch since we want it to fail on unclosed '/'.
  pChunks :: Parser [Text]
  pChunks = (pSlashFencedRegex <|> pUnfencedRegex) `sepBy` space1
  pSlashFencedRegex = do
    void $ char '/'
    chunks <- manyTill pFencedChunk (char '/' <?> "closing '/'")
    pure (T.concat chunks)
  -- TODO why do we need the first one below?
  pUnfencedRegex = do
    void $ notFollowedBy (char '/')
    chunks <- some pUnfencedChunk
    pure $ T.concat chunks

  pFencedEscapedChunk = backslashEscapedCharOf "\\/"
  pUnfencedEscapedChunk = backslashEscapedCharOf "\\/ "

  pFencedChunk =
    pFencedEscapedChunk
      <|> takeWhile1P (Just "quoted text") (`notElem` ("/\\" :: String))

  pUnfencedChunk = pUnfencedEscapedChunk <|> takeWhile1P (Just "unquoted text") (\c -> not (isSpace c) && c /= '\\')

backslashEscapedCharOf :: String -> Parser Text
backslashEscapedCharOf chars = T.singleton <$> (char '\\' >> oneOf chars)
