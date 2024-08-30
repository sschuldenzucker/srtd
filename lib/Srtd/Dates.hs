module Srtd.Dates where

import Control.Monad (void)
import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Calendar.Month
import Data.Time.Clock
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

-- | Type for deadlines, reminders, etc.
--
-- No Ord instance is provided because *different* behaviors (beginning of day vs end of day) are
-- expected in different situations.
data DateOrTime = DateOrTime Day (Maybe UTCTime) deriving (Eq, Show)

-----------------------------
-- Parsing
-----------------------------

-- The following contain methods to parse a free-form description.
-- This is intentionally incomplete, just what I use.

-- TODO I think this sucks a bit. Our parser should prob do this.

-- (Nothing, Nothing) is invalid and the parser will never return this.
data HumanDateOrTime
  = HumanDateOrTime (Maybe HumanDay) (Maybe HumanTime)
  deriving (Eq, Show)

data HDRelativeNext
  = NextDayOfMonth Int
  | NextWeekday DayOfWeek
  | NextMonth Month
  deriving (Eq, Show)

data HumanDay
  = HDAbsolute {hdYear :: Maybe Int, hdMonth :: Maybe Int, hdDay :: Maybe Int}
  | HDRelativeNext HDRelativeNext
  | HDRelativeOffset DiffTime
  deriving (Eq, Show)

data HumanTime = HumanTime deriving (Eq, Show) -- TODO

type Parser = Parsec Void Text

type MyParseErrorBundle = ParseErrorBundle Text Void

parseHumanDateOrTime :: Text -> Maybe HumanDateOrTime
parseHumanDateOrTime = parseMaybe pHumanDateOrTime

pHumanDateOrTime :: Parser HumanDateOrTime
pHumanDateOrTime = do
  hday <- optional (try pHumanDay)
  htime <- optional pHumanTime
  if isNothing hday && isNothing htime
    then fail "No input"
    else return $ HumanDateOrTime hday htime

pHumanDay :: Parser HumanDay
-- TODO dummy (but it works)
pHumanDay = do
  year <- pNonNegInt
  void $ char '-'
  month <- pNonNegInt
  void $ char '-'
  day <- pNonNegInt
  return $ HDAbsolute (Just year) (Just month) (Just day)

pHumanTime :: Parser HumanTime
pHumanTime = fail "TODO Not Implemented"

pNonNegInt :: Parser Int
pNonNegInt = read <$> some digitChar
