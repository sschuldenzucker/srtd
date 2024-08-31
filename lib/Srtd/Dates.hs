{-# LANGUAGE OverloadedStrings #-}

module Srtd.Dates where

import Control.Monad (void, when)
import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Time
import Data.Void
import Srtd.Todo
import Text.Megaparsec
import Text.Megaparsec.Char

-- | Type for deadlines, reminders, etc.
--
-- No Ord instance is provided because *different* behaviors (beginning of day vs end of day) are
-- expected in different situations.
data DateOrTime = OnlyDate Day | DateAndTime UTCTime deriving (Eq, Show)

-----------------------------
-- Parsing
-----------------------------

-- The following contain methods to parse a free-form description.
-- This is intentionally incomplete, just what I use.

-- | A date and/or time reference expressed by a human.
--
-- This may look structurally a bit funky. For example, 'in 1 day' is structurally in a different
-- place than 'in 1 hour'. This is because humans likely mean different things by these: 'in 1 hour' is
-- a precise time difference, but 'in 1 day' could refer to the beginning or end of tomorrow depending
-- on the situation.
data HumanDateOrTime
  = -- | Date (relative or absolute), optionally time.
    HDateMaybeTime HumanDate (Maybe TimeOfDay)
  | -- | Time absolute with in an implicit day. Like "17:00"
    HTimeOnly TimeOfDay
  | -- | Relative time difference, like "in 1h"
    HDiffTime NominalDiffTime
  deriving (Eq, Show)

data HumanDate
  = -- | Specific date, independent of a reference date.
    HDAbs Day
  | -- | Next relative anchor, e.g., 'in 2 mon' = 2nd monday from now.
    -- NB this allows some weird but formally ok constructions like 'in 2 11-23' = 2 years from now on 11-23.
    -- Some of these may still fail to apply later, e.g., if we want the 31st but it's April right now.
    HDToNext Int DateAnchor
  | -- | Relative difference, e.g., 'in 2 days' or 'tom'
    HDDiff CalendarDiffDays
  deriving (Eq, Show)

data DateAnchor
  = AnchorDayOfWeek DayOfWeek
  | AnchorDayOfMonth Int
  | AnchorMonthDay Int Int
  deriving (Eq, Show)

type Parser = Parsec Void Text

type MyParseErrorBundle = ParseErrorBundle Text Void

-- | See `pHumanDateOrTime`
parseHumanDateOrTime :: Text -> Maybe HumanDateOrTime
parseHumanDateOrTime = parseMaybe pHumanDateOrTime

-- | Parse a human-language date or time.
--
-- This is a pretty terse language rn, optimized to minimize the number of keystrokes rather than be intuitive.
--
-- By example (also see the tests):
--
-- - 2024-07-18
-- - 24-7-18
-- - 18th
-- - 11-18
-- - tod
-- - tom
-- - fri
-- - mon 17:00
-- - mon 17t  (short form for 17:00, *not* '17h'!)
-- - in 2 days
-- - 2d
-- - in 3 hours
-- - 3h
pHumanDateOrTime :: Parser HumanDateOrTime
pHumanDateOrTime = space >> (try pHDateMaybeTime <|> try pHTimeOnly <|> pHDiffTime) <* (space >> eof)

pHDateMaybeTime :: Parser HumanDateOrTime
pHDateMaybeTime = do
  hdate <- pHumanDate
  mhtime <- optional (space1 >> pTimeOfDay)
  return $ HDateMaybeTime hdate mhtime

pHumanDate :: Parser HumanDate
pHumanDate = do
  try pHDAbs <|> try pHDToNext <|> pHDDiff

-- | 13:00 or 13t as a short form.
--
-- Note that `13h` means "in 13 hours"!
--
-- SOMEDAY maybe we should change this, it's confusing. Prob would need to mandate 'in' for time offsets.
pTimeOfDay :: Parser TimeOfDay
pTimeOfDay = do
  h <- pNonNegInt
  mm <- try (string "t" >> return Nothing) <|> (string ":" >> Just <$> pNonNegInt)
  when (h > 23) $ fail "Invalid hour"
  when (mm > Just 59) $ fail "Invalid minute"
  -- We don't support seconds b/c we don't need it rn.
  return $ TimeOfDay h (fromMaybe 0 mm) 0

-- | "2024-03-27" or "24-03-27" as a short form.
pHDAbs :: Parser HumanDate
pHDAbs = do
  year <- fixYear <$> pNonNegInt
  void $ char '-'
  month <- pPosInt
  void $ char '-'
  day <- pPosInt
  Just theDate <- return $ fromGregorianValid (toInteger year) month day
  return $ HDAbs theDate
  where
    fixYear y = if y < 100 then 2000 + y else y

pHDToNext :: Parser HumanDate
pHDToNext =
  try ((HDToNext 1) <$> pDateAnchor) <|> do
    void $ optional (string "in" >> space)
    n <- pPosInt
    space
    a <- pDateAnchor
    return $ HDToNext n a

pDateAnchor :: Parser DateAnchor
pDateAnchor = try pDADayOfWeek <|> try pDADayOfMonth <|> pDAMonthDay

dayOfWeekPairs :: [([Text], DayOfWeek)]
dayOfWeekPairs =
  [ (["mon"], Monday),
    (["tue"], Tuesday),
    (["wed"], Wednesday),
    (["thu"], Thursday),
    (["fri"], Friday),
    (["sat"], Saturday),
    (["sun"], Sunday)
  ]

pDADayOfWeek :: Parser DateAnchor
pDADayOfWeek = AnchorDayOfWeek <$> keywords dayOfWeekPairs

pDADayOfMonth :: Parser DateAnchor
pDADayOfMonth = do
  day <- pPosInt
  -- This allows some wrong things like '2th' but is probably fine.
  void $ choice ["st", "nd", "rd", "th"]
  -- Basic sanity check. Not tight of course.
  when (day > 31) $ fail "Invalid day of month"
  return $ AnchorDayOfMonth day

pDAMonthDay :: Parser DateAnchor
pDAMonthDay = do
  month <- pPosInt
  void $ string "-"
  day <- pPosInt
  when (month > 12) $ fail "Invalid month"
  when (day > 31) $ fail "Invalid day of month"
  -- \^ We do *not* check the (day, month) combo here, b/c we can't anyways, b/c of leap years.
  return $ AnchorMonthDay month day

-- TODO Remember to implement today and tomorrow.
pHDDiff :: Parser HumanDate
pHDDiff = HDDiff <$> (try pToday <|> try pTomorrow <|> pProperOffset)
  where
    pToday = string "tod" >> return (CalendarDiffDays 0 0)
    pTomorrow = string "tom" >> return (CalendarDiffDays 0 1)
    pProperOffset = do
      void $ optional (string "in" >> space)
      n <- pNonNegInt
      space
      step <- pStep
      let res = scaleCalendarDiffDays (toInteger n) step
      return res
    pStep :: Parser CalendarDiffDays
    pStep =
      -- The default step is 1 day
      -- (disabled for now b/c it's a bit too ambiguous for my taste)
      -- <|> (return $ CalendarDiffDays 0 1)
      keywords
        [ (["days", "day", "d"], CalendarDiffDays 0 1),
          (["weeks", "week", "w"], CalendarDiffDays 0 7),
          (["months", "month", "m"], CalendarDiffDays 1 0)
        ]

pHTimeOnly :: Parser HumanDateOrTime
pHTimeOnly = HTimeOnly <$> pTimeOfDay

-- Similar to `pHDDiff`
pHDiffTime :: Parser HumanDateOrTime
pHDiffTime = HDiffTime <$> properTimeOffset
  where
    properTimeOffset = do
      void $ optional (string "in" >> space)
      n <- pNonNegInt
      space
      step <- pStep
      let res = scaleNominalDiffTime (toInteger n) step
      return res
    pStep :: Parser NominalDiffTime
    pStep =
      keywords
        [ (["hours", "hour", "h"], fromInteger (60 * 60)),
          -- We do *not* use 'm' as a short form for "minute" b/c it's already used by month. Prob fine, we don't use this really anyways.
          (["minutes", "minute", "mins", "min"], fromInteger 60),
          (["seconds", "second", "secs", "sec", "s"], fromInteger 1)
        ]

keywords :: [([Text], a)] -> Parser a
keywords = choice . map (\(ss, v) -> try $ choice (map mkKeyword ss) >> return v)
  where
    -- NB notFollowedBy is important to resolve ambiguities.
    -- For example, 'mon' (monday) and 'month' (unit of time in date) both occur and otherwise would be ambiguous.
    mkKeyword s = try (string s) >> notFollowedBy alphaNumChar

pPosInt :: Parser Int
pPosInt = do
  res <- pNonNegInt
  when (res == 0) $ fail "Must be positive"
  return res

pNonNegInt :: Parser Int
pNonNegInt = read <$> some digitChar

scaleNominalDiffTime :: (Integral a) => a -> NominalDiffTime -> NominalDiffTime
scaleNominalDiffTime n t = n' * t
  where
    n' = fromRational (toInteger n % 1)
