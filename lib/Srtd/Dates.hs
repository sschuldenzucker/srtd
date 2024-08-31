{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Srtd.Dates where

import Control.Monad (void, when)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Void
import GHC.Generics
import Srtd.Util (eitherToMaybe, maybeToEither)
import Text.Megaparsec
import Text.Megaparsec.Char

-- | Type for deadlines, reminders, etc.
--
-- No Ord instance is provided because *different* behaviors (beginning of day vs end of day) are
-- expected in different situations.
--
-- SOMEDAY Note that Day is fundamentally local while UTCTime is of course independent of time zone.
-- Not sure if it's a non-issue or wat do. (maybe the caller has to deal with that)
-- Currently, Day is *determined* in a local fashion when setting, but that's also fine.
data DateOrTime = OnlyDate Day | DateAndTime UTCTime deriving (Eq, Show, Generic)

jsonOptionsDateOrTime :: Options
jsonOptionsDateOrTime = defaultOptions {sumEncoding = UntaggedValue}

-- DateOrTime is encoded transparently (untagged) to give nicer JSON.
instance ToJSON DateOrTime where
  toJSON = genericToJSON jsonOptionsDateOrTime
  toEncoding = genericToEncoding jsonOptionsDateOrTime

instance FromJSON DateOrTime where
  parseJSON = genericParseJSON jsonOptionsDateOrTime

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

-- | Quick entry point: Parse and interpret, and discard errors.
parseInterpretHumanDateOrTime :: Text -> ZonedTime -> Maybe DateOrTime
parseInterpretHumanDateOrTime s now = do
  hdt <- parseHumanDateOrTime s
  eitherToMaybe $ interpretHumanDateOrTime hdt now

-------------------------------------------------------------------------------
-- Interpreting
-------------------------------------------------------------------------------

-- | Given the current time, interpret the human date.
--
-- May fail when an invalid date was specified, e.g. '31st' when it's currently April.
interpretHumanDateOrTime :: HumanDateOrTime -> ZonedTime -> Either String DateOrTime
interpretHumanDateOrTime hdt now = case hdt of
  HDateMaybeTime hd Nothing -> OnlyDate <$> interpretHumanDate hd
  HDateMaybeTime hd (Just tod) -> do
    day <- interpretHumanDate hd
    let beginOfDay = ZonedTime (LocalTime day midnight) (zonedTimeZone now)
    let deltaTOD = daysAndTimeOfDayToTime 0 tod
    return $ DateAndTime . zonedTimeToUTC $ mapZonedTime (addLocalTime deltaTOD) beginOfDay
  HDiffTime dt -> Right $ DateAndTime . zonedTimeToUTC $ mapZonedTime (addLocalTime dt) now
  HTimeOnly tod ->
    let deltaTOD = daysAndTimeOfDayToTime 0 tod
     in -- SOMEDAY if at 9:00 this is set to 8:00, do we want 8:00 of the *current* day (in the past) or of the *next* day?
        -- Currently, it's the current day (in the past).
        Right $ DateAndTime . zonedTimeToUTC $ mapZonedTime (addLocalTime deltaTOD) beginOfToday
  where
    -- Current day in *local* time.
    today :: Day
    today = localDay . zonedTimeToLocalTime $ now

    -- Beginning (i.e. 00:00) of the current day in *local* time.
    beginOfToday :: ZonedTime
    beginOfToday = ZonedTime (LocalTime today midnight) (zonedTimeZone now)

    interpretHumanDate :: HumanDate -> Either String Day
    interpretHumanDate (HDAbs day) = return $ day
    interpretHumanDate (HDDiff dd) = return $ addGregorianDurationRollOver dd today
    interpretHumanDate (HDToNext n anchor)
      | n <= 0 = Left $ "Invalid number of steps: " ++ show n
      | otherwise = stepOverAnchorTimes n anchor $ today

-- | Step n times over the given anchor. Require `times > 0`.
--
-- This always goes forward in time. For example, if it's the 4th today, then '4th' will give the
-- 4th of the *next* month. The assumption is that there is a reason that the user didn't write
-- 'tod'. This is different from todoist, for instance.
--
-- The function directly takes the `times` parameter instead of iterating to handle corner cases.
-- E.g., `in 2 31st` should go to the 31st of 2 months later, but if that's possible, then the next
-- month doesn't have a 31st, so we can't just walk to the next 31st twice.
stepOverAnchorTimes :: Int -> DateAnchor -> Day -> Either String Day
stepOverAnchorTimes n (AnchorDayOfWeek dow) day = return $ addDays ((toInteger n - 1) * 7) $ nextWeekday dow day
stepOverAnchorTimes n (AnchorDayOfMonth dom) day =
  let MonthDay currentMonth currentMonthDay = day
      deltaMonths = toInteger $ if currentMonthDay < dom then n - 1 else n
   in maybeToEither "Invalid day of month" $ fromMonthDayValid (addMonths deltaMonths currentMonth) dom
stepOverAnchorTimes n (AnchorMonthDay m d) day =
  let YearMonthDay currentYear currentMonth currentMonthDay = day
      deltaYears = toInteger $ if (currentMonth, currentMonthDay) < (m, d) then n - 1 else n
   in maybeToEither "Invalid month-day" $ fromGregorianValid (currentYear + deltaYears) m d

-- | Next of the given weekday strictly after the given day.
--
-- One of those things that should really be part of time.
nextWeekday :: DayOfWeek -> Day -> Day
nextWeekday dow day = addDays deltaDays day
  where
    (_, _, currentDOW) = toWeekDate day
    dowInt = fromEnum dow
    deltaDays0 = (dowInt - currentDOW) `mod` 7
    -- Ensure we go strictly forward:
    deltaDays = toInteger $ if deltaDays0 == 0 then 7 else deltaDays0

mapZonedTime :: (LocalTime -> LocalTime) -> ZonedTime -> ZonedTime
mapZonedTime f (ZonedTime t tz) = ZonedTime (f t) tz

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

-- | Pretty-render the date/time wrt. the given time zone.
prettyAbsolute :: TimeZone -> DateOrTime -> String
-- SOMEDAY This commits to making OnlyDate in local time zone, which is slightly inconsistent (see
-- above)
prettyAbsolute _ (OnlyDate day) = formatTime defaultTimeLocale "%a, %Y-%m-%d" day
-- SOMEDAY show time zone (%Z). Needs an extended locale though, the default doesn't know CET for
-- instance by name.
prettyAbsolute tz (DateAndTime time) = formatTime defaultTimeLocale "%a, %Y-%m-%d %H:%M" zonedTime
  where
    zonedTime = utcToZonedTime tz time

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

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
-- - 3h  (this is why we can't use '17h' as a short notation, would clash here)
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
  -- We don't support seconds b/c we don't need it rn.
  case makeTimeOfDayValid h (fromMaybe 0 mm) 0 of
    Nothing -> fail "Invalid time of day"
    Just res -> return res

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
