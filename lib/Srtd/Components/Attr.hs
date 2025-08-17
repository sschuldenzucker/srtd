-- | Some functions for rendering attrs in Brick.
module Srtd.Components.Attr where

import Brick hiding (on)
import Data.Composition ((.:))
import Data.Function (on)
import Data.Functor.Apply (gliftF2, gliftF3)
import Data.List (sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (comparing)
import Data.Ord qualified as Ord
import Data.Time (TimeZone, ZonedTime (..))
import Lens.Micro.Platform
import Safe (minimumByMay)
import Srtd.AppAttr qualified as AppAttr
import Srtd.Attr
import Srtd.BrickHelpers
import Srtd.Dates

-- NB it's slightly unfortunate that both Brick and us use the term 'Attr' to refer to different things, but w/e.

-- TODO We use `withDefAttr` for selected in the context where this is rendered, therefore we *can* actually make it status.waiting.selected where .selected is optional. Do that! Makes cleaner theme files.

data DateRenderMeta = DateRenderMeta
  { drmAttrName :: String
  , drmShortLabel :: String
  }

dateRenderMeta :: AttrDates_ DateRenderMeta
dateRenderMeta =
  AttrDates
    { deadline = DateRenderMeta "deadline" "D"
    , goalline = DateRenderMeta "goalline" "G"
    , scheduled = DateRenderMeta "scheduled" "S"
    , remind = DateRenderMeta "remind" "R"
    }

data LabeledDateOrTime = LabeledDateOrTime
  { ldtValue :: DateOrTime
  , ldtIsIndirect :: Bool
  , ldtMeta :: DateRenderMeta
  , ldtDateRule :: DateRule
  }

compareLabeledDateOrTime :: TimeZone -> LabeledDateOrTime -> LabeledDateOrTime -> Ordering
compareLabeledDateOrTime tz ld1 ld2 =
  mconcat
    [ comparing (\ld -> dateOrTimeToUTCTime (ldtDateRule ld) tz (ldtValue ld)) ld1 ld2
    , -- This makes direct values come first, which is what we want here.
      comparing ldtIsIndirect ld1 ld2
    ]

dates2LabeledList :: Bool -> AttrDates -> [LabeledDateOrTime]
dates2LabeledList isIndirect = catMaybes . attrDates2List . gliftF3 mkLabeled dateRenderMeta attrDatesDateRules
 where
  mkLabeled _ _ Nothing = Nothing
  mkLabeled meta dr (Just d) = Just $ LabeledDateOrTime d isIndirect meta dr

mostUrgentDateLabeled :: TimeZone -> AttrDates -> AttrDates -> Maybe LabeledDateOrTime
mostUrgentDateLabeled tz dates indates = minimumByMay (compareLabeledDateOrTime tz) ldates
 where
  ldates = dates2LabeledList False dates ++ dates2LabeledList True indates

renderLabeledDate :: ZonedTime -> Bool -> LabeledDateOrTime -> Widget n
renderLabeledDate now sel ld = withAttr (dateAttrForLabeled now sel ld) (str label)
 where
  label = (drmShortLabel . ldtMeta) ld ++ " " ++ prettyRelativeMed now (ldtValue ld)

dateAttrForLabeled :: ZonedTime -> Bool -> LabeledDateOrTime -> AttrName
dateAttrForLabeled now sel ld = rootattr <> kindattr <> subattr <> selattr
 where
  dt = ldtValue ld
  rootattr = attrName "date" <> (if ldtIsIndirect ld then attrName "indirect" else mempty)
  kindattr = attrName . drmAttrName . ldtMeta $ ld
  subattr
    | dt `isBefore` now = attrName "overdue"
    | dt `isTodayOf` now = attrName "today"
    | dt `isTomorrowOf` now = attrName "tomorrow"
    -- SOMEDAY style for next 7 days (like todoist)
    | otherwise = mempty
  selattr = if sel then attrName "selected" else mempty

-- | Only get the attr of the most urgent date
--
-- SOMEDAY this could take some refactoring. (the callers of this specifically)
mostUrgentDateAttr :: ZonedTime -> Bool -> AttrDates -> AttrDates -> AttrName
mostUrgentDateAttr now sel dates indates = maybe mempty (dateAttrForLabeled now sel) (mostUrgentDateLabeled tz dates indates)
 where
  tz = zonedTimeZone now

renderMostUrgentDate :: ZonedTime -> Bool -> AttrDates -> AttrDates -> Widget n
renderMostUrgentDate now sel =
  setWidth 15 . maybe almostEmptyWidget (renderLabeledDate now sel) .: mostUrgentDateLabeled tz
 where
  tz = zonedTimeZone now

-- | Variant of 'renderMostUrgentDate' that does not have a fixed width, but minimal, and returns
-- 'Nothing' if there's nothing to display.
renderMostUrgentDateMaybe :: ZonedTime -> Bool -> AttrDates -> AttrDates -> Maybe (Widget n)
renderMostUrgentDateMaybe now sel = fmap (renderLabeledDate now sel) .: mostUrgentDateLabeled tz
 where
  tz = zonedTimeZone now

renderLastModified :: ZonedTime -> Bool -> DateOrTime -> Widget n
renderLastModified now _sel dt = withAttr attr $ setWidth 9 $ str label
 where
  attr = attrName "date" <> attrName "last_modified"
  label = prettyPastStrictRelativeAdaptive False now dt

-- | `renderMStatus selected status actionability`
renderStatus :: Bool -> Status -> Status -> Widget n
renderStatus sel a act = withAttr (rootAttr <> subAttr) (str sym)
 where
  rootAttr = if sel then AppAttr.selected_item_row <> attrName "status" else attrName "status"
  subAttr = attrName . status2subAttrName $ act
  status2subAttrName s = case s of
    Next -> "next"
    Waiting -> "waiting"
    Project -> "project"
    Later -> "later"
    WIP -> "wip"
    Someday -> "someday"
    Done -> "done"
    Canceled -> "canceled"
    Open -> "open"
    -- NB this typically isn't configured in the theme, which is fine.
    None -> "none"
  sym = status2Symbol a
  status2Symbol s = case s of
    Next -> "*"
    Waiting -> "<"
    Project -> ">"
    Later -> "/"
    WIP -> "*"
    Someday -> "~"
    Done -> "+"
    Canceled -> "x"
    Open -> "o"
    None -> "-"
