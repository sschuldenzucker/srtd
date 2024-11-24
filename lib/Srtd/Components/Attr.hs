{-# LANGUAGE TemplateHaskell #-}

-- | Some functions for rendering attrs in Brick.
module Srtd.Components.Attr where

import Brick hiding (on)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Time (ZonedTime (..))
import Lens.Micro.Platform
import Srtd.AppAttr
import Srtd.Attr
import Srtd.BrickHelpers
import Srtd.Dates
import Srtd.Todo

-- NB it's slightly unfortunate that both Brick and us use the term 'Attr' to refer to different things, but w/e.

-- TODO We use `withDefAttr` for selected in the context where this is rendered, therefore we *can* actually make it status.waiting.selected where .selected is optional. Do that! Makes cleaner theme files.

-- SOMEDAY we can prob find something more elegant using barbies or something. Or maybe not idk.
-- NB in principle we also need different ordering for different kinds of things. E.g., remind is begin-of-day but deadline is end-of-day.
data Labeled a = Labeled
  { lblValue :: a
  , lblAttrName :: String
  , lblShortLabel :: String
  }
  deriving (Show, Functor, Foldable, Traversable)

suffixLenses ''Labeled

lbl :: a -> String -> String -> Labeled a
lbl = Labeled

-- Order matters. First have highest priority if equally urgent
labeledDateLenses :: [Labeled (AttrDates -> Maybe DateOrTime)]
labeledDateLenses =
  [ lbl deadline "deadline" "D"
  , lbl goalline "goalline" "G"
  , lbl scheduled "scheduled" "S"
  , lbl remind "remind" "R"
  ]

-- NB this only uses the time zone component rn, but this may change.
mostUrgentDateLabeled :: ZonedTime -> AttrDates -> Maybe (Labeled DateOrTime)
-- TODO I think this sorting is kinda broken? Sign error? Also, it should be either beginning or end
-- of day depending on what it is.
mostUrgentDateLabeled (ZonedTime _ tz) =
  listToMaybe
    . sortBy (compareDateOrTime EndOfDay tz `on` lblValue)
    . catMaybeVals
    . applyDates
  where
    applyDates dates = map (lblValueL %~ ($ dates)) labeledDateLenses
    catMaybeVals = catMaybes . map (traverse id)

renderLabeledDate :: ZonedTime -> Bool -> Labeled DateOrTime -> Widget n
renderLabeledDate now sel ldt = withAttr (dateAttrForLabeled now sel ldt) (str label)
  where
    label = lblShortLabel ldt ++ " " ++ prettyRelativeMed now (lblValue ldt)

dateAttrForLabeled :: ZonedTime -> Bool -> Labeled DateOrTime -> AttrName
dateAttrForLabeled now sel ldt = rootattr <> kindattr <> subattr <> selattr
  where
    dt = lblValue ldt
    rootattr = attrName "date"
    kindattr = attrName . lblAttrName $ ldt
    subattr
      | dt `isBefore` now = attrName "overdue"
      | dt `isTodayOf` now = attrName "today"
      | dt `isTomorrowOf` now = attrName "tomorrow"
      -- SOMEDAY style for next 7 days (like todoist)
      | otherwise = mempty
    selattr = if sel then attrName "selected" else mempty

-- | Only get the attr of the most urgent date
mostUrgentDateAttr :: ZonedTime -> Bool -> AttrDates -> AttrName
mostUrgentDateAttr now sel dates = maybe mempty (dateAttrForLabeled now sel) (mostUrgentDateLabeled now dates)

renderMostUrgentDate :: ZonedTime -> Bool -> AttrDates -> Widget n
renderMostUrgentDate now sel dates =
  setWidth 13 . maybe almostEmptyWidget (renderLabeledDate now sel) . mostUrgentDateLabeled now $
    dates

-- | Variant of 'renderMostUrgentDate' that does not have a fixed width, but minimal, and returns
-- 'Nothing' if there's nothing to display.
renderMostUrgentDateMaybe :: ZonedTime -> Bool -> AttrDates -> Maybe (Widget n)
renderMostUrgentDateMaybe now sel dates = fmap (renderLabeledDate now sel) . mostUrgentDateLabeled now $ dates

renderPastDate :: ZonedTime -> Bool -> DateOrTime -> Widget n
-- SOMEDAY styling options (`withAttr`), but I'm not using them rn.
renderPastDate now _sel dt = setWidth 10 $ str label
  where
    label = prettyRelativeMed now dt

-- | `renderMStatus selected status actionability`
renderStatus :: Bool -> Status -> Status -> Widget n
renderStatus sel a act = withAttr (rootAttr <> subAttr) (str sym)
  where
    rootAttr = if sel then selectedItemRowAttr <> attrName "status" else attrName "status"
    subAttr = attrName . status2subAttrName $ act
    status2subAttrName s = case s of
      Next -> "next"
      Waiting -> "waiting"
      Project -> "project"
      Later -> "later"
      WIP -> "wip"
      Someday -> "someday"
      Done -> "done"
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
      Open -> "o"
      None -> "-"
