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
  { lblValue :: a,
    lblAttrName :: String,
    lblShortLabel :: String
  }
  deriving (Show, Functor, Foldable, Traversable)

suffixLenses ''Labeled

lbl :: a -> String -> String -> Labeled a
lbl = Labeled

-- Order matters. First have highest priority if equally urgent
labeledDateLenses :: [Labeled (AttrDates -> Maybe DateOrTime)]
labeledDateLenses =
  [ lbl deadline "deadline" "D",
    lbl goalline "goalline" "G",
    lbl scheduled "scheduled" "S",
    lbl remind "remind" "R"
  ]

-- NB this only uses the time zone component rn, but this may change.
mostUrgentDateLabeled :: ZonedTime -> AttrDates -> Maybe (Labeled DateOrTime)
-- TODO I think this sorting is kinda broken? Sign error?
mostUrgentDateLabeled (ZonedTime _ tz) = listToMaybe . sortBy (ordBefore tz `on` lblValue) . catMaybeVals . applyDates
  where
    applyDates dates = map (lblValueL %~ ($ dates)) labeledDateLenses
    catMaybeVals = catMaybes . map (traverse id)

-- TODO WIP Now use `mostUrgentDateLabeled` to render the most urgent date. Replace `renderDeadline` prob.
-- (this can be subsumed later)

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
renderMostUrgentDate now sel dates = setWidth 12 . maybe almostEmptyWidget (renderLabeledDate now sel) . mostUrgentDateLabeled now $ dates

-- | Variant of 'renderMostUrgentDate' that does not have a fixed width, but minimal, and returns
-- 'Nothing' if there's nothing to display.
renderMostUrgentDateMaybe :: ZonedTime -> Bool -> AttrDates -> Maybe (Widget n)
renderMostUrgentDateMaybe now sel dates = fmap (renderLabeledDate now sel) . mostUrgentDateLabeled now $ dates

renderPastDate :: ZonedTime -> Bool -> DateOrTime -> Widget n
-- SOMEDAY styling options (`withAttr`), but I'm not using them rn.
renderPastDate now _sel dt = setWidth 10 $ str label
  where
    label = prettyRelativeMed now dt

-- SOMEDAY remove, is dead
renderDeadline :: ZonedTime -> Bool -> DateOrTime -> Widget n
renderDeadline now sel dt = withAttr (rootattr <> subattr <> selattr) . str . prettyRelativeMed now $ dt
  where
    rootattr = attrName "date" <> attrName "deadline"
    subattr
      | dt `isBefore` now = attrName "overdue"
      | dt `isTodayOf` now = attrName "today"
      | dt `isTomorrowOf` now = attrName "tomorrow"
      | otherwise = mempty
    selattr = if sel then attrName "selected" else mempty

-- SOMEDAY this is a bit wide and also too narrow for all dates. Can be fixed by implementing a
-- display variant that loses information the further out, e.g., ignore time unless today, ignore
-- year unless after this year, etc. We're semi doing this rn.
renderMaybeDeadline :: ZonedTime -> Bool -> Maybe DateOrTime -> Widget n
renderMaybeDeadline now sel = setWidth 10 . maybe almostEmptyWidget (renderDeadline now sel)

-- | `renderMStatus selected status actionability`
renderMStatus :: Bool -> Maybe Status -> Maybe Status -> Widget n
renderMStatus sel a act = withAttr (rootAttr <> subAttr) (str sym)
  where
    rootAttr = if sel then selectedItemRowAttr <> attrName "status" else attrName "status"
    subAttr = maybe mempty (attrName . status2subAttrName) $ act
    status2subAttrName s = case s of
      Next -> "next"
      Waiting -> "waiting"
      Project -> "project"
      Later -> "later"
      WIP -> "wip"
      Someday -> "someday"
      Done -> "done"
      Open -> "open"
    sym = maybe "-" status2Symbol $ a
    status2Symbol s = case s of
      Next -> "*"
      Waiting -> "<"
      Project -> ">"
      Later -> "/"
      WIP -> "*"
      Someday -> "~"
      Done -> "+"
      Open -> "o"
