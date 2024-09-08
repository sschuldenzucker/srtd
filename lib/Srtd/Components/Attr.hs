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
import Srtd.Dates
import Srtd.Todo

-- NB it's slightly unfortunate that both Brick and us use the term 'Attr' to refer to different things, but w/e.

-- TODO We use `withDefAttr` for selected in the context where this is rendered, therefore we *can* actually make it status.waiting.selected where .selected is optional. Do that! Makes cleaner theme files.

-- - Improve rendering for the rows in MainTree, see there.
renderStatus :: Bool -> Status -> Widget n
renderStatus sel a = case a of
  Next -> withAttr (root <> attrName "next") (str "*")
  Waiting -> withAttr (root <> attrName "waiting") (str "<")
  Project -> withAttr (root <> attrName "project") (str ">")
  Later -> withAttr (root <> attrName "later") (str "/")
  WIP -> withAttr (root <> attrName "wip") (str "*")
  Someday -> withAttr (root <> attrName "someday") (str "~")
  Done -> withAttr (root <> attrName "done") (str "+")
  Open -> withAttr (root <> attrName "open") (str "o")
  where
    root = if sel then selectedItemRowAttr <> attrName "status" else attrName "status"

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
renderLabeledDate now sel ldt = withAttr (rootattr <> kindattr <> subattr <> selattr) (str label)
  where
    dt = lblValue ldt
    label = lblShortLabel ldt ++ " " ++ prettyRelativeMed now dt
    rootattr = attrName "date"
    kindattr = attrName . lblAttrName $ ldt
    subattr
      | dt `isBefore` now = attrName "overdue"
      | dt `isTodayOf` now = attrName "today"
      | dt `isTomorrowOf` now = attrName "tomorrow"
      | otherwise = mempty
    selattr = if sel then attrName "selected" else mempty

renderMostUrgentDate :: ZonedTime -> Bool -> AttrDates -> Widget n
renderMostUrgentDate now sel dates = setWidth 12 . maybe almostEmptyWidget (renderLabeledDate now sel) . mostUrgentDateLabeled now $ dates
  where
    setWidth w = hLimit w . padRight Max
    -- BUG WORKAROUND: Using emptyWidget or str "" instead doesn't grow right in setWidth. No idea why.
    almostEmptyWidget = str " "

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
  where
    setWidth w = hLimit w . padRight Max
    -- BUG WORKAROUND: Using emptyWidget or str "" instead doesn't grow right in setWidth. No idea why.
    almostEmptyWidget = str " "

renderMaybeStatus :: Bool -> Maybe Status -> Widget n
renderMaybeStatus sel = maybe (str "-") (renderStatus sel)
