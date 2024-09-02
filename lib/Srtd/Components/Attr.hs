-- | Some functions for rendering attrs in Brick.
module Srtd.Components.Attr where

import Brick
import Data.Time (ZonedTime)
import Srtd.AppAttr
import Srtd.Attr
import Srtd.Dates

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
