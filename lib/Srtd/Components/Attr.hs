-- | Some functions for rendering attrs in Brick.
module Srtd.Components.Attr where

import Brick
import Srtd.AppAttr
import Srtd.Attr

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

renderMaybeStatus :: Bool -> Maybe Status -> Widget n
renderMaybeStatus sel = maybe (str "-") (renderStatus sel)
