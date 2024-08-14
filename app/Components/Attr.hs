-- | Some functions for rendering attrs in Brick.
module Components.Attr where

import AppAttr
import Attr
import Brick

-- NB it's slightly unfortunate that both Brick and us use the term 'Attr' to refer to different things, but w/e.

-- TODO WIP this works but the attrs look kinda atrocious b/c I don't match them to the background if selected.
-- - Improve rendering for the rows in MainTree, see there.
renderStatus :: Bool -> Status -> Widget n
renderStatus sel a = case a of
  Next -> withAttr (root <> attrName "next") (str "*")
  Waiting -> withAttr (root <> attrName "waiting") (str "<")
  Project -> withAttr (root <> attrName "project") (str ">")
  Later -> withAttr (root <> attrName "later") (str "/")
  WIP -> withAttr (root <> attrName "wip") (str ">") -- yeah we use the same for project and wip right now. Maybe it's fine.
  Someday -> withAttr (root <> attrName "someday") (str "~")
  Done -> withAttr (root <> attrName "done") (str "+")
  where
    root = if sel then selectedItemRowAttr <> attrName "status" else attrName "status"

renderMaybeStatus :: Bool -> Maybe Status -> Widget n
renderMaybeStatus sel = maybe (str "-") (renderStatus sel)
