-- | Some functions for rendering attrs in Brick.
module Components.Attr where

import AppAttr
import Attr
import Brick

-- NB it's slightly unfortunate that both Brick and us use the term 'Attr' to refer to different things, but w/e.

-- TODO WIP this works but the attrs look kinda atrocious b/c I don't match them to the background if selected.
-- - fix that fact.
-- - Also the colors are kinda bad generally.
-- - Improve rendering for the rows in MainTree, see there.
renderStatus :: Status -> Widget n
renderStatus a = case a of
  Next -> withAttr (attrName "status_next") (str "N")
  Waiting -> withAttr (attrName "status_waiting") (str "W")
  Project -> withAttr (attrName "status_project") (str "P")

renderMaybeStatus :: Maybe Status -> Widget n
renderMaybeStatus = maybe (str " ") renderStatus
