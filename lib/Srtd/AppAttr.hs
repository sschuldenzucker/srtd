-- | Brick attributes. To be imported qualified.
module Srtd.AppAttr where

import Brick.AttrMap (AttrName, attrName)

-- add 'TomlBiMap' and 'Key' here optionally

tab_bar :: AttrName
tab_bar = attrName "tab_bar"

header_row :: AttrName
header_row = attrName "header_row"

selected_item_row :: AttrName
selected_item_row = attrName "selected"

status :: AttrName
status = attrName "status"

filter_label :: AttrName
filter_label = attrName "filter_label"

-- TODO these indicators should move to a status bar and they don't need their own attr each I think
-- Right now we just hardcode it to the same attr as the filter label
follow_box :: AttrName
-- follow_box = attrName "follow_box"
follow_box = filter_label

collapsed_marker :: AttrName
collapsed_marker = attrName "collapsed_marker"

breadcrumbs :: AttrName
breadcrumbs = attrName "breadcrumbs"
