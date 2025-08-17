-- | Brick attributes
module Srtd.AppAttr where

import Brick.AttrMap (AttrName, attrName)

-- add 'TomlBiMap' and 'Key' here optionally

tabBarAttr :: AttrName
tabBarAttr = attrName "tab_bar"

headerRowAttr :: AttrName
headerRowAttr = attrName "header_row"

selectedItemRowAttr :: AttrName
selectedItemRowAttr = attrName "selected"

statusAttr :: AttrName
statusAttr = attrName "status"

filterLabelAttr :: AttrName
filterLabelAttr = attrName "filter_label"

-- TODO these indicators should move to a status bar and they don't need their own attr each I think
-- Right now we just hardcode it to the same attr as the filter label
followBoxAttr :: AttrName
-- followBoxAttr = attrName "follow_box"
followBoxAttr = filterLabelAttr

collapsedMarkerAttr :: AttrName
collapsedMarkerAttr = attrName "collapsed_marker"
