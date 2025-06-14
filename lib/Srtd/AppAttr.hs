-- | Brick attributes
module Srtd.AppAttr where

import Brick.AttrMap (AttrName, attrName)

-- add 'TomlBiMap' and 'Key' here optionally

tabBarAttr :: AttrName
tabBarAttr = attrName "tab_bar"

selectedItemRowAttr :: AttrName
selectedItemRowAttr = attrName "selected"

statusAttr :: AttrName
statusAttr = attrName "status"

filterLabelAttr :: AttrName
filterLabelAttr = attrName "filter_label"

collapsedMarkerAttr :: AttrName
collapsedMarkerAttr = attrName "collapsed_marker"
