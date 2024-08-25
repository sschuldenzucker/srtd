{-# LANGUAGE OverloadedStrings #-}

-- | Brick attributes
module AppAttr where

import Brick.AttrMap (AttrName, attrName)

-- add 'TomlBiMap' and 'Key' here optionally

selectedItemRowAttr :: AttrName
selectedItemRowAttr = attrName "selected"

statusAttr :: AttrName
statusAttr = attrName "status"

filterLabelAttr :: AttrName
filterLabelAttr = attrName "filter_label"
