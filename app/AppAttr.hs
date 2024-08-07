-- | Brick attributes
module AppAttr where

import Brick.AttrMap
import Brick.Themes
import Brick.Util (on)
import Graphics.Vty.Attributes

selectedItemRowAttr :: AttrName
selectedItemRowAttr = attrName "selectedItemRow"

myAttrMap :: AttrMap
myAttrMap = themeToAttrMap $ newTheme (green `on` black) [(selectedItemRowAttr, black `on` green)]
