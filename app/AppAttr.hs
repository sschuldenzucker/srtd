-- | Brick attributes
module AppAttr where

import Brick.AttrMap
import Brick.Themes
import Brick.Util (bg, fg, on)
import Graphics.Vty.Attributes

selectedItemRowAttr :: AttrName
selectedItemRowAttr = attrName "selectedItemRow"

statusAttr :: AttrName
statusAttr = attrName "status"

myAttrMap :: AttrMap
myAttrMap = themeToAttrMap $ defaultTheme

asChildrenOf :: AttrName -> [(AttrName, Attr)] -> [(AttrName, Attr)]
asChildrenOf root pairs = [(root <> name, attr) | (name, attr) <- pairs]

-- Not really sure what I'm doing here. There are 3 color transformations,,,
gray :: Color
gray = srgbColor 0x80 0x80 0x80

defaultTheme :: Theme
defaultTheme =
  newTheme
    (green `on` black)
    $ [(selectedItemRowAttr, black `on` green)]
      ++ [ (attrName "status_next", fg green),
           (attrName "status_waiting", fg gray),
           (attrName "status_project", fg blue)
         ]
