-- | Brick attributes
module AppAttr where

import Brick.AttrMap
import Brick.Themes
import Brick.Util (bg, fg, on, style)
import Graphics.Vty.Attributes hiding (black, white)

selectedItemRowAttr :: AttrName
selectedItemRowAttr = attrName "selectedItemRow"

statusAttr :: AttrName
statusAttr = attrName "status"

myAttrMap :: AttrMap
myAttrMap = themeToAttrMap $ defaultTheme

asChildrenOf :: AttrName -> [(AttrName, Attr)] -> [(AttrName, Attr)]
asChildrenOf root pairs = [(root <> name, attr) | (name, attr) <- pairs]

srgbMono :: (Integral i) => i -> Color
srgbMono c = srgbColor c c c

darkGray :: Color
darkGray = srgbMono 0x77

-- Not really sure what I'm doing here. There are 3 color transformations,,,
gray :: Color
gray = srgbMono 0xcc

white = srgbMono 0xf0

black = srgbMono 0x05

darkBlue = srgbColor 0x00 0x00 0xcc

darkGreen = srgbColor 0x00 0xbb 0x00

defaultTheme :: Theme
defaultTheme =
  newTheme
    (white `on` darkGray)
    $ [(selectedItemRowAttr, darkGray `on` white)]
      ++ asChildrenOf
        (attrName "status")
        [ (attrName "next", fg green),
          (attrName "waiting", fg gray),
          (attrName "project", fg blue)
        ]
      ++ asChildrenOf
        (selectedItemRowAttr <> attrName "status")
        [ (attrName "next", fg darkGreen),
          (attrName "waiting", fg darkGray),
          (attrName "project", fg darkBlue)
        ]
