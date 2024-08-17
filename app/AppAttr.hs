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

data AppTheme = CatppuccinDark | CatppuccinLight deriving (Show, Enum)

cycleAppTheme :: AppTheme -> AppTheme
cycleAppTheme CatppuccinDark = CatppuccinLight
cycleAppTheme CatppuccinLight = CatppuccinDark

-- TODO
getAttrMapForAppTheme _ = myAttrMap

myAttrMap :: AttrMap
myAttrMap = themeToAttrMap $ defaultTheme

asChildrenOf :: AttrName -> [(AttrName, Attr)] -> [(AttrName, Attr)]
asChildrenOf root pairs = [(root <> name, attr) | (name, attr) <- pairs]

srgbMono :: (Integral i) => i -> Color
srgbMono c = srgbColor c c c

-- SOMEDAY these colors suuuuck. Also, we prob want the cursor less bright.
-- Maybe just take from a color scheme, e.g., catpuccin.

darkGray :: Color
darkGray = srgbMono 0x77

-- Not really sure what I'm doing here. There are 3 color transformations,,,
gray :: Color
gray = srgbMono 0xcc

white = srgbMono 0xf0

black = srgbMono 0x05

darkBlue = srgbColor 0x00 0x00 0xcc

darkGreen = srgbColor 0x00 0xbb 0x00

darkCyan = srgbColor 0x00 0x8c 0x9e

orange = srgbColor 0xff 0xb7 0x4d

darkOrange = srgbColor 0xfb 0xaf 0x3b

purple = srgbColor 0xd1 0xc4 0xe9

deepPurple = srgbColor 0x67 0x3a 0xb7

defaultTheme :: Theme
defaultTheme =
  newTheme
    (white `on` darkGray)
    $ [(selectedItemRowAttr, darkGray `on` white)]
      ++ asChildrenOf
        (attrName "status")
        [ (attrName "next", fg green),
          (attrName "waiting", fg gray),
          (attrName "project", fg blue),
          (attrName "later", fg cyan),
          (attrName "wip", fg orange),
          (attrName "done", fg green),
          (attrName "someday", fg purple)
        ]
      ++ asChildrenOf
        (selectedItemRowAttr <> attrName "status")
        [ (attrName "next", fg darkGreen),
          (attrName "waiting", fg darkGray),
          (attrName "project", fg darkBlue),
          (attrName "later", fg darkCyan),
          (attrName "wip", fg darkOrange),
          (attrName "done", fg darkGreen),
          (attrName "someday", fg deepPurple)
        ]
