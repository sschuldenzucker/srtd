{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | My own theming and theme management. For loading themes from toml files (see README).
--
-- Similar to Brick's built-in theme support but I wanted indirection for colors (which Brick doesn't support).
module Srtd.AppTheme where

import Brick (AttrName, attrName)
import Brick.Themes
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import GHC.Generics
import Graphics.Vty.Attributes
import Toml.Schema

-- We first define a key-validated but otherwise untyped data structure, then parse this into a
-- format that can be understood by Brick.

data ThemeFile = ThemeFile
  { palette :: Map Text Text
  , theme :: Map Text MyAttr
  , defaultAttr :: MyAttr
  }
  deriving (Show, Generic)
  deriving (FromValue) via GenericTomlTable ThemeFile

data MyAttr = MyAttr
  -- SOMEDAY currently, one can only set one style, though multiple would in principle be supported (e.g. bold-italic)
  { style :: Maybe Text
  , fg :: Maybe Text
  , bg :: Maybe Text
  }
  deriving (Show, Generic)
  deriving (FromValue) via GenericTomlTable MyAttr

-- Mostly taken from Brick.Themes (which doesn't export this unfortunately)
parseColor :: Text -> Either Text Color
parseColor t = maybe (Left $ "Invalid color: " <> t) Right parsedRGB
  where
    parsedRGB =
      if T.head t /= '#'
        then Nothing
        else case mapMaybe readHex (T.chunksOf 2 (T.tail t)) of
          -- Do NOT use rgbColor or srgbColor here. Colors will be really off.
          -- SOMEDAY Maybe the color settings are wrong or something? Maybe I need to set full color for the terminal?
          [r, g, b] -> Just (linearColor r g b)
          _ -> Nothing

    readHex :: T.Text -> Maybe Int
    readHex t' = either (const Nothing) (Just . fst) (T.hexadecimal t')

type Palette = Map Text Color

-- Copied from Brick.Themes
allStyles :: [(Text, Style)]
allStyles =
  [ ("standout", standout)
  , ("underline", underline)
  , ("strikethrough", strikethrough)
  , ("reversevideo", reverseVideo)
  , ("blink", blink)
  , ("dim", dim)
  , ("bold", bold)
  , ("italic", italic)
  ]

parseStyle :: Text -> Either Text Style
parseStyle s = maybe (Left $ "Unknown style: " <> s) Right $ lookup s allStyles

myAttrToAttr :: Palette -> MyAttr -> Either Text Attr
myAttrToAttr palette MyAttr {style, fg, bg} = do
  fgTrans <- maybeGetTrans fg getColor (flip withForeColor)
  bgTrans <- maybeGetTrans bg getColor (flip withBackColor)
  styleTrans <- maybeGetTrans style parseStyle (flip withStyle)
  return $ fgTrans . bgTrans . styleTrans $ defAttr
  where
    maybeGetTrans Nothing _ _ = return id
    maybeGetTrans (Just x) fget ftrans = ftrans <$> fget x

    getColor c = maybe (Left $ "Color not found in palette: " <> c) Right $ Map.lookup c palette

parseAttrName :: Text -> AttrName
parseAttrName = mconcat . map (attrName . T.unpack) . T.splitOn "."

themeMapToAttrPairs :: Palette -> Map Text MyAttr -> Either Text [(AttrName, Attr)]
themeMapToAttrPairs palette = mapM transPair . Map.toList
  where
    transPair (attrStr, myattr) = do
      attr <- myAttrToAttr palette myattr
      return (parseAttrName attrStr, attr)

themeFileToTheme :: ThemeFile -> Either Text Theme
themeFileToTheme ThemeFile {palette, theme, defaultAttr} = do
  palette' <- mapM parseColor palette
  defaultAttr' <- myAttrToAttr palette' defaultAttr
  theme' <- themeMapToAttrPairs palette' theme
  return $ newTheme defaultAttr' theme'
