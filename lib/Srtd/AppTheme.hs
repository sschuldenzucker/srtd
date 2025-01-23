{-# LANGUAGE DerivingVia #-}

{-| My own theming and theme management. For loading themes from toml files (see README).

Similar to Brick's built-in theme support but I wanted indirection for colors (which Brick doesn't support).
-}
module Srtd.AppTheme where

import Brick (AttrName, attrName)
import Brick.Themes
import Control.Monad (foldM, forM, forM_)
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.Bits ((.|.))
import Data.Graph
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import GHC.Generics
import Graphics.Vty.Attributes
import Srtd.Log
import Srtd.Util (for)
import System.Directory
import System.FilePath.Posix
import Toml qualified
import Toml.Schema

-- We first define a key-validated but otherwise untyped data structure, then parse this into a
-- format that can be understood by Brick.

data ThemeFile = ThemeFile
  { palette :: Map Text Text
  , theme :: Map Text MyAttr
  , -- SOMEDAY we could have multiple inherits easily but this is how helix does it so let's keep it.
    inherits :: Maybe Text
  , defaultAttr :: MyAttr
  }
  deriving (Show, Generic)
  deriving (FromValue) via GenericTomlTable ThemeFile

data MyAttr = MyAttr
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
  , ("normal", defaultStyleMask)
  , ("", defaultStyleMask)
  ]

parseStyle1 :: Text -> Either Text Style
parseStyle1 s = maybe (Left $ "Unknown style: " <> s) Right $ lookup s allStyles

parseStyles :: [Text] -> Either Text Style
parseStyles = fmap (foldl (.|.) defaultStyleMask) . mapM parseStyle1

parseStyle :: Text -> Either Text Style
parseStyle = parseStyles . T.split (== ',')

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

-- | This ignores the `inherits` directive; you need to preprocess this!
themeFileToThemeIgnoreInherits :: ThemeFile -> Either Text Theme
themeFileToThemeIgnoreInherits ThemeFile {palette, theme, defaultAttr} = do
  palette' <- mapM parseColor palette
  defaultAttr' <- myAttrToAttr palette' defaultAttr
  theme' <- themeMapToAttrPairs palette' theme
  return $ newTheme defaultAttr' theme'

-- | Inherit a theme from a parent (first arg) into a child (second arg).
--
-- Inherits work by *shallow* merge for palette and theme only.
inheritThemeFile :: ThemeFile -> ThemeFile -> ThemeFile
inheritThemeFile parent child =
  ThemeFile
    { palette = Map.unionWith (flip const) (palette parent) (palette child)
    , theme = Map.unionWith (flip const) (theme parent) (theme child)
    , inherits = Nothing
    , defaultAttr = defaultAttr child
    }

-- | Handle inherits within a collection of themes. The resulting map has no inherits set (and thus
-- inherits can be safely ignored afterwards)
inheritThemeFiles :: Map Text ThemeFile -> Either Text (Map Text ThemeFile)
inheritThemeFiles themeFiles = foldM go Map.empty themefilesInheritSort
 where
  -- A -> B if A inherits from B.
  (inheritsGraph, vx2node, _) =
    graphFromEdges
      [ (themefile, name, maybe [] return (inherits themefile)) | (name, themefile) <- Map.toList themeFiles
      ]
  -- Sorted in the order they need to be processed. If there's a cycle, then not, and this is gonna
  -- error out.
  themefilesInheritSort = for (reverseTopSort inheritsGraph) $ \vx ->
    let (themefile, name, _) = vx2node vx
     in (name, themefile)
  go res (name, themefile) = case inherits themefile of
    Nothing -> Right $ Map.insert name themefile res
    Just pname -> case Map.lookup pname res of
      Just pthemefile -> Right $ Map.insert name (inheritThemeFile pthemefile themefile) res
      Nothing -> Left $ "File not found or circular `inherits` dependency for " <> name

themefilesToThemes :: Map Text ThemeFile -> Either Text (Map Text Theme)
themefilesToThemes themeFiles = do
  themeFilesPostInherit <- inheritThemeFiles themeFiles
  mapM themeFileToThemeIgnoreInherits themeFilesPostInherit

readThemeFile :: FilePath -> ExceptT Text IO ThemeFile
readThemeFile p = do
  content <- lift $ T.readFile p
  let res :: Toml.Result String ThemeFile = Toml.decode content
  case res of
    Toml.Failure errors -> do
      logParseErrors ERROR errors
      fail "Errors while reading theme file, see log"
    Toml.Success warnings themefile -> do
      logParseErrors WARNING warnings
      return themefile
 where
  logParseErrors lvl errors = lift $ forM_ errors $ \e ->
    glogL lvl $ "While reading " ++ p ++ ": parse error: " ++ e

-- SOMEDAY error handling is quite messy around here b/c I'm using like 5 different ways to lift an Either.
-- Also, Text vs String and all

-- | Given a list of theme (.toml) files, load all of them, respecting inherits.
loadAllThemes :: String -> IO (Either Text [(Text, Theme)])
loadAllThemes themeDir = runExceptT $ do
  filenames <- filter ((== ".toml") . takeExtension) <$> lift (listDirectory themeDir)
  themeFiles <- forM filenames $ \filename -> do
    themefile <- readThemeFile $ themeDir </> filename
    return (T.pack $ takeBaseName filename, themefile)
  liftEither $ Map.toList <$> themefilesToThemes (Map.fromList themeFiles)
