{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Alignment
import AppAttr
import AppTheme qualified
import Attr
import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Themes (Theme, themeToAttrMap)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Table (columnBorders, renderTable, rowBorders, surroundingBorder, table)
import CmdlineArgs qualified as CArgs
import Component
import Components.MainTree qualified as MainTree
import Control.Arrow (second)
import Control.Monad (forM_, void)
import Control.Monad.State (liftIO)
import Data.CircularList qualified as CList
import Data.List (intersperse)
import Data.List.Zipper qualified as LZ
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (forM)
import GHC.Stack (HasCallStack)
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Keymap (KeyDesc (..))
import Lens.Micro.Platform
import Log
import Model
import ModelSaver (startModelSaver)
import ModelServer
import System.Directory (listDirectory)
import System.Exit (exitFailure)
import System.FilePath
import Todo
import Toml qualified

data AppState = AppState
  { asContext :: AppContext,
    -- SOMEDAY we may wanna keep these as MainTree so we can clone. Or 'new tab' should be a shortcut of MainTree, not sure.

    -- | We make sure that `asTabs` is never after the end and in particular that it's nonempty.
    asTabs :: LZ.Zipper SomeBrickComponent,
    -- | "Unique ID" for the next tab being opened, for resource names.
    asNextTabID :: Int,
    asOverlays :: [SomeBrickComponent],
    asHelpAlways :: Bool,
    asAttrMapRing :: CList.CList (String, AttrMap)
  }

suffixLenses ''AppState

readThemeFileOrExit :: FilePath -> IO Theme
readThemeFileOrExit p = do
  content <- T.readFile p
  let res :: Toml.Result String AppTheme.ThemeFile = Toml.decode content
  case res of
    Toml.Failure errors -> logParseErrors ERROR errors >> exitFailure
    Toml.Success warnings themefile -> do
      logParseErrors WARNING warnings
      case AppTheme.themeFileToTheme themefile of
        Left err -> logParseErrors ERROR [T.unpack err] >> exitFailure
        Right res' -> return res'
  where
    logParseErrors lvl errors = forM_ errors $ \e ->
      glogL lvl $ "While reading " ++ p ++ ": parse error: " ++ e

loadAllThemes :: IO [(String, Theme)]
loadAllThemes = do
  filenames <- filter ((== ".toml") . takeExtension) <$> listDirectory themeDir
  forM filenames $ \filename -> do
    theme <- readThemeFileOrExit $ themeDir </> filename
    return (takeBaseName filename, theme)
  where
    themeDir = "themes"

ringSelectNamedTheme :: String -> CList.CList (String, AttrMap) -> CList.CList (String, AttrMap)
ringSelectNamedTheme s ring = fromMaybe ring $ CList.findRotateTo (\(s', _) -> s' == s) ring

defaultThemeName :: String
defaultThemeName = "catppuccin_dark"

main :: IO ()
main = do
  setupLogger
  CArgs.Args {CArgs.theme_name = mtheme_name} <- CArgs.execAppParser

  allAttrMaps <- map (second themeToAttrMap) <$> loadAllThemes
  let attrMapRing = ringSelectNamedTheme (fromMaybe defaultThemeName mtheme_name) . CList.fromList $ allAttrMaps

  glogL INFO "App starting"
  modelServer <- startModelServer
  -- SOMEDAY I should probably do something with the returned thread ID.
  _ <- startModelSaver modelServer
  model <- getModel modelServer
  -- SOMEDAY it's unfortunate that this is bounded actually, could in principle lead to deadlock.
  appChan <- newBChan 100
  subscribe modelServer $ writeBChan appChan . ModelUpdated
  let appState =
        AppState
          { asContext = AppContext modelServer appChan,
            asTabs = LZ.fromList [SomeBrickComponent $ MainTree.make Vault model (Tab 0)],
            asNextTabID = 1,
            asOverlays = [],
            asHelpAlways = False,
            asAttrMapRing = attrMapRing
          }

  -- let buildVty = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  -- initialVty <- buildVty
  -- void $
  --   customMain
  --     initialVty
  --     buildVty
  --     (Just appChan)
  --     app
  --     appState
  void $ customMainWithDefaultVty (Just appChan) app appState
  glogL INFO "App did quit normally"

myAppDraw :: AppState -> [Widget AppResourceName]
myAppDraw state@(AppState {asTabs, asOverlays}) = [keyHelpUI] ++ map renderOverlay asOverlays ++ [mainUI]
  where
    tab0 = LZ.cursor asTabs
    mainUI = renderTabBar (componentTitle <$> asTabs) <=> renderComponent tab0
    renderTabTitle :: Bool -> Text -> Widget n
    renderTabTitle sel t = withAttr theAttrName . padLeftRight 1 . hLimit 20 $ txt t
      where
        theAttrName = (if sel then tabBarAttr <> attrName "selected" else tabBarAttr) <> attrName "tab_title"
    renderTabBar titlez =
      withDefAttr tabBarAttr $
        let (front, cur, back) = lzSplit3 titlez
         in hBox $
              intersperse (txt "|") $
                map (renderTabTitle False) front
                  ++ [renderTabTitle True cur]
                  ++ map (renderTabTitle False) back
                  ++ [padLeft Max (str " ")]
    renderOverlay o =
      centerLayer
        . Brick.hLimitPercent 80
        . Brick.vLimitPercent 75
        . borderWithLabel (padLeftRight 1 . txt $ componentTitle o)
        $ renderComponent o
    renderKeyHelp keymapName pairs =
      let configTable = surroundingBorder False . rowBorders False . columnBorders False
          inner = configTable $ table [[padRight (Pad 1) (txt keydesc), txt actdesc] | (keydesc, actdesc) <- pairs]
       in -- SOMEDAY minor bug: When `keymapName` is wider than the content (the key table), it's cut off.
          -- This happens in particular with smaller sub-mode keymaps.
          alignBottomRightLayer . borderWithLabel (padLeftRight 1 $ txt keymapName) $ renderTable inner
    keyHelpUI =
      let KeyDesc keymapName isToplevel keydescs = componentKeyDesc $ state ^. activeComponentL
       in if not isToplevel || (asHelpAlways state) then renderKeyHelp keymapName keydescs else emptyWidget

myHandleEvent :: BrickEvent AppResourceName AppMsg -> EventM AppResourceName AppState ()
myHandleEvent ev =
  dbgprint >> case ev of
    (VtyEvent (EvKey (KChar 'q') [MCtrl])) -> do
      liftIO (glogL INFO "quitting...")
      halt
    -- Toggle: Always show overlay. (o/w only at the top level)
    -- TODO I have no idea why Ctrl+/ is registered as Ctrl+_ but here we are.
    (VtyEvent (EvKey (KChar '_') [MCtrl])) -> do
      asHelpAlwaysL %= not
    (VtyEvent (EvKey (KFun 10) [])) -> do
      asAttrMapRingL %= CList.rotR
    (AppEvent (PopOverlay _)) -> do
      modify popOverlay
      -- This is some unclean design right here. Ideally the caller-callee relationship should specify return values. :/
      AppState {asContext} <- get
      zoom activeComponentL $ handleEvent asContext ev
    (AppEvent (PushOverlay o)) -> do
      modify $ pushOverlay o
    (AppEvent (PushTab t)) -> modify $ pushTab t
    (AppEvent PopTab) -> modify popTab
    (AppEvent NextTab) -> asTabsL %= lzCircRight
    (AppEvent PrevTab) -> asTabsL %= lzCircLeft
    (AppEvent (ModelUpdated _)) -> do
      AppState {asContext} <- get
      forComponentsM $ handleEvent asContext ev
    _ -> do
      AppState {asContext} <- get
      zoom activeComponentL $ handleEvent asContext ev
  where
    dbgprint = liftIO $ glogL INFO $ "Received: " ++ show ev

forComponentsM :: EventM AppResourceName SomeBrickComponent () -> EventM AppResourceName AppState ()
forComponentsM act = do
  state@(AppState {asOverlays, asTabs}) <- get
  asOverlays' <- forM asOverlays $ \t -> nestEventM' t act
  asTabs' <- forMLZ asTabs $ \t -> nestEventM' t act
  put $ state {asOverlays = asOverlays', asTabs = asTabs'}
  return ()

activeComponentL :: Lens' AppState SomeBrickComponent
-- There's probably some clever way to do this but idk. It's also trivial rn.
activeComponentL = lens getter setter
  where
    getter AppState {asOverlays = o : _} = o
    getter AppState {asTabs} = LZ.cursor asTabs
    setter state@(AppState {asOverlays = _ : os}) t' = state {asOverlays = t' : os}
    setter state@(AppState {asTabs}) t' = state {asTabs = LZ.replace t' asTabs}

pushOverlay :: (AppResourceName -> SomeBrickComponent) -> AppState -> AppState
pushOverlay mk state@(AppState {asOverlays}) = state {asOverlays = mk ovlResourceName : asOverlays}
  where
    ovlResourceName = Overlay (length asOverlays)

popOverlay :: AppState -> AppState
popOverlay state@(AppState {asOverlays = _ : os}) = state {asOverlays = os}
-- SOMEDAY could also just do nothing here. Source of crashes but only due to logic bugs though.
popOverlay _ = error "No overlays"

pushTab :: (AppResourceName -> SomeBrickComponent) -> AppState -> AppState
pushTab mk state@(AppState {asTabs, asNextTabID}) = state {asTabs = asTabs', asNextTabID = asNextTabID + 1}
  where
    asTabs' = LZ.insert (mk $ Tab asNextTabID) . LZ.right $ asTabs

-- | Remove active tab and go to next or else previous.
popTab :: AppState -> AppState
popTab state@AppState {asTabs} =
  let asTabs' = LZ.delete asTabs
      asTabs''
        | LZ.emptyp asTabs' = asTabs -- can't pop the last tab.
        | LZ.endp asTabs' = LZ.left asTabs'
        | otherwise = asTabs'
   in state {asTabs = asTabs''}

-- not sure if this is quite right but maybe it's enough. If we're missing cursors, we should prob revise.
myChooseCursor :: AppState -> [CursorLocation AppResourceName] -> Maybe (CursorLocation AppResourceName)
myChooseCursor _ = listToMaybe . reverse . filter isEditLocation . filter cursorLocationVisible
  where
    isEditLocation cloc = case cursorLocationName cloc of
      Just (EditorFor _) -> True
      _ -> False

myChooseAttrMap :: AppState -> AttrMap
myChooseAttrMap state = case CList.focus $ asAttrMapRing state of
  Just (_, res) -> res
  -- Only relevant when there are no themes. This shouldn't really happen.
  Nothing -> error "No themes?!?"

app :: App AppState AppMsg AppResourceName
app =
  App
    { appDraw = myAppDraw,
      appHandleEvent = myHandleEvent,
      appStartEvent = return (),
      appAttrMap = myChooseAttrMap,
      appChooseCursor = myChooseCursor
    }

-- --------------------------
-- Utils
-- --------------------------

-- | Map a monad function over a zipper. Effects propagate from the first to the last element.
--
-- Not implementing any instances b/c (a) it's a PITA and (b) it's not super clear in which order
-- you want effects actually.
mapMLZ :: (Monad m) => (a -> m b) -> LZ.Zipper a -> m (LZ.Zipper b)
mapMLZ f z =
  let (back, front) = lzSplit z
      (mback, mfront) = (mapM f back, mapM f front)
   in lzFromFrontBack <$> mback <*> mfront

-- | (part before the cursor, part including and after the cursor)
--
-- SOMEDAY would be easier & faster if I could access the zipper internals -.-
lzSplit :: LZ.Zipper a -> ([a], [a])
lzSplit (LZ.Zip rfront back) = (reverse rfront, back)

-- | (part before cursor, element at cursor, part after cursor). Error if at end.
lzSplit3 :: (HasCallStack) => LZ.Zipper a -> ([a], a, [a])
lzSplit3 (LZ.Zip rfront (cur : back)) = (reverse rfront, cur, back)
lzSplit3 _ = error "lzSplit3: Empty list"

lzFromFrontBack :: [a] -> [a] -> LZ.Zipper a
lzFromFrontBack front back = LZ.Zip (reverse front) back

forMLZ :: (Monad m) => LZ.Zipper a -> (a -> m b) -> m (LZ.Zipper b)
forMLZ = flip mapMLZ

lzCircRight, lzCircLeft :: LZ.Zipper a -> LZ.Zipper a
lzCircRight z =
  let nxt = LZ.right z
   in if LZ.endp nxt then LZ.start z else nxt
lzCircLeft z = if LZ.beginp z then LZ.left (LZ.end z) else LZ.left z
