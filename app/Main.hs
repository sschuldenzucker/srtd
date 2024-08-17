{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Alignment
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
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (forM)
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Lens.Micro.Platform
import Log
import Model
import ModelSaver (startModelSaver)
import ModelServer
import System.Directory (listDirectory)
import System.Exit (exitFailure)
import System.FilePath
import Toml qualified

data AppState = AppState
  { asContext :: AppContext,
    -- TODO implement features to show & manage tabs. Rn  there's only one tab, lol.
    -- TODO we may wanna keep these as MainTree so we can clone. Or 'new tab' should be a shortcut of MainTree, not sure.
    asTabs :: [SomeBrickComponent],
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

  print attrMapRing

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
            asTabs = [SomeBrickComponent $ MainTree.make Vault f_identity model],
            asOverlays = [],
            asHelpAlways = True, -- Good default rn.
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
myAppDraw state@(AppState {asTabs, asOverlays}) = [keyHelpUI] ++ map renderOverlay asOverlays ++ [renderComponent tab0]
  where
    tab0 = case asTabs of
      [t] -> t
      _ -> error "Wrong number of tabs."
    -- renderOverlay o = hLimitPercent 70 $ renderComponent o
    renderOverlay =
      centerLayer
        . Brick.hLimitPercent 80
        . Brick.vLimitPercent 75
        -- TODO give overlays names so user knows what to do. (should prob be a Component method)
        -- . borderWithLabel (txt "Overlay")
        . border
        . renderComponent
    renderKeyHelp pairs =
      let configTable = surroundingBorder False . rowBorders False . columnBorders False
          inner = configTable $ table [[padRight (Pad 1) (txt keydesc), txt actdesc] | (keydesc, actdesc) <- pairs]
       in alignBottomRightLayer . borderWithLabel (padLeftRight 1 $ str "Help") $ renderTable inner
    keyHelpUI =
      let (isToplevel, keydescs) = componentKeyDesc $ state ^. activeComponentL
       in if not isToplevel || (asHelpAlways state) then renderKeyHelp keydescs else emptyWidget

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
  asTabs' <- forM asTabs $ \t -> nestEventM' t act
  put $ state {asOverlays = asOverlays', asTabs = asTabs'}
  return ()

activeComponentL :: Lens' AppState SomeBrickComponent
-- There's probably some clever way to do this but idk. It's also trivial rn.
activeComponentL = lens getter setter
  where
    getter AppState {asOverlays = o : _} = o
    getter AppState {asTabs = t : _} = t
    getter _ = error "No tabs"
    setter state@(AppState {asOverlays = _ : os}) t' = state {asOverlays = t' : os}
    setter state@(AppState {asTabs = _ : ts}) t' = state {asTabs = t' : ts}
    setter _ _ = error "No tabs"

pushOverlay :: (AppResourceName -> SomeBrickComponent) -> AppState -> AppState
pushOverlay mk state@(AppState {asOverlays}) = state {asOverlays = mk ovlResourceName : asOverlays}
  where
    ovlResourceName = Overlay (length asOverlays)

popOverlay :: AppState -> AppState
popOverlay state@(AppState {asOverlays = _ : os}) = state {asOverlays = os}
-- SOMEDAY could also just do nothing here. Source of crashes but only due to logic bugs though.
popOverlay _ = error "No overlays"

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
