module Srtd.Main (main) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Keybindings (ctrl)
import Brick.Keybindings.KeyConfig (binding)
import Brick.Keybindings.Pretty (ppBinding)
import Brick.Themes (Theme, themeToAttrMap)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Table (columnBorders, renderTable, rowBorders, surroundingBorder, table)
import Control.Arrow (first, second)
import Control.Concurrent.Async qualified as Async
import Control.Monad.State (liftIO)
import Data.CircularList qualified as CList
import Data.Foldable (forM_)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Time (getZonedTime)
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Graphics.Vty qualified as Vty
import Lens.Micro.Platform
import Srtd.Alignment
import Srtd.AppTheme qualified as AppTheme
import Srtd.Attr (EID (Vault))
import Srtd.CmdlineArgs qualified as CArgs
import Srtd.Component
import Srtd.Components.MainTree qualified as MainTree
import Srtd.Components.Tabs hiding (make)
import Srtd.Components.Tabs qualified as Tabs
import Srtd.Keymap (KeyDesc (..))
import Srtd.Log
import Srtd.ModelSaver qualified as ModelSaver
import Srtd.ModelServer
import Srtd.Ticker
import System.Exit (ExitCode (..), exitFailure, exitWith)

data AppState = AppState
  { asContext :: AppContext
  , -- SOMEDAY we may wanna keep these as MainTree so we can clone. Or 'new tab' should be a shortcut of MainTree, not sure.
    -- SOMEDAY should we have a componentResourceName getter in AppComponent? Then we can get rid of storing it here.
    -- SOMEDAY maybe it makes sense to frame a tab as an overlay and use the infra from maintree??
    asTabs :: Tabs SomeAppComponent
  -- ^ We make sure that `asTabs` is never after the end and in particular that it's nonempty.
  , asHelpAlways :: Bool
  , asAttrMapRing :: CList.CList (String, AttrMap)
  , asExitCode :: ExitCode
  -- ^ Used to pass the desired shell exit code back through the event loop. Only set in combination
  -- with 'halt'.
  }

suffixLenses ''AppState

loadAllThemes :: IO [(String, Theme)]
loadAllThemes = do
  ethemes <- AppTheme.loadAllThemes themeDir
  case ethemes of
    Left err -> do
      glogL ERROR (T.unpack err)
      -- NB this is quite harsh, but nothing is initialized at this point so w/e.
      exitFailure
    Right themes ->
      return $ map (first T.unpack) themes
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
  -- SOMEDAY should we be really careful about consistency between the modelserver and the saver?
  -- As in, we have to start from the same consistent state?
  modelSaver <- ModelSaver.startModelSaver modelServer
  ModelSaver.linkModelSaver modelSaver

  -- SOMEDAY it's unfortunate that this is bounded actually, could in principle lead to deadlock.
  appChan <- newBChan 100
  subscribe modelServer $ writeBChan appChan . AppComponentMsg . ModelUpdated

  Async.link =<< startTicker 60 (writeBChan appChan $ AppComponentMsg Tick)

  ztime <- getZonedTime
  let actx = AppContext modelServer appChan ztime
  let appState =
        AppState
          { asContext = actx
          , -- Set to the default tab upon init action.
            asTabs = Tabs.make 0 "Root Tabs" [] "root_tabs"
          , asHelpAlways = False
          , asAttrMapRing = attrMapRing
          , asExitCode = ExitSuccess
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
  (s, _vty) <- customMainWithDefaultVty (Just appChan) app appState

  -- SOMEDAY would be good to have some kind of bracket expression that makes sure this is called
  -- and can't be missed. I guess I could make a `withModelSaver` function or something.
  -- Should I worry about exceptions though?
  ModelSaver.exitGracefully modelSaver

  let exitCode = asExitCode s
  case exitCode of
    ExitSuccess -> glogL INFO "App did quit normally"
    ExitFailure c -> glogL ERROR ("App did quit with error: exit code " ++ show c)
  exitWith exitCode

myAppDraw :: AppState -> [Widget AppResourceName]
myAppDraw state@(AppState {asTabs, asContext}) = [keyHelpUI] ++ mainUIs
 where
  mainUIs =
    let (w0, ovls0) =
          let ?actx = asContext
           in renderComponentWithOverlays asTabs
     in map (uncurry wrapOverlay) ovls0 ++ [w0]
  wrapOverlay title w =
    centerLayer
      . Brick.hLimitPercent 80
      . Brick.vLimitPercent 75
      . borderWithLabel (padLeftRight 1 . txt $ title)
      $ w
  renderKeyHelp keymapName pairs =
    let configTable = surroundingBorder False . rowBorders False . columnBorders False
        inner = configTable $ table [[padRight (Pad 1) (txt keydesc), txt actdesc] | (keydesc, actdesc) <- pairs]
     in -- SOMEDAY minor bug: When `keymapName` is wider than the content (the key table), it's cut off.
        -- This happens in particular with smaller sub-mode keymaps.
        -- The easiest fix is probably to set a min width for the keymap overlay (content of border; also looks better).
        -- I think helix did this, e.g., in the `"` overlay.
        alignBottomRightLayer . borderWithLabel (padLeftRight 1 $ txt keymapName) $ renderTable inner
  keyHelpUI =
    let KeyDesc keymapName isToplevel keydescs = componentKeyDesc asTabs
        -- Key desc for keys at the toplevel. We don't use a keymap for this right now.
        mainKeydescs =
          map (first ppBinding) $
            [ (ctrl 'q', "Quit")
            , (ctrl '/', "Toggle key help")
            , (binding (KFun 10) [], "Next Theme")
            ]
        fullKeydescs =
          sortBy
            (comparing fst)
            ( keydescs
                ++
                -- We only show the root keydesc at the toplevel b/c it looks better
                (if isToplevel then mainKeydescs else [])
            )
     in if not isToplevel || (asHelpAlways state)
          then renderKeyHelp keymapName fullKeydescs
          else emptyWidget

myHandleEvent :: BrickEvent AppResourceName AppRootMsg -> EventM AppResourceName AppState ()
myHandleEvent ev = wrappingActions $
  case ev of
    (VtyEvent (EvKey (KChar 'q') [MCtrl])) -> do
      liftIO (glogL DEBUG "quitting...")
      halt
    -- Toggle: Always show overlay. (o/w only at the top level)
    -- TODO I have no idea why Ctrl+/ is registered as Ctrl+_ but here we are.
    (VtyEvent (EvKey (KChar '_') [MCtrl])) -> do
      asHelpAlwaysL %= not
    (VtyEvent (EvKey (KFun 10) [])) -> do
      asAttrMapRingL %= CList.rotR
    (AppEvent aev) -> case aev of
      PushTab t -> callIntoTabs (pushTab t)
      NextTab -> callIntoTabs nextTab
      PrevTab -> callIntoTabs prevTab
      SwapTabNext -> callIntoTabs swapTabNext
      SwapTabPrev -> callIntoTabs swapTabPrev
      AppComponentMsg acev -> handleTabs $ handleEvent (AppEvent acev)
    -- We rebuild the events here to convert the app event type
    VtyEvent k -> handleTabs $ handleEvent (VtyEvent k)
    MouseDown rname btn mods loc -> handleTabs $ handleEvent (MouseDown rname btn mods loc)
    MouseUp rname mbtn loc -> handleTabs $ handleEvent (MouseUp rname mbtn loc)
 where
  -- NB explicit type required to bind the implicit parameter in the right place.
  wrappingActions ::
    ((?actx :: AppContext) => EventM AppResourceName AppState ()) -> EventM AppResourceName AppState ()
  wrappingActions act = do
    dbgprint
    updateCurrentTime
    AppState {asContext = actx} <- get
    -- NB we still use implicit params in this file b/c it's somewhat more convenient than moving
    -- everything to Reader or passing params around, and we don't live in ComponentEventM.
    -- At some point we may abstract the tabs logic into its own component, then this should all get
    -- much more trivial.
    let ?actx = actx
     in act
  dbgprint = liftIO $ glogL DEBUG $ "Received: " ++ show ev
  -- SOMEDAY could also just react to the tick event.
  updateCurrentTime = liftIO getZonedTime >>= assign (asContextL . acZonedTimeL)

callIntoTabs ::
  ComponentEventM (Tabs SomeAppComponent) a -> EventM AppResourceName AppState a
callIntoTabs act = do
  actx <- gets asContext
  tabs <- use asTabsL
  (tabs', (ret, events)) <- nestEventM tabs $ runComponentEventM' actx $ act
  -- Ignore events. Nothing interesting here.
  forM_ events $ const (return ())
  asTabsL .= tabs'
  return ret

handleTabs ::
  (?actx :: AppContext) =>
  ComponentEventM' (Tabs SomeAppComponent) -> EventM AppResourceName AppState ()
handleTabs act = do
  res <- callIntoTabs act
  case res of
    Continue -> return ()
    -- When Tabs returns Confirmed, this means the last tab closed with Confirmed. This is quit, so halt.
    Confirmed () -> halt
    -- When Tabs returns Canceled, this means the last tab errored out. Push the default tab instead
    Canceled -> pushDefaultTab

pushDefaultTab :: EventM AppResourceName AppState ()
pushDefaultTab = do
  actx <- use asContextL
  model <- liftIO $ getModel (acModelServer actx)
  let emk = MainTree.make' actx Vault model
  case emk of
    Left _err -> do
      liftIO $ glogL ERROR "'Vault' node not found. Something is horribly wrong. Exiting."
      asExitCodeL .= ExitFailure 1
      halt
    Right mk -> callIntoTabs (pushTab $ SomeAppComponent . mk)

-- not sure if this is quite right but maybe it's enough. If we're missing cursors, we should prob revise.
myChooseCursor ::
  AppState -> [CursorLocation AppResourceName] -> Maybe (CursorLocation AppResourceName)
myChooseCursor _ = listToMaybe . reverse . filter isEditLocation . filter cursorLocationVisible
 where
  isEditLocation cloc = case cursorLocationName cloc of
    -- See EditorProactive.
    Just (AppResourceName names@(_ : _))
      | NamedAppResource "brick editor" _ <- last names -> True
    _ -> False

myChooseAttrMap :: AppState -> AttrMap
myChooseAttrMap state = case CList.focus $ asAttrMapRing state of
  Just (_, res) -> res
  -- Only relevant when there are no themes. This shouldn't really happen.
  Nothing -> error "No themes?!?"

myAppStartEvent :: EventM AppResourceName AppState ()
myAppStartEvent = do
  -- Set up mouse support. For some reason we have to do this here, not in 'main'.
  -- See Brick's `MouseDemo.hs`
  vty <- getVtyHandle
  liftIO $ Vty.setMode (Vty.outputIface vty) Vty.Mouse True
  -- Push default tab
  pushDefaultTab

app :: App AppState AppRootMsg AppResourceName
app =
  App
    { appDraw = myAppDraw
    , appHandleEvent = myHandleEvent
    , appStartEvent = myAppStartEvent
    , appAttrMap = myChooseAttrMap
    , appChooseCursor = myChooseCursor
    }
