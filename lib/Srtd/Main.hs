module Srtd.Main (main) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Keybindings (ToBinding (bind), ctrl)
import Brick.Keybindings.KeyConfig (binding)
import Brick.Keybindings.Pretty (ppBinding)
import Brick.Themes (Theme, themeToAttrMap)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Table (columnBorders, renderTable, rowBorders, surroundingBorder, table)
import Control.Arrow (first, second)
import Control.Concurrent.Async qualified as Async
import Control.Monad (when)
import Control.Monad.State (liftIO)
import Data.CircularList qualified as CList
import Data.List (intersperse, sortBy)
import Data.List.Zipper qualified as LZ
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Time (getZonedTime)
import Data.Void
import GHC.Stack (HasCallStack)
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Graphics.Vty qualified as Vty
import Lens.Micro.Platform
import Srtd.Alignment
import Srtd.AppAttr qualified as AppAttr
import Srtd.AppTheme qualified as AppTheme
import Srtd.Attr (EID (Vault))
import Srtd.CmdlineArgs qualified as CArgs
import Srtd.Component
import Srtd.Components.MainTree qualified as MainTree
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
    asTabs :: LZ.Zipper (AppResourceName, SomeAppComponent)
  -- ^ We make sure that `asTabs` is never after the end and in particular that it's nonempty.
  -- We store the resource name of the tab title (!) so we can find it again for mouse clicks.
  -- SOMEDAY I don't think we even have to store the resource name as state. We can just use the
  -- index in the list.
  , asNextTabID :: Int
  -- ^ "Unique ID" for the next tab being opened, for resource names.
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
            asTabs = LZ.empty
          , asNextTabID = 1
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
  tab0 = snd $ LZ.cursor asTabs
  mainUIs =
    let (w0, ovls0) =
          let ?actx = asContext
           in renderComponentWithOverlays tab0
     in map (uncurry wrapOverlay) ovls0 ++ [renderTabBar asTabs <=> w0]
  renderTabTitle :: (AppComponent c, Ord n) => Bool -> n -> c -> Widget n
  renderTabTitle sel rname c = clickable rname . withAttr theAttrName . padLeftRight 1 . hLimit 25 $ txt (componentTitle c)
   where
    theAttrName =
      (if sel then AppAttr.tab_bar <> attrName "selected" else AppAttr.tab_bar)
        <> attrName "tab_title"
  renderTabBar pairs =
    withDefAttr AppAttr.tab_bar $
      let (front, cur, back) = lzSplit3 pairs
       in hBox $
            intersperse (txt "|") $
              map (uncurry (renderTabTitle False)) front
                ++ [uncurry (renderTabTitle True) cur]
                ++ map (uncurry (renderTabTitle False)) back
                ++ [padLeft Max (str " ")]
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
    let KeyDesc keymapName isToplevel keydescs = componentKeyDesc $ state ^. activeTabL
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
      PushTab t -> modify $ pushTab t
      NextTab -> asTabsL %= lzCircRight
      PrevTab -> asTabsL %= lzCircLeft
      SwapTabNext -> asTabsL %= lzSwapRightCirc
      SwapTabPrev -> asTabsL %= lzSwapLeftCirc
      AppComponentMsg acev ->
        case acev of
          -- SOMEDAY alt, just push acev through every time, but let's stay a bit safe here for now.
          ModelUpdated _ -> eachTabHandleEvent (AppEvent acev)
          Tick -> eachTabHandleEvent (AppEvent acev)
    (MouseDown rname@(SingleTabTitleFor _) Vty.BLeft [] _location) -> asTabsL %= lzFindBegin ((rname ==) . fst)
    -- some boilerplate to safely cast BrickEvent n AppRootMsg to BrickEvent n AppComponentMsg here.
    -- SOMEDAY this can certainly be done in a more clever way. Maybe a dispatch function on BrickEvent.
    VtyEvent k -> routeToCurrentTab (VtyEvent k)
    MouseDown rname btn mods loc -> routeToCurrentTab (MouseDown rname btn mods loc)
    MouseUp rname mbtn loc -> routeToCurrentTab (MouseUp rname mbtn loc)
 where
  routeToCurrentTab ev' = do
    -- NB we ignore events from child components here.
    -- SOMEDAY information could travel up to us (but not with SomeAppComponent, that one eats events)
    (res, events) <- zoom activeTabL $ runComponentEventM' ?actx $ handleEvent ev'
    -- Attests that there are no (non-bottom) events
    mapM_ absurd events
    case res of
      Continue -> return ()
      -- See the AppComponent instance of MainTree
      Confirmed () -> popTabOrQuitAction
      Canceled -> popTabAction
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

eachTabHandleEvent ::
  (?actx :: AppContext) =>
  BrickEvent AppResourceName AppComponentMsg -> EventM AppResourceName AppState ()
eachTabHandleEvent ev = do
  tabs <- use asTabsL
  mtabs' <- lzForM tabs $ \(rname, tabCmp) -> do
    -- NB we ignore events from child components here.
    -- SOMEDAY information could travel up to us (but not with SomeAppComponent, that one eats events)
    (tabCmp', (res, events)) <- nestEventM tabCmp $ runComponentEventM' ?actx $ handleEvent ev
    -- Attests that there are no (non-bottom) events
    mapM_ absurd events
    return $ case res of
      Continue -> Just (rname, tabCmp')
      _ -> Nothing
  let tabs' = lzCatMaybesLeftNonEmpty $ mtabs'
  asTabsL .= tabs'
  fixEmptyTabs
 where

-- | Use this (internally) after a tab was deleted to make sure we always have a tab.
fixEmptyTabs :: EventM AppResourceName AppState ()
fixEmptyTabs = do
  tabs <- use asTabsL
  when (LZ.emptyp tabs) setDefaultTab

pattern SingleTabTitleFor :: AppResourceName -> AppResourceName
pattern SingleTabTitleFor rname = AppResourceName [TabTitleFor rname]

-- | Only valid when there are no tabs. Then sets the default tabs (or exits if something is *really* wrong)
setDefaultTab :: EventM AppResourceName AppState ()
setDefaultTab = do
  actx <- use asContextL
  model <- liftIO $ getModel (acModelServer actx)
  let ?actx = actx
  rname <- getFreshTabRName
  let emt = MainTree.make ?actx Vault model rname
  case emt of
    Left _err -> do
      liftIO $ glogL ERROR "'Vault' node not found. Something is horribly wrong. Exiting."
      asExitCodeL .= ExitFailure 1
      halt
    Right mt -> do
      asTabsL .= (LZ.fromList [(SingleTabTitleFor rname, SomeAppComponent mt)])

activeTabL :: Lens' AppState SomeAppComponent
-- There's probably some clever way to do this but idk. It's also trivial rn.
activeTabL = lens getter setter
 where
  getter AppState {asTabs} = snd $ LZ.cursor asTabs
  setter state@(AppState {asTabs}) t' = state {asTabs = lzModify (second $ const t') asTabs}

pushTab :: (AppResourceName -> SomeAppComponent) -> AppState -> AppState
pushTab mk state@(AppState {asTabs, asNextTabID}) = state {asTabs = asTabs', asNextTabID = asNextTabID + 1}
 where
  rname = AppResourceName [NamedAppResource "tab" asNextTabID]
  asTabs' = LZ.insert (SingleTabTitleFor rname, mk rname) . LZ.right $ asTabs

-- | You prob wanna use 'pushTab' instead.
getFreshTabRName :: EventM AppResourceName AppState AppResourceName
getFreshTabRName = do
  tabID <- use asNextTabIDL
  asNextTabIDL += 1
  return $ AppResourceName [NamedAppResource "tab" tabID]

-- | Remove active tab and go to previous or else next. You must call 'fixEmptyTabs' afterwards!
popTab :: AppState -> AppState
popTab = asTabsL %~ lzDeleteLeft

popTabAction :: EventM AppResourceName AppState ()
popTabAction = modify popTab >> fixEmptyTabs

-- | Like `popTabAction` but halt when the last tab was closed.
popTabOrQuitAction :: EventM AppResourceName AppState ()
popTabOrQuitAction = do
  modify popTab
  isNoTabs <- LZ.emptyp <$> use asTabsL
  when isNoTabs halt

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

  setDefaultTab

app :: App AppState AppRootMsg AppResourceName
app =
  App
    { appDraw = myAppDraw
    , appHandleEvent = myHandleEvent
    , appStartEvent = myAppStartEvent
    , appAttrMap = myChooseAttrMap
    , appChooseCursor = myChooseCursor
    }

-- --------------------------
-- Utils
-- --------------------------

-- | Map a monad function over a zipper. Effects propagate from the first to the last element.
lzMapM :: (Monad m) => (a -> m b) -> LZ.Zipper a -> m (LZ.Zipper b)
lzMapM f z =
  let (back, front) = lzSplit z
      (mback, mfront) = (mapM f back, mapM f front)
   in lzFromFrontBack <$> mback <*> mfront

-- | Like 'catMaybes' but for a zipper. If the current tab is nothing and possible, we move to the
-- left. We only return an invalid (post-end) zipper if the result is empty.
--
-- SOMEDAY test this. Prob want a separate module for these helpers.
lzCatMaybesLeftNonEmpty :: LZ.Zipper (Maybe a) -> LZ.Zipper a
lzCatMaybesLeftNonEmpty = \case
  LZ.Zip rfront (Just x : back) -> LZ.Zip (catMaybes rfront) (x : catMaybes back)
  LZ.Zip rfront (Nothing : back) -> case (catMaybes rfront, catMaybes back) of
    (x : rfront', back') -> LZ.Zip rfront' (x : back')
    ([], back') -> LZ.Zip [] back'
  -- Post-end. This won't be hit in our caller; we do something reasonable (namely, produce another
  -- post-end zipper)
  LZ.Zip rfront [] -> LZ.Zip (catMaybes rfront) []

-- | Like 'LZ.delete' but move left after deletion, if possible. We only return an invalid zipper if
-- the result is empty. Only valid of nonempty zippers.
lzDeleteLeft :: LZ.Zipper a -> LZ.Zipper a
lzDeleteLeft = \case
  LZ.Zip (x : xs) (_ : ys) -> LZ.Zip xs (x : ys)
  LZ.Zip [] (_ : ys) -> LZ.Zip [] ys
  _ -> error "lzDeleteLeft: Invalid zipper"

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

lzForM :: (Monad m) => LZ.Zipper a -> (a -> m b) -> m (LZ.Zipper b)
lzForM = flip lzMapM

lzCircRight, lzCircLeft :: LZ.Zipper a -> LZ.Zipper a
lzCircRight z =
  let nxt = LZ.right z
   in if LZ.endp nxt then LZ.start z else nxt
lzCircLeft z = if LZ.beginp z then LZ.left (LZ.end z) else LZ.left z

lzSwapRightCirc, lzSwapLeftCirc :: LZ.Zipper a -> LZ.Zipper a
-- I'm feeling the zipper's API isn't all that useful.
lzSwapRightCirc (LZ.Zip rfront (cur : nxt : back)) = LZ.Zip (nxt : rfront) (cur : back)
lzSwapRightCirc (LZ.Zip rfront [cur]) = LZ.Zip [] (cur : reverse rfront)
lzSwapRightCirc z = z
lzSwapLeftCirc (LZ.Zip (prv : rfront) (cur : back)) = LZ.Zip rfront (cur : prv : back)
lzSwapLeftCirc (LZ.Zip [] (cur : back)) = LZ.Zip (reverse back) [cur]
lzSwapLeftCirc z = z

-- | Modify the current element by a function. Error if zipper is at its end.
lzModify :: (a -> a) -> LZ.Zipper a -> LZ.Zipper a
lzModify f z = LZ.replace (f $ LZ.cursor z) z

-- | Find the first position in the *list* where the predicate is true, or return the original
-- zipper unchanged if none.
lzFindBegin :: (a -> Bool) -> LZ.Zipper a -> LZ.Zipper a
lzFindBegin p z =
  let res = lzFind p (LZ.start z)
   in if LZ.endp res then z else res
 where
  -- Find the first following position where the predicate is true, or return the end position
  -- if none.
  lzFind p' z'
    | LZ.endp z' = z'
    | p' (LZ.cursor z') = z'
    | otherwise = lzFind p' (LZ.right z')
