{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Alignment
import AppAttr
import Attr
import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Table (columnBorders, renderTable, rowBorders, surroundingBorder, table)
import Component
import Components.MainTree qualified as MainTree
import Control.Monad (void)
import Control.Monad.State (liftIO)
import Data.Traversable (forM)
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Lens.Micro.Platform
import Log
import Model
import ModelSaver (startModelSaver)
import ModelServer
import Todo

data AppState = AppState
  { asContext :: AppContext,
    -- TODO implement features to show & manage tabs. Rn  there's only one tab, lol.
    -- TODO we may wanna keep these as MainTree so we can clone. Or 'new tab' should be a shortcut of MainTree, not sure.
    asTabs :: [SomeBrickComponent],
    asOverlays :: [SomeBrickComponent],
    asHelpAlways :: Bool
  }

suffixLenses ''AppState

main :: IO ()
main = do
  setupLogger
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
            asTabs = [SomeBrickComponent $ MainTree.make Inbox f_identity model],
            asOverlays = [],
            asHelpAlways = True -- Good default rn.
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
    -- TODO fix: we can't type q in a task (lol). I prob want a toplevel check on overlays (or loop through the chan)
    -- (or just make this Ctrl+q for the global version at least)
    (VtyEvent (EvKey (KChar 'q') [])) -> do
      liftIO (glogL INFO "quitting...")
      halt
    -- Toggle: Always show overlay. (o/w only at the top level)
    -- TODO I have no idea why Ctrl+/ is registered as Ctrl+_ but here we are.
    (VtyEvent (EvKey (KChar '_') [MCtrl])) -> do
      asHelpAlwaysL %= not
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

app :: App AppState AppMsg AppResourceName
app =
  App
    { appDraw = myAppDraw,
      appHandleEvent = myHandleEvent,
      appStartEvent = return (),
      appAttrMap = const myAttrMap,
      appChooseCursor = neverShowCursor
    }
