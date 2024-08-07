{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import AppAttr
import AppMsg
import Attr
import Brick
import Component
import Components.MainTree qualified as MainTree
import Control.Monad (void)
import Control.Monad.State (liftIO)
import Graphics.Vty (Event (..), Key (..))
import Lens.Micro.Platform
import Log
import Model
import ModelSaver (startModelSaver)
import ModelServer

data AppState = AppState
  { asContext :: AppContext,
    -- TODO implement features to show & manage tabs. Rn  there's only one tab, lol.
    -- TODO we may wanna keep these as MainTree so we can clone. Or 'new tab' should be a shortcut of MainTree, not sure.
    asTabs :: [SomeBrickComponent]
  }

main :: IO ()
main = do
  setupLogger
  glogL INFO "App starting"
  modelServer <- startModelServer
  -- SOMEDAY I should probably do something with the returned thread ID.
  _ <- startModelSaver modelServer
  model <- getModel modelServer
  let appState =
        AppState
          { asContext = AppContext modelServer,
            asTabs = [SomeBrickComponent $ MainTree.make Inbox f_identity model]
          }
  void $ defaultMain app appState
  glogL INFO "App did quit normally"

myAppDraw :: AppState -> [Widget AppResourceName]
myAppDraw AppState {asTabs} = [renderComponent tab0]
  where
    tab0 = case asTabs of
      [t] -> t
      _ -> error "Wrong number of tabs."

myHandleEvent :: BrickEvent AppResourceName AppMsg -> EventM AppResourceName AppState ()
myHandleEvent ev = case ev of
  (VtyEvent (EvKey (KChar 'q') [])) -> do
    liftIO (glogL INFO "quitting...")
    halt
  _ -> do
    AppState {asContext} <- get
    zoom activeComponentL $ handleEvent asContext ev

activeComponentL :: Lens' AppState SomeBrickComponent
-- There's probably some clever way to do this but idk. It's also trivial rn.
activeComponentL = lens getter setter
  where
    getter AppState {asTabs} = head asTabs
    setter state@(AppState {asTabs = _ : ts}) t' = state {asTabs = t' : ts}
    setter _ _ = error "No tabs"

app :: App AppState AppMsg AppResourceName
app =
  App
    { appDraw = myAppDraw,
      appHandleEvent = myHandleEvent,
      appStartEvent = return (),
      appAttrMap = const myAttrMap,
      appChooseCursor = neverShowCursor
    }
