module Main (main) where

import Brick (on)
import Brick.AttrMap qualified
import Brick.Main
import Brick.Themes
import Brick.Types (BrickEvent (..), EventM, Widget)
import Brick.Widgets.Core (str)
import Control.Concurrent.STM (TVar)
import Control.Monad (void)
import Control.Monad.State (MonadState (get, put), liftIO, modify)
import Graphics.Vty (Event (..), Key (..), black, blue, green, magenta, white, yellow)
import Log
import Model
import System.Log.Logger (Priority (DEBUG, ERROR, INFO, WARNING), logL)

data State = State
  { modelServer :: ModelServer,
    cachedVal :: Int
  }

main :: IO ()
main = do
  setupLogger
  glogL INFO "App starting"
  modelServer <- startModelServer
  model <- getModel modelServer
  void $ defaultMain app (State modelServer (val model))
  glogL INFO "App did quit normally"

myAttrMap :: Brick.AttrMap.AttrMap
myAttrMap = themeToAttrMap $ newTheme (green `on` black) []

ui :: State -> Widget n
ui s = str (show (cachedVal s))

myModifyModelState :: State -> (Model -> Model) -> IO State
myModifyModelState s f = do
  let ms = modelServer s
  modifyModelOnServer ms f
  model <- getModel ms
  return s {cachedVal = val model}

myHandleEvent :: BrickEvent () e -> EventM () State ()
myHandleEvent ev = case ev of
  (VtyEvent (EvKey (KChar 'q') [])) -> do
    liftIO (glogL INFO "quitting...")
    halt
  (VtyEvent (EvKey (KChar '+') [])) -> do
    liftIO (glogL INFO "adding 1")
    -- TODO going forward, the server should send US (and everyone else) a message on update.
    s <- get
    s' <- liftIO (myModifyModelState s (\(Model i) -> Model (i + 1)))
    put s'
  (VtyEvent (EvKey (KChar '-') [])) -> do
    s <- get
    s' <- liftIO (myModifyModelState s (\(Model i) -> Model (i - 1)))
    put s'
  _ -> return ()

app :: App State e ()
app =
  App
    { appDraw = \s -> [ui s],
      appHandleEvent = myHandleEvent,
      appStartEvent = return (),
      appAttrMap = const myAttrMap,
      appChooseCursor = neverShowCursor
    }
