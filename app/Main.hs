module Main(main) where

import Brick.Main
import Control.Monad (void)
import Brick.Themes
import Graphics.Vty (white, blue, green, yellow, black, magenta, Event(..), Key(..))
import Brick (on)
import qualified Brick.AttrMap
import Brick.Widgets.Core (str)
import Brick.Types ( Widget, BrickEvent(..) )
import Brick.Types (EventM)
import Control.Monad.State (modify, liftIO, MonadState (put, get))

import System.Log.Logger (logL, Priority(ERROR, WARNING, INFO, DEBUG))
import Log
import Control.Concurrent.STM (TVar)

import Model

data State = State {
  modelServer :: ModelServer,
  cachedVal :: Int
}

main :: IO()
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
  return s { cachedVal = val model }
 
myHandleEvent :: BrickEvent () e -> EventM () State ()
myHandleEvent ev = case ev of
  (VtyEvent (EvKey (KChar 'q') [])) -> do
    liftIO (glogL INFO "quitting...")
    halt

  (VtyEvent (EvKey (KChar '+') [])) -> do
    liftIO (glogL INFO "adding 1")
    -- TODO going forward, the server should send US (and everyone else) a message on update.
    s <- get
    s' <- liftIO (myModifyModelState s (\(Model i) -> Model (i+1)))
    put s'

  (VtyEvent (EvKey (KChar '-') [])) -> do
    s <- get
    s' <- liftIO (myModifyModelState s (\(Model i) -> Model (i-1)))
    put s'
  _ -> return ()

app :: App State e ()
app = App
  { appDraw = \s -> [ui s]
  , appHandleEvent = myHandleEvent
  , appStartEvent = return ()
  , appAttrMap = const myAttrMap
  , appChooseCursor = neverShowCursor
}

