module Main (main) where

import Brick (on, vBox, (<=>))
import Brick.AttrMap qualified
import Brick.Main
import Brick.Themes
import Brick.Types (BrickEvent (..), EventM, Widget)
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core (str)
import Brick.Widgets.List qualified as L
import Control.Concurrent.STM (TVar)
import Control.Monad (void)
import Control.Monad.State (MonadState (get, put), liftIO, modify)
import Data.Tree (flatten)
import Data.Vector qualified as Vec
import Graphics.Vty (Event (..), Key (..), black, blue, green, magenta, white, yellow)
import Log
import Model
import System.Log.Logger (Priority (DEBUG, ERROR, INFO, WARNING), logL)
import Todo

data AppState = AppState
  { modelServer :: ModelServer,
    asRoot :: EID,
    asFilter :: Filter,
    asSubtree :: Subtree,
    -- SOMEDAY we may wanna pre-render a bit more here. E.g. put the fully rendered stuff and the EID in there. Or something.
    asList :: L.List AppResourceName (Int, EID, Attr)
  }
  deriving (Show)

data AppResourceName = MainList deriving (Eq, Ord, Show)

main :: IO ()
main = do
  setupLogger
  glogL INFO "App starting"
  modelServer <- startModelServer
  model <- getModel modelServer
  let subtree = runFilter f_identity Inbox model
  void $
    defaultMain
      app
      ( AppState
          { modelServer = modelServer,
            asRoot = Inbox,
            asFilter = f_identity,
            asSubtree = subtree,
            asList = forestToBrickList (stForest subtree)
          }
      )
  glogL INFO "App did quit normally"

myAttrMap :: Brick.AttrMap.AttrMap
myAttrMap = themeToAttrMap $ newTheme (green `on` black) []

ui :: AppState -> Widget AppResourceName
-- ui _s = str "ok I guess TODO"
-- TODO render root. Prob just another, unselected element.
ui AppState {asList, asSubtree = Subtree {root, rootAttr}} = box
  where
    box = renderRow False rootRow <=> L.renderList renderRow True asList
    rootRow = (-1, root, rootAttr)
    renderRow :: Bool -> (Int, EID, Attr) -> Widget AppResourceName
    -- todo use selected and set attrs
    renderRow sel (lvl, _, Attr {name}) = str (concat (replicate (lvl + 1) "  ") ++ name)

forestToBrickList :: MForest -> L.List AppResourceName (Int, EID, Attr)
forestToBrickList forest = L.list MainList (Vec.fromList contents) 1
  where
    contents = map (\(lvl, (i, attr)) -> (lvl, i, attr)) $ forestFlattenWithLevels forest

myModifyModelState :: AppState -> (Model -> Model) -> IO AppState
myModifyModelState s@(AppState {modelServer, asRoot, asFilter}) f = do
  modifyModelOnServer modelServer f
  model <- getModel modelServer
  let subtree = runFilter asFilter asRoot model
  return s {asSubtree = subtree, asList = forestToBrickList (stForest subtree)}

myHandleEvent :: BrickEvent AppResourceName e -> EventM AppResourceName AppState ()
myHandleEvent ev = case ev of
  (VtyEvent (EvKey (KChar 'q') [])) -> do
    liftIO (glogL INFO "quitting...")
    halt
  _ -> return ()

app :: App AppState e AppResourceName
app =
  App
    { appDraw = \s -> [ui s],
      appHandleEvent = myHandleEvent,
      appStartEvent = return (),
      appAttrMap = const myAttrMap,
      appChooseCursor = neverShowCursor
    }
