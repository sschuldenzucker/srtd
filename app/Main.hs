module Main (main) where

import Brick (nestEventM, on, vBox, withDefAttr, (<=>))
import Brick.AttrMap qualified
import Brick.AttrMap qualified as A
import Brick.Main
import Brick.Themes
import Brick.Types (BrickEvent (..), EventM, Widget)
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core (str)
import Brick.Widgets.List qualified as L
import Control.Concurrent.STM (TVar)
import Control.Monad (void)
import Control.Monad.State (MonadState (get, put), liftIO, modify)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Tree (flatten)
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as Vec
import Graphics.Vty (Event (..), Key (..), black, blue, green, magenta, white, yellow)
import Log
import Model
import System.Log.Logger (Priority (DEBUG, ERROR, INFO, WARNING), logL)
import Todo

data AppState = AppState
  { modelServer :: ModelServer,
    -- TODO is this redundant? Same as subtree root?
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

selectedItemRowAttr :: A.AttrName
selectedItemRowAttr = A.attrName "selectedItemRow"

myAttrMap :: Brick.AttrMap.AttrMap
myAttrMap = themeToAttrMap $ newTheme (green `on` black) [(selectedItemRowAttr, black `on` green)]

ui :: AppState -> Widget AppResourceName
-- ui _s = str "ok I guess TODO"
-- TODO render root. Prob just another, unselected element.
ui AppState {asList, asSubtree = Subtree {root, rootAttr}} = box
  where
    box = renderRow False rootRow <=> L.renderList renderRow True asList
    rootRow = (-1, root, rootAttr)
    renderRow :: Bool -> (Int, EID, Attr) -> Widget AppResourceName
    renderRow sel (lvl, _, Attr {name}) = withSelAttr sel $ str (concat (replicate (lvl + 1) "  ") ++ name)
    withSelAttr True = withDefAttr selectedItemRowAttr
    withSelAttr False = id

forestToBrickList :: MForest -> L.List AppResourceName (Int, EID, Attr)
forestToBrickList forest = L.list MainList (Vec.fromList contents) 1
  where
    contents = map (\(lvl, (i, attr)) -> (lvl, i, attr)) $ forestFlattenWithLevels forest

myModifyModelState :: AppState -> (Model -> Model) -> IO AppState
myModifyModelState s@(AppState {modelServer, asRoot, asFilter, asList}) f = do
  modifyModelOnServer modelServer f
  model <- getModel modelServer
  let subtree = runFilter asFilter asRoot model
  -- TODO what happens when an element is deleted and this is not possible?
  let resetPosition = L.listSelected asList & maybe id L.listMoveTo
  let asList' = resetPosition $ forestToBrickList (stForest subtree)
  return s {asSubtree = subtree, asList = asList'}

myHandleEvent :: BrickEvent AppResourceName e -> EventM AppResourceName AppState ()
myHandleEvent ev = case ev of
  (VtyEvent (EvKey (KChar 'q') [])) -> do
    liftIO (glogL INFO "quitting...")
    halt
  (VtyEvent (EvKey (KChar 'n') [])) -> do
    liftIO (glogL INFO "creating new node")
    uuid <- liftIO nextRandom
    liftIO $ glogL INFO ("new UUID: " ++ show uuid)
    state <- get
    let attr = Attr {name = "foobar"}
    let tgtLoc = asCur state & maybe (LastChild (asRoot state)) After
    liftIO $ glogL INFO ("State PRE " ++ show state)
    -- TODO this somehow doesn't work when we have selected something that's not a first child.
    state' <- liftIO $ myModifyModelState state (insertNewNormalWithNewId uuid attr tgtLoc)
    liftIO $ glogL INFO ("State POST " ++ show state')
    put state'
  (VtyEvent (EvKey (KChar 's') [])) -> do
    liftIO (glogL INFO "creating new subnode")
    uuid <- liftIO nextRandom
    state <- get
    let attr = Attr {name = "foobar"}
    case asCur state of
      Just cur -> (liftIO $ myModifyModelState state (insertNewNormalWithNewId uuid attr (LastChild cur))) >>= put
      Nothing -> return ()

  -- SOMEDAY use lenses. Is this actually useful tho? (this one would be `zoom`)
  (VtyEvent e) -> do
    state <- get
    let listState = asList state
    -- Why does the Vi variant have a fallback but the other one doesn't?! (not using the fallback here)
    (listState', ()) <- nestEventM listState (L.handleListEventVi (const $ return ()) e)
    put (state {asList = listState'})
  _ -> return ()

asCur :: AppState -> Maybe EID
asCur (AppState {asList}) = L.listSelectedElement asList & fmap (\(_, (_, i, _)) -> i)

app :: App AppState e AppResourceName
app =
  App
    { appDraw = \s -> [ui s],
      appHandleEvent = myHandleEvent,
      appStartEvent = return (),
      appAttrMap = const myAttrMap,
      appChooseCursor = neverShowCursor
    }
