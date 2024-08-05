{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Attr
import Brick
import Brick.Themes
import Brick.Widgets.List qualified as L
import Control.Monad (void)
import Control.Monad.State (liftIO)
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as Vec
import Graphics.Vty (Event (..), Key (..), black, green)
import Lens.Micro.Platform
import Log
import Model
import ModelSaver (startModelSaver)
import ModelServer

data AppResourceName = MainList deriving (Eq, Ord, Show)

data AppState = AppState
  { _asModelServer :: ModelServer,
    -- NB this is a bit redundant (same as subtree root) but the subtree can change so I'm keeping it.
    _asRoot :: EID,
    _asFilter :: Filter,
    _asSubtree :: Subtree,
    -- SOMEDAY we may wanna pre-render a bit more here. E.g. put the fully rendered stuff and the EID in there. Or something.
    _asList :: L.List AppResourceName (Int, EID, Attr)
  }
  deriving (Show)

makeLensesFor [("_asList", "asList")] ''AppState

main :: IO ()
main = do
  setupLogger
  glogL INFO "App starting"
  modelServer <- startModelServer
  -- SOMEDAY I should probably do something with the returned thread ID.
  _ <- startModelSaver modelServer
  model <- getModel modelServer
  let subtree = runFilter f_identity Inbox model
  void $
    defaultMain
      app
      ( AppState
          { _asModelServer = modelServer,
            _asRoot = Inbox,
            _asFilter = f_identity,
            _asSubtree = subtree,
            _asList = forestToBrickList (stForest subtree)
          }
      )
  glogL INFO "App did quit normally"

selectedItemRowAttr :: AttrName
selectedItemRowAttr = attrName "selectedItemRow"

myAttrMap :: AttrMap
myAttrMap = themeToAttrMap $ newTheme (green `on` black) [(selectedItemRowAttr, black `on` green)]

ui :: AppState -> Widget AppResourceName
-- ui _s = str "ok I guess TODO"
-- TODO render root. Prob just another, unselected element.
ui AppState {_asList, _asSubtree = Subtree {root, rootAttr}} = box
  where
    box = renderRow False rootRow <=> L.renderList renderRow True _asList
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
myModifyModelState s@(AppState {_asModelServer, _asRoot, _asFilter, _asList}) f = do
  modifyModelOnServer _asModelServer f
  model <- getModel _asModelServer
  let subtree = runFilter _asFilter _asRoot model
  -- TODO what happens when an element is deleted and this is not possible?
  let resetPosition = L.listSelected _asList & maybe id L.listMoveTo
  let asList' = resetPosition $ forestToBrickList (stForest subtree)
  return s {_asSubtree = subtree, _asList = asList'}

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
    let tgtLoc = asCur state & maybe (LastChild (_asRoot state)) After
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
  (VtyEvent e) -> do
    zoom asList $ L.handleListEventVi (const $ return ()) e
  _ -> return ()

asCur :: AppState -> Maybe EID
asCur (AppState {_asList}) = L.listSelectedElement _asList & fmap (\(_, (_, i, _)) -> i)

app :: App AppState e AppResourceName
app =
  App
    { appDraw = \s -> [ui s],
      appHandleEvent = myHandleEvent,
      appStartEvent = return (),
      appAttrMap = const myAttrMap,
      appChooseCursor = neverShowCursor
    }
