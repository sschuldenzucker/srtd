{-# LANGUAGE TemplateHaskell #-}

module Components.MainTree (MainTree (..), make) where

import AppAttr
import Attr
import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List qualified as L
import Component
import Components.NewNodeOverlay (newNodeOverlay)
import Components.TestOverlay (TestOverlay (..))
import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as Vec
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Lens.Micro.Platform
import Log
import Model
import ModelServer

type MyList = L.List AppResourceName (Int, EID, Attr)

data MainTree = MainTree
  { mtRoot :: EID,
    mtFilter :: Filter,
    mtSubtree :: Subtree,
    mtList :: MyList
    -- mlKeymap :: Keymap
  }
  deriving (Show)

makeLensesFor [("mtList", "mtListL"), ("mtSubtree", "mtSubtreeL")] ''MainTree

make :: EID -> Filter -> Model -> MainTree
make root filter_ model = MainTree root filter_ subtree list
  where
    subtree = runFilter filter_ root model
    list = forestToBrickList $ stForest subtree

forestToBrickList :: MForest -> MyList
forestToBrickList forest = L.list MainList (Vec.fromList contents) 1
  where
    contents = map (\(lvl, (i, attr)) -> (lvl, i, attr)) $ forestFlattenWithLevels forest

instance BrickComponent MainTree where
  renderComponent MainTree {mtList, mtSubtree = Subtree {root, rootAttr}} = box
    where
      box = renderRow False rootRow <=> L.renderList renderRow True mtList
      rootRow = (-1, root, rootAttr)
      renderRow :: Bool -> (Int, EID, Attr) -> Widget AppResourceName
      renderRow sel (lvl, _, Attr {name}) = withSelAttr sel $ str (concat (replicate (lvl + 1) "  ") ++ name)
      withSelAttr True = withDefAttr selectedItemRowAttr
      withSelAttr False = id

  handleEvent ctx@(AppContext {acModelServer, acAppChan}) ev = case ev of
    (VtyEvent (EvKey (KChar 'n') [])) -> do
      state <- get
      let tgtLoc = mtCur state & maybe (LastChild (mtRoot state)) After
      let cb = \name (AppContext {acModelServer = acModelServer'}) -> do
            let attr = Attr {name = name}
            uuid <- nextRandom
            modifyModelOnServer acModelServer' (insertNewNormalWithNewId uuid attr tgtLoc)
            return $ EIDNormal uuid
      liftIO $ writeBChan acAppChan $ PushOverlay (SomeBrickComponent . newNodeOverlay cb)
    (VtyEvent (EvKey (KChar 's') [])) -> do
      state <- get
      case mtCur state of
        Just cur -> do
          let tgtLoc = LastChild cur
          let cb = \name (AppContext {acModelServer = acModelServer'}) -> do
                let attr = Attr {name = name}
                uuid <- nextRandom
                modifyModelOnServer acModelServer' (insertNewNormalWithNewId uuid attr tgtLoc)
                return $ EIDNormal uuid
          liftIO $ writeBChan acAppChan $ PushOverlay (SomeBrickComponent . newNodeOverlay cb)
        Nothing -> return ()
    (AppEvent (ModelUpdated _)) -> pullNewModel ctx
    (AppEvent (PopOverlay (OREID eid))) -> do
      -- We do not distinguish between *who* returned or *why* rn. That's a bit of a hole but not needed right now.
      -- NB we really trust in synchronicity here b/c we don't reload the model. That's fine now but could be an issue later.
      mtListL %= scrollListToEID eid
    (VtyEvent (EvKey KEnter [])) -> do
      s <- get
      case mtCur s of
        Just cur -> do
          model <- liftIO $ getModel acModelServer
          MainTree {mtFilter} <- get
          put $ make cur mtFilter model
        Nothing -> return ()
    (VtyEvent (EvKey KEsc [])) -> do
      s <- get
      case s ^. mtSubtreeL . breadcrumbsL of
        [] -> return ()
        parent : _ -> do
          model <- liftIO $ getModel acModelServer
          MainTree {mtFilter} <- get
          put $ make parent mtFilter model & mtListL %~ scrollListToEID (mtRoot s)
    -- TODO this is really not very nice. Prob should disappear.
    (VtyEvent (EvKey (KChar 'd') [])) -> do
      state <- get
      case mtCur state of
        Just cur -> liftIO (myModifyModelState ctx state $ deleteSubtree cur) >>= put
        Nothing -> return ()
    (VtyEvent (EvKey (KChar 'N') [])) -> do
      liftIO (glogL INFO "creating new node")
      uuid <- liftIO nextRandom
      liftIO $ glogL INFO ("new UUID: " ++ show uuid)
      state <- get
      let attr = Attr {name = "foobar"}
      let tgtLoc = mtCur state & maybe (LastChild (mtRoot state)) After
      liftIO $ glogL INFO ("State PRE " ++ show state)
      state' <- liftIO $ myModifyModelState ctx state (insertNewNormalWithNewId uuid attr tgtLoc)
      liftIO $ glogL INFO ("State POST " ++ show state')
      put state'
    (VtyEvent (EvKey (KChar 'S') [])) -> do
      liftIO (glogL INFO "creating new subnode")
      uuid <- liftIO nextRandom
      state <- get
      let attr = Attr {name = "foobar"}
      case mtCur state of
        Just cur -> (liftIO $ myModifyModelState ctx state (insertNewNormalWithNewId uuid attr (LastChild cur))) >>= put
        Nothing -> return ()
    (VtyEvent (EvKey (KChar 'T') [])) -> do
      liftIO $ writeBChan acAppChan $ PushOverlay (const $ SomeBrickComponent TestOverlay)
    (VtyEvent e) -> do
      zoom mtListL $ L.handleListEventVi (const $ return ()) e
    _ -> return ()

  -- TODO WIP use a keymap.
  componentKeyDesc _ = (True, [])

mtCur :: MainTree -> Maybe EID
mtCur (MainTree {mtList}) = L.listSelectedElement mtList & fmap (\(_, (_, i, _)) -> i)

myModifyModelState :: AppContext -> MainTree -> (Model -> Model) -> IO MainTree
myModifyModelState AppContext {acModelServer} s@(MainTree {mtRoot, mtFilter, mtList}) f = do
  modifyModelOnServer acModelServer f
  model <- getModel acModelServer
  let subtree = runFilter mtFilter mtRoot model
  -- TODO what happens when an element is deleted and this is not possible?
  -- TODO actually we should move to the selected *eid*, not position.
  let resetPosition = L.listSelected mtList & maybe id L.listMoveTo
  let asList' = resetPosition $ forestToBrickList (stForest subtree)
  return s {mtSubtree = subtree, mtList = asList'}

pullNewModel :: AppContext -> EventM n MainTree ()
pullNewModel (AppContext {acModelServer}) = do
  model <- liftIO $ getModel acModelServer
  s@(MainTree {mtRoot, mtFilter, mtList}) <- get
  let subtree = runFilter mtFilter mtRoot model
  -- TODO what happens when an element is deleted and this is not possible?
  let resetPosition = L.listSelected mtList & maybe id L.listMoveTo
  let asList' = resetPosition $ forestToBrickList (stForest subtree)
  put s {mtSubtree = subtree, mtList = asList'}

scrollListToEID :: EID -> MyList -> MyList
scrollListToEID eid = L.listFindBy $ \(_, eid', _) -> eid' == eid
