{-# LANGUAGE TemplateHaskell #-}

module Components.MainTree (MainTree (..), make) where

import AppAttr
import AppMsg
import Attr
import Brick
import Brick.Widgets.List qualified as L
import Component
import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as Vec
import Graphics.Vty (Event (..), Key (..))
import Lens.Micro.Platform
import Log
import Model
import ModelServer

data MainTree = MainTree
  { mtRoot :: EID,
    mtFilter :: Filter,
    mtSubtree :: Subtree,
    mtList :: L.List AppResourceName (Int, EID, Attr)
  }
  deriving (Show)

makeLensesFor [("mtList", "mtListL")] ''MainTree

make :: EID -> Filter -> Model -> MainTree
make root filter_ model = MainTree root filter_ subtree list
  where
    subtree = runFilter filter_ root model
    list = forestToBrickList $ stForest subtree

forestToBrickList :: MForest -> L.List AppResourceName (Int, EID, Attr)
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

  handleEvent ctx ev = case ev of
    (VtyEvent (EvKey (KChar 'n') [])) -> do
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
    (VtyEvent (EvKey (KChar 's') [])) -> do
      liftIO (glogL INFO "creating new subnode")
      uuid <- liftIO nextRandom
      state <- get
      let attr = Attr {name = "foobar"}
      case mtCur state of
        Just cur -> (liftIO $ myModifyModelState ctx state (insertNewNormalWithNewId uuid attr (LastChild cur))) >>= put
        Nothing -> return ()
    (VtyEvent e) -> do
      zoom mtListL $ L.handleListEventVi (const $ return ()) e
    _ -> return ()

mtCur :: MainTree -> Maybe EID
mtCur (MainTree {mtList}) = L.listSelectedElement mtList & fmap (\(_, (_, i, _)) -> i)

myModifyModelState :: AppContext -> MainTree -> (Model -> Model) -> IO MainTree
myModifyModelState AppContext {acModelServer} s@(MainTree {mtRoot, mtFilter, mtList}) f = do
  modifyModelOnServer acModelServer f
  model <- getModel acModelServer
  let subtree = runFilter mtFilter mtRoot model
  -- TODO what happens when an element is deleted and this is not possible?
  let resetPosition = L.listSelected mtList & maybe id L.listMoveTo
  let asList' = resetPosition $ forestToBrickList (stForest subtree)
  return s {mtSubtree = subtree, mtList = asList'}
