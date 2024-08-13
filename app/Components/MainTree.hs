{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Components.MainTree (MainTree (..), make) where

import AppAttr
import Attr
import Brick
import Brick.BChan (writeBChan)
import Brick.Keybindings (bind)
import Brick.Keybindings.KeyConfig (binding)
import Brick.Widgets.List qualified as L
import Brick.Widgets.Table (ColumnAlignment (..), alignColumns)
import Component
import Components.Attr (renderMaybeStatus)
import Components.NewNodeOverlay (newNodeOverlay)
import Components.TestOverlay (TestOverlay (..))
import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as Vec
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Keymap
import Lens.Micro.Platform
import Log
import Model
import ModelServer
import Todo

type MyList = L.List AppResourceName (Int, EID, Attr)

data MainTree = MainTree
  { mtRoot :: EID,
    mtFilter :: Filter,
    mtSubtree :: Subtree,
    mtList :: MyList,
    mtKeymap :: KeymapZipper (AppContext -> EventM AppResourceName MainTree ())
  }
  deriving (Show)

suffixLenses ''MainTree

rootKeymap :: Keymap (AppContext -> EventM n MainTree ())
rootKeymap =
  kmMake
    [ ( kmLeaf (bind 'n') "New as next sibling" $ \ctx -> do
          state <- get
          let tgtLoc = mtCur state & maybe (LastChild (mtRoot state)) After
          let cb = \name (AppContext {acModelServer = acModelServer'}) -> do
                let attr = attrMinimal name
                uuid <- nextRandom
                modifyModelOnServer acModelServer' (insertNewNormalWithNewId uuid attr tgtLoc)
                return $ EIDNormal uuid
          liftIO $ writeBChan (acAppChan ctx) $ PushOverlay (SomeBrickComponent . newNodeOverlay cb)
      ),
      ( kmLeaf (bind 's') "New as first child" $ \ctx -> do
          state <- get
          case mtCur state of
            Just cur -> do
              let tgtLoc = LastChild cur
              let cb = \name (AppContext {acModelServer = acModelServer'}) -> do
                    let attr = attrMinimal name
                    uuid <- nextRandom
                    modifyModelOnServer acModelServer' (insertNewNormalWithNewId uuid attr tgtLoc)
                    return $ EIDNormal uuid
              liftIO $ writeBChan (acAppChan ctx) $ PushOverlay (SomeBrickComponent . newNodeOverlay cb)
            Nothing -> return ()
      ),
      ( kmLeaf (bind 'T') "Open test overlay" $ \ctx -> do
          liftIO $ writeBChan (acAppChan ctx) $ PushOverlay (const $ SomeBrickComponent TestOverlay)
      ),
      ( kmSub (bind 'd') "Delete" deleteKeymap
      ),
      ( kmLeaf (binding KEnter []) "Hoist" $ \ctx -> do
          s <- get
          case mtCur s of
            Just cur -> do
              model <- liftIO $ getModel (acModelServer ctx)
              MainTree {mtFilter} <- get
              put $ make cur mtFilter model
            Nothing -> return ()
      ),
      ( kmLeaf (binding KEsc []) "De-hoist" $ \ctx -> do
          s <- get
          case s ^. mtSubtreeL . breadcrumbsL of
            [] -> return ()
            parent : _ -> do
              model <- liftIO $ getModel (acModelServer ctx)
              MainTree {mtFilter} <- get
              put $ make parent mtFilter model & mtListL %~ scrollListToEID (mtRoot s)
      ),
      (kmSub (bind 't') "Status" setStatusKeymap),
      (kmLeaf (bind 'q') "Quit" (const halt))
    ]

-- TODO bubble up the keymap name so it's displayed in the help overlay. Prob easiest to just duplicate the name.
deleteKeymap :: Keymap (AppContext -> EventM n MainTree ())
deleteKeymap =
  kmMake
    -- TOOD some undo would be nice, lol.
    [ ( kmLeaf (bind 'd') "Subtree" $ \ctx -> do
          state <- get
          case mtCur state of
            Just cur -> liftIO (myModifyModelState ctx state $ deleteSubtree cur) >>= put
            Nothing -> return ()
      )
    ]

setStatusKeymap :: Keymap (AppContext -> EventM n MainTree ())
setStatusKeymap =
  kmMake
    [ kmLeaf (bind ' ') "None" (setStatus Nothing),
      kmLeaf (bind 'n') "Next" (setStatus $ Just Next),
      kmLeaf (bind 'w') "Waiting" (setStatus $ Just Waiting),
      kmLeaf (bind 'p') "Project" (setStatus $ Just Project)
    ]

setStatus :: Maybe Status -> AppContext -> EventM n MainTree ()
setStatus status' ctx = do
  state <- get
  case mtCur state of
    Just cur -> liftIO (myModifyModelState ctx state $ modifyAttrByEID cur (statusL .~ status')) >>= put
    Nothing -> return ()

make :: EID -> Filter -> Model -> MainTree
make root filter_ model = MainTree root filter_ subtree list (keymapToZipper rootKeymap)
  where
    subtree = runFilter filter_ root model
    list = forestToBrickList $ stForest subtree

forestToBrickList :: MForest -> MyList
forestToBrickList forest = L.list MainList (Vec.fromList contents) 1
  where
    contents = map (\(lvl, (i, attr)) -> (lvl, i, attr)) $ forestFlattenWithLevels forest

withSelAttr :: Bool -> Widget n -> Widget n
withSelAttr True = withDefAttr selectedItemRowAttr
withSelAttr False = id

renderRow :: Bool -> (Int, a, Attr) -> Widget n
renderRow sel (lvl, _, Attr {name, status}) =
  withSelAttr sel $
    hBox $
      -- previous version. We prob don't wanna bring this back b/c it's not flexible enough (e.g., we can't fill), and it's not very complicated anyways.
      -- alignColumns [AlignLeft, AlignLeft] [2, 80] [renderMaybeStatus sel status, renderName lvl name]
      -- Ideally we'd have a table-list hybrid but oh well. NB this is a bit hard b/c of widths and partial drawing.
      [indentW, statusW, str " ", nameW]
  where
    indentW = str (concat (replicate (lvl + 1) "  "))
    statusW = renderMaybeStatus sel status
    nameW = str name

instance BrickComponent MainTree where
  renderComponent MainTree {mtList, mtSubtree = Subtree {root, rootAttr}} = box
    where
      box = renderRow False rootRow <=> L.renderList renderRow True mtList
      rootRow = (-1, root, rootAttr)

  handleEvent ctx ev = do
    isTopLevel <- use (mtKeymapL . to kmzIsToplevel)
    case ev of
      (AppEvent (ModelUpdated _)) -> pullNewModel ctx
      (AppEvent (PopOverlay (OREID eid))) -> do
        -- We do not distinguish between *who* returned or *why* rn. That's a bit of a hole but not needed right now.
        -- NB we really trust in synchronicity here b/c we don't reload the model. That's fine now but could be an issue later.
        mtListL %= scrollListToEID eid
      -- Code for keymap. NB this is a bit nasty, having some abstraction here would be good if we need it again.
      -- We also gotta be a bit careful not to overlap these in principle.
      (VtyEvent (EvKey KEsc [])) | not isTopLevel -> mtKeymapL %= kmzReset
      (VtyEvent (EvKey KBS [])) | not isTopLevel -> mtKeymapL %= kmzUp
      (VtyEvent e@(EvKey key mods)) -> do
        keymap <- use mtKeymapL
        -- TODO case esc handler for when we're in a submap: then only reset the keymap
        -- TODO also case backspace for this: then go up.
        case stepKeymap keymap key mods of
          NotFound -> handleFallback e
          LeafResult act -> act ctx >> mtKeymapL %= kmzReset
          SubmapResult sm -> mtKeymapL .= sm
      (VtyEvent e) -> handleFallback e
      _ -> return ()
    where
      handleFallback e = zoom mtListL $ L.handleListEventVi (const $ return ()) e

  componentKeyDesc = kmzDesc . mtKeymap

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
