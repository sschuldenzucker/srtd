{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Components.MainTree (MainTree (..), make) where

import AppAttr
import Attr
import Brick
import Brick.BChan (writeBChan)
import Brick.Keybindings (bind)
import Brick.Keybindings.KeyConfig (binding)
import Brick.Widgets.List qualified as L
import Component
import Components.Attr (renderMaybeStatus)
import Components.NewNodeOverlay (newNodeOverlay)
import Components.TestOverlay (TestOverlay (..))
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as Vec
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Keymap
import Lens.Micro.Platform
import Log
import Model
import ModelServer

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
    "Tree"
    -- TODO unify these keys into one. s/S should also behave like n/N when there is no current node.
    [ kmLeaf (bind 'n') "New as next sibling" $ pushInsertNewItemRelToCur After,
      kmLeaf (bind 'N') "New as prev sibling" $ pushInsertNewItemRelToCur Before,
      kmLeaf (bind 'S') "New as first child" $ pushInsertNewItemRelToCur FirstChild,
      kmLeaf (bind 's') "New as last child" $ pushInsertNewItemRelToCur LastChild,
      ( kmLeaf (bind 'e') "Edit name" $ \ctx -> do
          state <- get
          case mtCurWithAttr state of
            Just (cur, curAttr) -> do
              let oldName = name curAttr
              let cb name' (AppContext {acModelServer = acModelServer'}) = do
                    modifyModelOnServer acModelServer' (modifyAttrByEID cur (nameL .~ name'))
                    -- NB we wouldn't need to return anything here; it's just to make the interface happy (and also the most correct approximation for behavior)
                    return cur
              liftIO $ writeBChan (acAppChan ctx) $ PushOverlay (SomeBrickComponent . newNodeOverlay cb oldName)
            Nothing -> return ()
      ),
      ( kmLeaf (bind 'T') "Open test overlay" $ \ctx -> do
          liftIO $ writeBChan (acAppChan ctx) $ PushOverlay (const $ SomeBrickComponent TestOverlay)
      ),
      ( kmLeaf (binding (KChar 'j') [MMeta]) "Move down same level" $ withCur $ \cur ->
          modifyModel (moveSubtree cur NextSibling)
      ),
      ( kmLeaf (binding (KChar 'k') [MMeta]) "Move up same level" $ withCur $ \cur ->
          modifyModel (moveSubtree cur PrevSibling)
      ),
      ( kmSub (bind 'd') deleteKeymap
      ),
      ( kmLeaf (binding KEnter []) "Hoist" $ withCur $ \cur ctx -> do
          model <- liftIO $ getModel (acModelServer ctx)
          MainTree {mtFilter} <- get
          put $ make cur mtFilter model
      ),
      ( kmLeaf (binding KEsc []) "De-hoist" $ \ctx -> do
          s <- get
          case s ^. mtSubtreeL . breadcrumbsL of
            [] -> return ()
            (parent, _) : _ -> do
              model <- liftIO $ getModel (acModelServer ctx)
              MainTree {mtFilter} <- get
              put $ make parent mtFilter model & mtListL %~ scrollListToEID (mtRoot s)
      ),
      (kmLeaf (bind 'h') "Go to parent" (const $ modify (mtGoSubtreeFromCur forestGetParentId))),
      (kmLeaf (bind 'J') "Go to next sibling" (const $ modify (mtGoSubtreeFromCur forestGetNextSiblingId))),
      (kmLeaf (bind 'K') "Go to prev sibling" (const $ modify (mtGoSubtreeFromCur forestGetPrevSiblingId))),
      (kmSub (bind 't') setStatusKeymap),
      (kmLeaf (bind 'q') "Quit" (const halt))
    ]

-- TODO bubble up the keymap name so it's displayed in the help overlay. Prob easiest to just duplicate the name.
deleteKeymap :: Keymap (AppContext -> EventM n MainTree ())
deleteKeymap =
  kmMake
    "Delete"
    -- TOOD some undo would be nice, lol.
    [ ( kmLeaf (bind 'd') "Subtree" $ withCur $ \cur -> modifyModel (deleteSubtree cur)
      )
    ]

setStatusKeymap :: Keymap (AppContext -> EventM n MainTree ())
setStatusKeymap =
  kmMake
    "Set Status"
    [ kmLeaf (bind ' ') "None" (setStatus Nothing),
      kmLeaf (bind 'n') "Next" (setStatus $ Just Next),
      kmLeaf (bind 'w') "Waiting" (setStatus $ Just Waiting),
      kmLeaf (bind 'p') "Project" (setStatus $ Just Project),
      kmLeaf (bind 'l') "Later" (setStatus $ Just Later),
      kmLeaf (bind 'i') "WIP" (setStatus $ Just WIP),
      kmLeaf (binding KEnter []) "Done" (setStatus $ Just Done),
      kmLeaf (bind 's') "Someday" (setStatus $ Just Someday)
    ]

pushInsertNewItemRelToCur :: (EID -> InsertLoc EID) -> AppContext -> EventM n MainTree ()
pushInsertNewItemRelToCur toLoc ctx = do
  state <- get
  let tgtLoc = mtCur state & maybe (LastChild (mtRoot state)) toLoc
  let cb name (AppContext {acModelServer = acModelServer'}) = do
        let attr = attrMinimal name
        uuid <- nextRandom
        modifyModelOnServer acModelServer' (insertNewNormalWithNewId uuid attr tgtLoc)
        return $ EIDNormal uuid
  liftIO $ writeBChan (acAppChan ctx) $ PushOverlay (SomeBrickComponent . newNodeOverlay cb "")

setStatus :: Maybe Status -> AppContext -> EventM n MainTree ()
setStatus status' = withCur $ \cur ->
  modifyModel (modifyAttrByEID cur (statusL .~ status'))

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
    indentW = str (concat (replicate (lvl + 1) "    "))
    statusW = renderMaybeStatus sel status
    nameW = str name

renderRoot :: Attr -> [(a, Attr)] -> Widget n
renderRoot rootAttr breadcrumbs =
  hBox
    [statusW, str " ", str pathStr]
  where
    statusW = renderMaybeStatus False (status rootAttr)
    pathStr = intercalate " < " $ name rootAttr : [name attr | (_, attr) <- breadcrumbs]

instance BrickComponent MainTree where
  renderComponent MainTree {mtList, mtSubtree = Subtree {rootAttr, breadcrumbs}} = box
    where
      box = renderRoot rootAttr breadcrumbs <=> L.renderList renderRow True mtList

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
      (VtyEvent (EvKey KEsc [])) | not isTopLevel -> mtKeymapL %= kmzResetRoot
      (VtyEvent (EvKey KBS [])) | not isTopLevel -> mtKeymapL %= kmzUp
      (VtyEvent e@(EvKey key mods)) -> do
        keymap <- use mtKeymapL
        -- TODO case esc handler for when we're in a submap: then only reset the keymap
        -- TODO also case backspace for this: then go up.
        case kmzLookup keymap key mods of
          NotFound -> handleFallback e
          LeafResult act nxt -> act ctx >> mtKeymapL .= nxt
          SubmapResult sm -> mtKeymapL .= sm
      (VtyEvent e) -> handleFallback e
      _miscEvents -> return ()
    where
      handleFallback e = zoom mtListL $ L.handleListEventVi (const $ return ()) e

  componentKeyDesc = kmzDesc . mtKeymap

mtCur :: MainTree -> Maybe EID
mtCur (MainTree {mtList}) = L.listSelectedElement mtList & fmap (\(_, (_, i, _)) -> i)

mtCurWithAttr :: MainTree -> Maybe (EID, Attr)
mtCurWithAttr (MainTree {mtList}) = L.listSelectedElement mtList & fmap (\(_, (_, i, attr)) -> (i, attr))

withCur :: (EID -> AppContext -> EventM n MainTree ()) -> AppContext -> EventM n MainTree ()
withCur go ctx = do
  s <- get
  case mtCur s of
    Just cur -> go cur ctx
    Nothing -> return ()

modifyModel :: (Model -> Model) -> AppContext -> EventM n MainTree ()
modifyModel f AppContext {acModelServer} = do
  s@(MainTree {mtRoot, mtFilter}) <- get
  let resetListPosition = maybe id scrollListToEID $ mtCur s
  model' <- liftIO $ do
    modifyModelOnServer acModelServer f
    getModel acModelServer
  let subtree = runFilter mtFilter mtRoot model'
  let list' = resetListPosition $ forestToBrickList (stForest subtree)
  put s {mtSubtree = subtree, mtList = list'}
  return ()

pullNewModel :: AppContext -> EventM n MainTree ()
-- Not the cleanest thing in the world, may need to be re-written when we go more async. (but then
-- so does `modifyModel`)
pullNewModel = modifyModel id

scrollListToEID :: EID -> MyList -> MyList
scrollListToEID eid = L.listFindBy $ \(_, eid', _) -> eid' == eid

-- TODO the following ask for an abstraction. Anyone else?

mtGoSubtreeFromCur :: (EID -> MForest -> Maybe EID) -> MainTree -> MainTree
mtGoSubtreeFromCur f mt = fromMaybe mt mres
  where
    mres = do
      -- Maybe monad
      cur <- mtCur mt
      par <- mt ^. mtSubtreeL . stForestL . to (f cur)
      return $ mt & mtListL %~ scrollListToEID par
