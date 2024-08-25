{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Components.MainTree (MainTree (..), make) where

import Alignment (hAlignRightLayer)
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
import Components.TestOverlay (TestOverlay (..), newTestOverlay)
import Control.Monad.IO.Class (liftIO)
import Data.CircularList qualified as CList
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as Vec
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Keymap
import Lens.Micro.Platform
import Log
import Model
import ModelServer
import System.Process (callCommand, callProcess)
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))
import Todo
import Util

type MyList = L.List AppResourceName (Int, EID, Attr)

data MainTree = MainTree
  { mtRoot :: EID,
    mtFilters :: CList.CList Filter,
    mtSubtree :: Subtree,
    mtList :: MyList,
    mtKeymap :: KeymapZipper (AppContext -> EventM AppResourceName MainTree ())
  }
  deriving (Show)

suffixLenses ''MainTree

mtFilter :: MainTree -> Filter
mtFilter = fromJust . CList.focus . mtFilters
-- ^ NB we know that filters will be non-empty.

-- The first one is default selected.
defaultFilters :: [Filter]
defaultFilters =
  [ f_hide_completed,
    f_identity
  ]

rootKeymap :: Keymap (AppContext -> EventM n MainTree ())
rootKeymap =
  kmMake
    "Tree"
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
          liftIO $ writeBChan (acAppChan ctx) $ PushOverlay (const $ SomeBrickComponent newTestOverlay)
      ),
      ( kmLeaf (binding (KChar 'j') [MMeta]) "Move subtree down same level" $ withCur $ \cur ->
          modifyModel (moveSubtree cur NextSibling)
      ),
      ( kmLeaf (binding (KChar 'k') [MMeta]) "Move subtree up same level" $ withCur $ \cur ->
          modifyModel (moveSubtree cur PrevSibling)
      ),
      ( kmLeaf (bind '<') "Move subtree after parent" $ withRoot $ \root -> withCur $ \cur ->
          modifyModel (moveSubtreeBelow' root cur toAfterParent)
      ),
      ( kmLeaf (bind '>') "Move subtree last child of previous" $ withRoot $ \root -> withCur $ \cur ->
          modifyModel (moveSubtreeBelow' root cur toLastChildOfPrev)
      ),
      -- TODO hierarchy-up and one hierarchy-down (taken from move-subtree keymap.)
      -- (kmSub (bind 'm') moveSingleModeKeymap),
      (kmSub (bind 'M') moveSubtreeModeKeymap),
      (kmSub (bind 'd') deleteKeymap),
      ( kmLeaf (binding KEnter []) "Hoist" $ withCur $ \cur ctx -> do
          model <- liftIO $ getModel (acModelServer ctx)
          filters <- mtFilters <$> get
          put $ makeWithFilters cur filters model
      ),
      ( kmLeaf (binding KEsc []) "De-hoist" $ \ctx -> do
          s <- get
          case s ^. mtSubtreeL . breadcrumbsL of
            [] -> return ()
            (parent, _) : _ -> do
              model <- liftIO $ getModel (acModelServer ctx)
              filters <- mtFilters <$> get
              put $ makeWithFilters parent filters model & mtListL %~ scrollListToEID (mtRoot s)
      ),
      (kmLeaf (bind 'h') "Go to parent" (const $ modify (mtGoSubtreeFromCur forestGetParentId))),
      (kmLeaf (bind 'J') "Go to next sibling" (const $ modify (mtGoSubtreeFromCur forestGetNextSiblingId))),
      (kmLeaf (bind 'K') "Go to prev sibling" (const $ modify (mtGoSubtreeFromCur forestGetPrevSiblingId))),
      (kmSub (bind 't') setStatusKeymap),
      (kmSub (bind 'o') openExternallyKeymap),
      (kmLeaf (bind '.') "Next filter" cycleNextFilter),
      (kmLeaf (bind 'q') "Quit" (const halt))
    ]

-- TODO bubble up the keymap name so it's displayed in the help overlay. Prob easiest to just duplicate the name.
deleteKeymap :: Keymap (AppContext -> EventM n MainTree ())
deleteKeymap =
  kmMake
    "Delete"
    -- TOOD some undo would be nice, lol.
    [ (kmLeaf (bind 'd') "Subtree" $ withCur $ \cur -> modifyModel (deleteSubtree cur))
    -- (kmLeaf (bind 's') "Single" $ withCur $ \cur -> modifyModel (deleteSingle cur))
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

moveSubtreeModeKeymap :: Keymap (AppContext -> EventM n MainTree ())
moveSubtreeModeKeymap =
  sticky $
    kmMake
      "Move Subtree Mode"
      -- SOMEDAY clean up repetition
      -- TODO WIP I think these moveSubtree (and moveSingle) things can take a cleanup with their destinations.
      -- Can we reduce the number of different options? E.g., ("next based on preorder relative to self", "next based on siblings relative to parent") - Prob think about indicating the *target* relative to sth.
      [ ( kmLeaf (bind 'j') "Down" $ withRoot $ \root -> withCur $ \cur ->
            modifyModel (moveSubtreeBelow' root cur toBeforeNextPreorder)
        ),
        ( kmLeaf (bind 'k') "Up" $ withRoot $ \root -> withCur $ \cur ->
            modifyModel (moveSubtreeBelow' root cur toAfterPrevPreorder)
        ),
        ( kmLeaf (bind 'J') "Down same level" $ withRoot $ \root -> withCur $ \cur ctx -> do
            liftIO $ glogL INFO "Running: Move down same level"
            modifyModel (moveSubtreeBelow' root cur toNextSibling) ctx
        ),
        ( kmLeaf (bind 'K') "Up same level" $ withRoot $ \root -> withCur $ \cur ->
            modifyModel (moveSubtreeBelow' root cur toPrevSibling)
        ),
        ( kmLeaf (bind 'h') "Before parent" $ withRoot $ \root -> withCur $ \cur ->
            modifyModel (moveSubtreeBelow' root cur toBeforeParent)
        ),
        ( kmLeaf (bind '<') "After parent" $ withRoot $ \root -> withCur $ \cur ->
            modifyModel (moveSubtreeBelow' root cur toAfterParent)
        ),
        ( kmLeaf (bind 'L') "Last child of next" $ withRoot $ \root -> withCur $ \cur ->
            modifyModel (moveSubtreeBelow' root cur toLastChildOfNext)
        ),
        ( kmLeaf (bind 'l') "First child of next" $ withRoot $ \root -> withCur $ \cur ->
            modifyModel (moveSubtreeBelow' root cur toFirstChildOfNext)
        ),
        ( kmLeaf (bind '>') "Last child of previous" $ withRoot $ \root -> withCur $ \cur ->
            modifyModel (moveSubtreeBelow' root cur toLastChildOfPrev)
        )
        -- NB 'H' is not used and that's fine IMHO. I'm not sure why but these bindings were the most intuitive.
        -- SOMEDAY hierarchy-breaking '<' (dedent)
      ]

openExternallyKeymap :: Keymap (AppContext -> EventM n MainTree ())
openExternallyKeymap =
  kmMake
    "Open externally"
    [ ( kmLeaf (bind 'l') "First link in name" $ withCurWithAttr $ \(_eid, Attr {name}) _ctx ->
          whenJust (findFirstURL name) $ \url -> liftIO (openURL url)
      )
    ]

-- SOMEDAY these should be moved to another module.
findFirstURL :: String -> Maybe String
findFirstURL s = listToMaybe $ getAllTextMatches (s =~ urlPattern :: AllTextMatches [] String)
  where
    urlPattern :: String
    urlPattern = "(\\b[a-z]+://[a-zA-Z0-9./?=&-_%]+)"

-- TODO This doens't work with Obisdian URLs. Not sure if it's a detection problem (prob not) or if open gets confused with special chars (prob yes). This command shouldn't go through a shell.
openURL :: String -> IO ()
openURL url = callProcess "open" [url]

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

cycleNextFilter :: AppContext -> EventM n MainTree ()
cycleNextFilter ctx = do
  mtFiltersL %= CList.rotR
  pullNewModel ctx

make :: EID -> Model -> MainTree
make root model = makeWithFilters root (CList.fromList defaultFilters) model

-- | filters must not be empty.
makeWithFilters :: EID -> CList.CList Filter -> Model -> MainTree
makeWithFilters root filters model = MainTree root filters subtree list (keymapToZipper rootKeymap)
  where
    subtree = filterRun (fromJust $ CList.focus filters) root model
    list = forestToBrickList $ stForest subtree

forestToBrickList :: MForest -> MyList
-- TODO when we have multiple tabs, MainList should be replaced by something that will actually be unique (take as an argument)
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

-- Currently we only render the currently selected filter.
renderFilters :: CList.CList Filter -> Widget n
renderFilters fs = maybe emptyWidget go (CList.focus fs)
  where
    go f = withDefAttr filterLabelAttr $ str (filterName f)

instance BrickComponent MainTree where
  renderComponent MainTree {mtList, mtSubtree = Subtree {rootAttr, breadcrumbs}, mtFilters} = box
    where
      -- NOT `hAlignRightLayer` b/c that breaks background colors in light mode for some reason.
      headrow = renderRoot rootAttr breadcrumbs <+> (padLeft Max (renderFilters mtFilters))
      box = headrow <=> L.renderList renderRow True mtList

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

withCurWithAttr :: ((EID, Attr) -> AppContext -> EventM n MainTree ()) -> AppContext -> EventM n MainTree ()
withCurWithAttr go ctx = do
  s <- get
  case mtCurWithAttr s of
    Just cura -> go cura ctx
    Nothing -> return ()

withRoot :: (EID -> AppContext -> EventM n MainTree ()) -> AppContext -> EventM n MainTree ()
withRoot go ctx = do
  s <- get
  let root = mtRoot s
  go root ctx

modifyModel :: (Model -> Model) -> AppContext -> EventM n MainTree ()
modifyModel f AppContext {acModelServer} = do
  s@(MainTree {mtRoot, mtList}) <- get
  let filter_ = mtFilter s
  model' <- liftIO $ do
    -- needs to be re-written when we go more async. Assumes that the model update is performed *synchronously*!
    -- SOMEDAY should we just not pull here (and thus remove everything after this) and instead rely on the ModelUpdated event?
    modifyModelOnServer acModelServer f
    getModel acModelServer
  let subtree = filterRun filter_ mtRoot model'
  let list' = resetListPosition mtList $ forestToBrickList (stForest subtree)
  put s {mtSubtree = subtree, mtList = list'}
  return ()

pullNewModel :: AppContext -> EventM n MainTree ()
pullNewModel AppContext {acModelServer} = do
  s@(MainTree {mtRoot, mtList}) <- get
  let filter_ = mtFilter s
  model' <- liftIO $ getModel acModelServer
  let subtree = filterRun filter_ mtRoot model'
  let list' = resetListPosition mtList $ forestToBrickList (stForest subtree)
  put s {mtSubtree = subtree, mtList = list'}
  return ()

scrollListToEID :: EID -> MyList -> MyList
scrollListToEID eid = L.listFindBy $ \(_, eid', _) -> eid' == eid

-- | `resetListPosition old new` tries to set the position of `new` to the current element of `old`, prioritizing the EID or, if that fails, the current position.
resetListPosition :: MyList -> MyList -> MyList
resetListPosition old new = case L.listSelectedElement old of
  Nothing -> new
  Just (ix_, (_, tgt, _)) -> case Vec.findIndex (\(_, eid, _) -> eid == tgt) (L.listElements new) of
    -- NB this is fine if `new` doesn't actually have `ix` b/c it's too short: then it goes to the end, as desired.
    Nothing -> L.listMoveTo ix_ new
    Just ix' -> L.listMoveTo ix' new

mtGoSubtreeFromCur :: (EID -> MForest -> Maybe EID) -> MainTree -> MainTree
mtGoSubtreeFromCur f mt = fromMaybe mt mres
  where
    mres = do
      -- Maybe monad
      cur <- mtCur mt
      par <- mt ^. mtSubtreeL . stForestL . to (f cur)
      return $ mt & mtListL %~ scrollListToEID par
