{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Srtd.Components.MainTree (MainTree (..), make) where

import Brick hiding (on)
import Brick.BChan (writeBChan)
import Brick.Keybindings (Binding, bind, ctrl)
import Brick.Keybindings.KeyConfig (binding)
import Brick.Widgets.List qualified as L
import Control.Monad.IO.Class (liftIO)
import Data.CircularList qualified as CList
import Data.Function (on)
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (ZonedTime, zonedTimeZone)
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as Vec
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Lens.Micro.Platform
import Srtd.AppAttr
import Srtd.Attr
import Srtd.Component
import Srtd.Components.Attr (renderMaybeStatus, renderMostUrgentDate)
import Srtd.Components.DateSelectOverlay (dateSelectOverlay)
import Srtd.Components.NewNodeOverlay (newNodeOverlay)
import Srtd.Components.TestOverlay (newTestOverlay)
import Srtd.Dates (DateOrTime)
import Srtd.Keymap
import Srtd.Log
import Srtd.Model
import Srtd.ModelServer
import Srtd.Todo
import Srtd.Util
import System.Hclip (setClipboard)
import System.Process (callProcess)
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

type MyList = L.List AppResourceName (Int, EID, Attr)

data MainTree = MainTree
  { mtRoot :: EID,
    mtFilters :: CList.CList Filter,
    mtSubtree :: Subtree,
    mtList :: MyList,
    -- | Top-level resource name for this component. We can assign anything nested below (or "above") it.
    mtResourceName :: AppResourceName,
    mtZonedTime :: ZonedTime,
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
    "Tree View"
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
              liftIO $ writeBChan (acAppChan ctx) $ PushOverlay (SomeBrickComponent . newNodeOverlay cb oldName "Edit Item")
            Nothing -> return ()
      ),
      ( kmLeaf (ctrl 't') "Open test overlay" $ \ctx -> do
          liftIO $ writeBChan (acAppChan ctx) $ PushOverlay (const $ SomeBrickComponent newTestOverlay)
      ),
      ( kmLeaf (bind 'T') "New tab" $ \ctx -> do
          state <- get
          liftIO $ writeBChan (acAppChan ctx) $ PushTab (\rname -> SomeBrickComponent $ setResourceName rname state)
      ),
      ( kmLeaf (bind 'Q') "Close tab" $ \ctx ->
          liftIO $ writeBChan (acAppChan ctx) $ PopTab
      ),
      ( kmLeaf (bind ']') "Next tab" $ \ctx ->
          liftIO $ writeBChan (acAppChan ctx) $ NextTab
      ),
      ( kmLeaf (bind '[') "Prev tab" $ \ctx ->
          liftIO $ writeBChan (acAppChan ctx) $ PrevTab
      ),
      ( kmLeaf (binding (KChar 'j') [MMeta]) "Move subtree down same level" $ moveCurRelative goNextSibling insAfter
      ),
      ( kmLeaf (binding (KChar 'k') [MMeta]) "Move subtree up same level" $ moveCurRelative goPrevSibling insBefore
      ),
      ( kmLeaf (bind '<') "Move subtree after parent" $ moveCurRelative goParent insAfter
      ),
      ( kmLeaf (bind '>') "Move subtree last child of previous" $ moveCurRelative goPrevSibling insLastChild
      ),
      -- (kmSub (bind 'm') moveSingleModeKeymap),
      (kmSub (bind 'M') moveSubtreeModeKeymap),
      (kmSub (bind 'D') deleteKeymap),
      ( kmLeaf (binding KEnter []) "Hoist" $ withCur $ \cur ctx -> do
          rname <- mtResourceName <$> get
          model <- liftIO $ getModel (acModelServer ctx)
          ztime <- use mtZonedTimeL
          filters <- mtFilters <$> get
          put $ makeWithFilters cur filters model ztime rname
      ),
      ( kmLeaf (binding KBS []) "De-hoist" $ \ctx -> do
          s <- get
          case s ^. mtSubtreeL . breadcrumbsL of
            [] -> return ()
            (parent, _) : _ -> do
              rname <- mtResourceName <$> get
              model <- liftIO $ getModel (acModelServer ctx)
              ztime <- use mtZonedTimeL
              filters <- mtFilters <$> get
              put $ makeWithFilters parent filters model ztime rname & mtListL %~ scrollListToEID (mtRoot s)
      ),
      (kmSub (bind ',') sortCurKeymap),
      (kmSub (bind ';') sortRootKeymap),
      (kmLeaf (bind 'h') "Go to parent" (const $ modify (mtGoSubtreeFromCur forestGetParentId))),
      (kmLeaf (bind 'J') "Go to next sibling" (const $ modify (mtGoSubtreeFromCur forestGetNextSiblingId))),
      (kmLeaf (bind 'K') "Go to prev sibling" (const $ modify (mtGoSubtreeFromCur forestGetPrevSiblingId))),
      (kmSub (bind 't') setStatusKeymap),
      (kmSub (bind 'o') openExternallyKeymap),
      (kmLeaf (bind '.') "Next filter" cycleNextFilter),
      (kmSub (bind 'd') editDateKeymap),
      (kmLeaf (bind 'q') "Quit" (const halt))
    ]

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
      kmLeaf (bind 's') "Someday" (setStatus $ Just Someday),
      kmLeaf (bind 'o') "Someday" (setStatus $ Just Open)
    ]

editDateKeymap :: Keymap (AppContext -> EventM n MainTree ())
editDateKeymap =
  kmMake
    "Edit Date"
    $ map mkDateEditShortcut
    $ [ (bind 'd', "Deadline", datesL . deadlineL),
        (bind 'g', "Goalline", datesL . goallineL),
        (bind 's', "Scheduled", datesL . scheduledL),
        (bind 'r', "Remind", datesL . remindL)
      ]
  where
    mkDateEditShortcut :: (Binding, Text, Lens' Attr (Maybe DateOrTime)) -> (Binding, KeymapItem (AppContext -> EventM n MainTree ()))
    mkDateEditShortcut (kb, label, l) = kmLeaf kb label $ withCurWithAttr $ \(cur, attr) ctx ->
      let tz = zonedTimeZone $ acZonedTime ctx
          cb date' ctx' = do
            modifyModelOnServer (acModelServer ctx') (modifyAttrByEID cur (l .~ date'))
            return cur
          mkDateEdit = dateSelectOverlay cb (attr ^. l) tz ("Edit " <> label)
       in liftIO $ writeBChan (acAppChan ctx) $ PushOverlay (SomeBrickComponent . mkDateEdit)

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
        ( kmLeaf (bind 'J') "Down same level" $ moveCurRelative goNextSibling insAfter
        ),
        ( kmLeaf (bind 'K') "Up same level" $ moveCurRelative goPrevSibling insBefore
        ),
        ( kmLeaf (bind 'h') "Before parent" $ moveCurRelative goParent insBefore
        ),
        ( kmLeaf (bind '<') "After parent" $ moveCurRelative goParent insAfter
        ),
        ( kmLeaf (bind 'L') "Last child of next" $ moveCurRelative goNextSibling insLastChild
        ),
        ( kmLeaf (bind 'l') "First child of next" $ moveCurRelative goNextSibling insFirstChild
        ),
        ( kmLeaf (bind '>') "Last child of previous" $ moveCurRelative goPrevSibling insLastChild
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
      ),
      ( kmLeaf (bind 'y') "Copy to clipboard" $ withCurWithAttr $ \(_eid, Attr {name}) _ctx ->
          liftIO $ setClipboard name
      )
    ]

-- SOMEDAY The following two only differ by what is used, `withRoot` or `withCur`. Could easily be unified.

sortRootKeymap :: Keymap (AppContext -> EventM n MainTree ())
sortRootKeymap =
  kmMake
    "Sort root by"
    $ (kmSub (bind 'D') $ kmMake "Deep" (mkItems sortDeepBelow))
      : mkItems sortShallowBelow
  where
    mkItems sorter =
      [kmLeaf (bind 't') "Status" $ sortRootBy sorter (compareMStatusActionability `on` (view statusL))]
    sortRootBy sorter ord = withRoot $ \root -> modifyModel (sorter ord root)

sortCurKeymap :: Keymap (AppContext -> EventM n MainTree ())
sortCurKeymap =
  kmMake
    "Sort selected by"
    $ (kmSub (bind 'D') $ kmMake "Deep" (mkItems sortDeepBelow))
      : mkItems sortShallowBelow
  where
    mkItems sorter =
      [kmLeaf (bind 't') "Status" $ sortCurBy sorter (compareMStatusActionability `on` (view statusL))]
    sortCurBy sorter ord = withCur $ \cur -> modifyModel (sorter ord cur)

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
  liftIO $ writeBChan (acAppChan ctx) $ PushOverlay (SomeBrickComponent . newNodeOverlay cb "" "New Item")

setStatus :: Maybe Status -> AppContext -> EventM n MainTree ()
setStatus status' = withCur $ \cur ->
  modifyModel (modifyAttrByEID cur (statusL .~ status'))

cycleNextFilter :: AppContext -> EventM n MainTree ()
cycleNextFilter ctx = do
  mtFiltersL %= CList.rotR
  pullNewModel ctx

make :: EID -> Model -> ZonedTime -> AppResourceName -> MainTree
make root = makeWithFilters root (CList.fromList defaultFilters)

-- | filters must not be empty.
makeWithFilters :: EID -> CList.CList Filter -> Model -> ZonedTime -> AppResourceName -> MainTree
makeWithFilters root filters model ztime rname =
  MainTree
    { mtRoot = root,
      mtFilters = filters,
      mtSubtree = subtree,
      mtList = list,
      mtResourceName = rname,
      mtZonedTime = ztime,
      mtKeymap = keymapToZipper rootKeymap
    }
  where
    subtree = filterRun (fromJust $ CList.focus filters) root model
    list = forestToBrickList (MainListFor rname) $ stForest subtree

forestToBrickList :: AppResourceName -> MForest -> MyList
-- TODO when we have multiple tabs, MainList should be replaced by something that will actually be unique (take as an argument)
forestToBrickList rname forest = L.list rname (Vec.fromList contents) 1
  where
    contents = map (\(lvl, (i, attr)) -> (lvl, i, attr)) $ forestFlattenWithLevels forest

withSelAttr :: Bool -> Widget n -> Widget n
withSelAttr True = withDefAttr selectedItemRowAttr
withSelAttr False = id

renderRow :: ZonedTime -> Bool -> (Int, a, Attr) -> Widget n
-- TODO render all the dates
renderRow ztime sel (lvl, _, Attr {name, status, dates}) =
  withSelAttr sel $
    hBox $
      -- previous version. We prob don't wanna bring this back b/c it's not flexible enough (e.g., we can't fill), and it's not very complicated anyways.
      -- alignColumns [AlignLeft, AlignLeft] [2, 80] [renderMaybeStatus sel status, renderName lvl name]
      -- Ideally we'd have a table-list hybrid but oh well. NB this is a bit hard b/c of widths and partial drawing.
      [dateW, str " ", indentW, statusW, str " ", nameW]
  where
    -- The first level doesn't take indent b/c deadlines are enough rn.
    indentW = str (concat (replicate (lvl) "    "))
    dateW = renderMostUrgentDate ztime sel dates
    statusW = renderMaybeStatus sel status
    nameW = str name

-- SOMEDAY also deadline for the root (if any)?
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
  renderComponent MainTree {mtList, mtSubtree = Subtree {rootAttr, breadcrumbs}, mtFilters, mtZonedTime} = box
    where
      -- NOT `hAlignRightLayer` b/c that breaks background colors in light mode for some reason.
      headrow = renderRoot rootAttr breadcrumbs <+> (padLeft Max (renderFilters mtFilters))
      box = headrow <=> L.renderList (renderRow mtZonedTime) True mtList

  handleEvent ctx ev =
    updateZonedTime >> do
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
          case kmzLookup keymap key mods of
            NotFound -> handleFallback e
            LeafResult act nxt -> act ctx >> mtKeymapL .= nxt
            SubmapResult sm -> mtKeymapL .= sm
        (VtyEvent e) -> handleFallback e
        _miscEvents -> return ()
    where
      -- SOMEDAY when I want mouse support, I prob have to revise this: this only interprets
      -- keystroke events (Event, from vty), not BrickEvent. Possible I have to run *both* event
      -- handlers. Really a shame we can't know if somethings was handled. - or can we somehow??
      -- NB e.g., Editor handles the full BrickEvent type.
      handleFallback e = zoom mtListL $ L.handleListEventVi (const $ return ()) e
      updateZonedTime = mtZonedTimeL .= acZonedTime ctx

  componentKeyDesc = kmzDesc . mtKeymap

  componentTitle MainTree {mtSubtree} = T.pack $ pathStr
    where
      pathStr = intercalate " < " $ name (rootAttr mtSubtree) : [name attr | (_, attr) <- (breadcrumbs mtSubtree)]

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

-- | Helper for relative move operations.
--
-- NB we do *not* need special precautions to prevent us from moving things out of the root b/c in
-- that case, the respective anchor is just not found in the haystack. (this is a bit of a function
-- of our insert walkers being not too crazy. When they do become more complex, we may need to bring
-- this back.)
--
-- SOMEDAY ^^
--
-- SOMEDAY this can be generalized by replacing the first Label by whatever label type we ultimately use
-- here. The forest just has to be labeled (EID, a) for some a. See `moveSubtreeRelFromForest`.
moveCurRelative :: GoWalker Label -> InsertWalker Label -> AppContext -> EventM n MainTree ()
moveCurRelative go ins = withCur $ \cur ctx -> do
  forest <- use $ mtSubtreeL . stForestL
  modifyModel (moveSubtreeRelFromForest cur go ins forest) ctx

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
  let list' = resetListPosition mtList $ forestToBrickList (getName mtList) (stForest subtree)
  put s {mtSubtree = subtree, mtList = list'}
  return ()

pullNewModel :: AppContext -> EventM n MainTree ()
pullNewModel AppContext {acModelServer} = do
  s@(MainTree {mtRoot, mtList}) <- get
  let filter_ = mtFilter s
  model' <- liftIO $ getModel acModelServer
  let subtree = filterRun filter_ mtRoot model'
  let list' = resetListPosition mtList $ forestToBrickList (getName mtList) (stForest subtree)
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

-- | Toplevel app resource name, including all contained resources. This is a "cloning" routine.
setResourceName :: AppResourceName -> MainTree -> MainTree
setResourceName rname state =
  state
    { mtList = resetListPosition (mtList state) $ forestToBrickList (MainListFor rname) (stForest . mtSubtree $ state),
      mtResourceName = rname
    }
