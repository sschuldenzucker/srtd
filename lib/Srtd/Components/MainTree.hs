{-# HLINT ignore "Redundant bracket" #-}
-- For overlay management
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Srtd.Components.MainTree (MainTree (..), make, make') where

import Brick hiding (on)
import Brick.BChan (writeBChan)
-- hiding to avoid a name clash

import Brick.Keybindings (Binding, bind, ctrl, meta)
import Brick.Keybindings.KeyConfig (binding)
import Brick.Widgets.List qualified as L
import Brick.Widgets.Table
import Control.Monad (replicateM, when)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Functor (void)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, zonedTimeToUTC, zonedTimeZone)
import Data.Tree (Tree (Node))
import Data.UUID.V4 (nextRandom)
import Data.Void (absurd)
import Graphics.Vty (Key (..), Modifier (..))
import Lens.Micro.Platform
import Srtd.AppAttr qualified as AppAttr
import Srtd.Attr hiding (Canceled)
import Srtd.Attr qualified (Status (Canceled))
import Srtd.BrickHelpers
import Srtd.Component
import Srtd.Components.Attr (
  renderStatus,
 )
import Srtd.Components.CompilingTextEntry
import Srtd.Components.DateSelectOverlay (dateSelectOverlay)
import Srtd.Components.NewNodeOverlay (newNodeOverlay)
import Srtd.Components.QuickFilter qualified as QF
import Srtd.Components.TreeStatusBar (
  BreadcrumbDirection (..),
  renderBreadcrumbs,
  renderLabelShort,
  renderStatusActionabilityCounts,
  statusBarW,
 )
import Srtd.Components.TreeView qualified as TV
import Srtd.Config qualified as Config
import Srtd.Data.IdTree
import Srtd.Data.TreeZipper
import Srtd.Dates (DateOrTime (..), prettyAbsolute)
import Srtd.Keymap
import Srtd.Log
import Srtd.Model
import Srtd.ModelServer
import Srtd.MonadBrick (MonadBrick (..))
import Srtd.ProactiveBandana (Cell', cValue)
import Srtd.ProactiveBandana qualified as C
import Srtd.Util
import System.Hclip (setClipboard)
import Text.Regex.TDFA.Common (Regex)
import Text.Wrap (WrapSettings (..), defaultWrapSettings)

-- * Overlay infra (must come early b/c Template Haskell)

-- SOMEDAY can we make this a more general thing? Then also review how specific types have to be.
-- (probably not very specific.)

-- | A class for overlays living in parent state `t`. Two callbacks define interruptible computations. Note that this doesn't
-- store state of the computation *itself*.
--
-- SOMEDAY are we over-generalizing here? Compare this to explicitly naming the overlays we might have open.
--
-- Might also be related to that question of representation of having a data structure vs the list
-- of filled fields in some priority order (e.g., for dates rendering).
--
-- SOMEDAY we can process events here as well by adding them to the functions, or just having a separate events processing function! This usually runs *before* the other callbacks. We can access `Event s` here!
--
-- SOMEDAY move to Component, feels general.
data Overlay t = forall s. (AppComponent s) => Overlay
  { olState :: s
  , olOnConfirm :: Return s -> ComponentEventM t (AppEventReturn ())
  , olOnCanceled :: ComponentEventM t (AppEventReturn ())
  , olOnEvent :: Event s -> ComponentEventM t ()
  }

-- | `olOnEvent` handler that ignores events
ignoreEvent :: (Monad m) => a -> m ()
ignoreEvent _ = return ()

-- * Main Data Structure

-- | Main tree component with the selected filter and hierarchy-collapse state.
data MainTree = MainTree
  { mtSelectedFilter :: Cell' Filter (ComponentEventMOrNotFound MainTree ())
  -- ^ Currently selected view filter. Updating it refreshes the child 'TV.TreeView'.
  , mtTreeView :: TV.TreeView
  , mtResourceName :: AppResourceName
  -- ^ Top-level resource name for this component. We can assign anything nested below (or "above") it.
  , mtKeymap :: KeymapZipper (ComponentEventM' MainTree)
  , mtShowDetails :: Bool
  -- ^ Whether or not to show the details view. This is not implemented as a full overlay
  -- component for simplicity.
  , mtOverlay :: Maybe (Overlay MainTree)
  -- ^ Active overlay, if any. If this is 'Just', events are forwarded to the overlay.
  --
  -- SOMEDAY do I want to make a wrapper for "things that have overlays" that *consistenly* handles everything?
  , mtHideHierarchyFilter :: Cell' HideHierarchyFilter (ComponentEventMOrNotFound MainTree ())
  }

suffixLenses ''MainTree

-- | Default selected filter for new main trees.
defaultFilter :: Filter
defaultFilter = f_notDone

-- | Key bindings for directly selecting filters from the view menu.
filterBindings :: [(Char, Filter)]
filterBindings =
  [ ('n', f_notDone)
  , ('u', f_deepByDates)
  , ('m', f_NotDelayedByLastModified)
  , ('F', f_flatByDates)
  , ('N', f_nextFlatByDates)
  , ('W', f_waitingFlatByDates)
  , ('s', f_stalledProjects)
  , ('p', f_projectOverview)
  , ('a', f_all)
  ]

-- ** Convenience Accessors

mtSubtree :: MainTree -> Subtree
mtSubtree = cValue . TV.tvSubtree . mtTreeView

mtRoot :: MainTree -> EID
mtRoot = root . mtSubtree

mtSearchRxL :: Lens' MainTree (Maybe (CompiledWithSource Regex))
mtSearchRxL = mtTreeViewL . TV.tvSearchRxL

mtDoFollowItemL :: Lens' MainTree Bool
mtDoFollowItemL = mtTreeViewL . TV.tvDoFollowItemL

mtCur :: MainTree -> Maybe EID
mtCur = TV.tvCur . mtTreeView

mtCurWithAttr :: MainTree -> Maybe LocalIdLabel
mtCurWithAttr = TV.tvCurWithAttr . mtTreeView

withCurOrElse ::
  (MonadState MainTree m) =>
  m a -> (EID -> m a) -> m a
withCurOrElse dflt go = maybe dflt go =<< gets mtCur

withCur ::
  (MonadState MainTree m) =>
  (EID -> m ()) -> m ()
withCur = withCurOrElse (return ())

withCurWithAttr ::
  (MonadState MainTree m) =>
  (LocalIdLabel -> m ()) -> m ()
withCurWithAttr = withCurWithAttrOrElse (return ())

withCurWithAttrOrElse ::
  (MonadState MainTree m) =>
  m a -> (LocalIdLabel -> m a) -> m a
withCurWithAttrOrElse dflt go = maybe dflt go =<< gets mtCurWithAttr

withRoot ::
  (MonadState MainTree m) =>
  (EID -> m ()) -> m ()
withRoot go = go =<< gets mtRoot

-- ** Internal calls

callIntoTreeView :: ComponentEventM TV.TreeView a -> ComponentEventM MainTree a
callIntoTreeView = callIntoComponentEventM mtTreeViewL $ safeConst (return ())

-- * API

-- | Create new 'MainTree'
make :: AppContext -> EID -> Model -> AppResourceName -> Either IdNotFoundError MainTree
make actx root = makeWithFilter actx root defaultFilter emptyHideHierarchyFilter True

-- | Create a reusable 'MainTree' constructor for a fixed root and model.
make' :: AppContext -> EID -> Model -> Either IdNotFoundError (AppResourceName -> MainTree)
make' actx root model = do
  go <- makeWithFilter' actx root defaultFilter emptyHideHierarchyFilter model
  return $ \rname -> go True rname

-- | Create a new 'MainTree' with an explicit selected filter.
makeWithFilter ::
  AppContext ->
  EID ->
  Filter ->
  HideHierarchyFilter ->
  Bool ->
  Model ->
  AppResourceName ->
  Either IdNotFoundError MainTree
makeWithFilter actx root selectedFilter hhf doFollowItem model rname = do
  tv <-
    TV.makeFromModel
      (appContext2FilterContext actx)
      root
      (chainFilters (hideHierarchyFilter hhf) selectedFilter)
      doFollowItem
      Config.scrolloff
      model
      (rname <> "treeview")
  return $
    MainTree
      { mtSelectedFilter = C.cell selectedFilter $ C.simple $ \_fi -> resetTreeViewFilter
      , mtHideHierarchyFilter = C.cell hhf $ C.simple $ \_fis -> resetTreeViewFilter
      , mtTreeView = tv
      , mtResourceName = rname
      , mtKeymap = keymapToZipper rootKeymap
      , mtShowDetails = False
      , mtOverlay = Nothing
      }

-- | A version of 'makeWithFilter' that's stricter based on what can fail.
makeWithFilter' ::
  AppContext ->
  EID ->
  Filter ->
  HideHierarchyFilter ->
  Model ->
  Either
    IdNotFoundError
    (Bool -> AppResourceName -> MainTree)
makeWithFilter' actx root selectedFilter hhf model = do
  mkTV <-
    TV.makeFromModel'
      (appContext2FilterContext actx)
      root
      (chainFilters (hideHierarchyFilter hhf) selectedFilter)
      model
  let go doFollowItem rname =
        MainTree
          { mtSelectedFilter = C.cell selectedFilter $ C.simple $ \_fi -> resetTreeViewFilter
          , mtHideHierarchyFilter = C.cell hhf $ C.simple $ \_fis -> resetTreeViewFilter
          , mtTreeView = mkTV doFollowItem Config.scrolloff rname
          , mtResourceName = rname
          , mtKeymap = keymapToZipper rootKeymap
          , mtShowDetails = False
          , mtOverlay = Nothing
          }
  return go

-- | Toplevel app resource name, including all contained resources. This is a "cloning" routine.
--
-- TODO also handle overlays. To do this, AppComponent needs a function, which is prob a good idea.
setResourceName :: AppResourceName -> MainTree -> MainTree
setResourceName rname = (mtResourceNameL .~ rname) . (mtTreeViewL %~ TV.setResourceName rname)

-- * Keymaps

rootKeymap :: Keymap (ComponentEventM' MainTree)
rootKeymap =
  kmMake
    "Tree View"
    ( [ kmLeaf_ (bind 'n') "New as next sibling" $ pushInsertNewItemRelToCur insAfter
      , kmLeaf_ (bind 'N') "New as prev sibling" $ pushInsertNewItemRelToCur insBefore
      , kmLeaf_ (bind 'S') "New as first child" $ pushInsertNewItemRelToCur insFirstChild
      , kmLeaf_ (bind 's') "New as last child" $ pushInsertNewItemRelToCur insLastChild
      , kmLeaf_ (bind 'a') "Quick add to inbox" pushQuickAddToInbox
      , ( kmLeaf_ (bind 'e') "Edit name" $ do
            state <- get
            case mtCurWithAttr state of
              Just (cur, ((curAttr, _), _)) -> do
                let oldName = name curAttr
                let cb name' = do
                      ztime <- asks acZonedTime
                      let f = setLastModified (zonedTimeToUTC ztime) . (nameL .~ name')
                      modifyModelAsync $ modifyAttrByEID cur f
                      callIntoTreeView $ TV.moveToEID cur
                      return Continue
                pushOverlay (newNodeOverlay oldName "Edit Item") cb (return Continue) absurd
              Nothing -> return ()
        )
      , kmSub (ctrl 't') debugKeymap
      , ( kmLeaf_ (bind 'T') "New tab" $ do
            state <- get
            achan <- asks acAppChan
            liftIO $
              writeBChan achan $
                PushTab (\rname -> SomeAppComponent $ setResourceName rname state)
        )
      , (kmLeaf (bind 'q') "Close tab / quit" $ return $ Confirmed ())
      , ( kmLeaf_ (bind ']') "Next tab" $ do
            achan <- asks acAppChan
            liftIO $ writeBChan achan NextTab
        )
      , ( kmLeaf_ (bind '[') "Prev tab" $ do
            achan <- asks acAppChan
            liftIO $ writeBChan achan PrevTab
        )
      , ( kmLeaf_ (bind '}') "Swap tab next" $ do
            achan <- asks acAppChan
            liftIO $ writeBChan achan SwapTabNext
        )
      , ( kmLeaf_ (bind '{') "Swap tab prev" $ do
            achan <- asks acAppChan
            liftIO $ writeBChan achan SwapTabPrev
        )
      , ( kmLeaf_ (binding (KChar 'j') [MMeta]) "Move subtree down same level" $
            moveCurRelative goNextSibling insAfter
        )
      , ( kmLeaf_ (binding (KChar 'k') [MMeta]) "Move subtree up same level" $
            moveCurRelative goPrevSibling insBefore
        )
      , (kmLeaf_ (bind '<') "Move subtree after parent" $ moveCurRelative goParent insAfter)
      , ( kmLeaf_ (bind '>') "Move subtree last child of previous" $
            moveCurRelative goPrevSibling insLastChild
        )
      , -- (kmSub (bind 'm') moveSingleModeKeymap),
        (kmSub (bind 'M') moveSubtreeModeKeymap)
      , (kmSub (bind 'y') yankKeymap)
      , (kmSub (bind 'p') pasteKeymap)
      , (kmSub (bind 'D') deleteKeymap)
      , ( kmLeaf (binding KEnter []) "Hoist" $ withCurOrElse (return Continue) $ \cur -> do
            notFoundToAER_ $ moveRootToEID cur
        )
      , ( kmLeaf (binding KBS []) "De-hoist" $ do
            mt <- get
            case breadcrumbs . mtSubtree $ mt of
              [] -> return Continue
              (par, _) : _ -> notFoundToAER_ $ do
                moveRootToEID par
                replaceExceptT callIntoTreeView $ TV.moveToEID (mtRoot mt)
        )
      , (kmSub (bind ';') sortKeymap)
      , (kmLeaf_ (bind 'h') "Go to parent" (callIntoTreeView $ TV.moveGoWalkerFromCur goParent))
      , (kmLeaf_ (bind 'J') "Go to next sibling" (callIntoTreeView $ TV.moveGoWalkerFromCur goNextSibling))
      , (kmLeaf_ (bind 'K') "Go to prev sibling" (callIntoTreeView $ TV.moveGoWalkerFromCur goPrevSibling))
      , (kmLeaf_ (bind 'H') "Go to next uncle" (callIntoTreeView $ TV.moveGoWalkerFromCur goNextAncestor))
      , (kmSub (bind 't') setStatusKeymap)
      , (kmSub (bind 'o') openExternallyKeymap)
      , (kmSub (bind 'd') editDateKeymap)
      , (kmLeaf_ (bind '`') "Toggle details overlay" (mtShowDetailsL %= not))
      , (kmSub (bind 'g') goKeymap)
      , (kmSub (bind 'f') searchKeymap)
      , -- SOMEDAY bind '/' to directly go to search. A bit of duplication b/c ESC should *not* go to
        -- the submenu in this case.
        (kmSub (bind 'v') viewKeymap)
      , ( kmLeaf
            (bind '-')
            "Un/collapse"
            ( withCurOrElse (return Continue) $ \cur ->
                notFoundToAER_ $
                  modifyHideHierarchyFilter (hhfToggle cur)
            )
        )
      , ( kmLeaf
            (bind '0')
            "Uncollapse all"
            ( notFoundToAER_ $
                modifyHideHierarchyFilter (const emptyHideHierarchyFilter)
            )
        )
      , kmSub (bind ' ') spaceKeymap
      , kmLeaf_ (bind 'j') "Move down" (callIntoTreeView $ TV.moveBy 1)
      , kmLeaf_ (bind 'k') "Move up" (callIntoTreeView $ TV.moveBy (-1))
      , kmSub (bind 'z') viewportKeymap
      ]
    )
    `kmUnion` collapseLevelKeymap

viewKeymap :: Keymap (ComponentEventM' MainTree)
viewKeymap =
  kmMake
    "Filter"
    ( map
        mkMapping
        filterBindings
        ++ [ kmLeaf_ (ctrl 'f') "Toggle follow item" (mtDoFollowItemL %= not)
           ]
    )
 where
  mkMapping (k :: Char, fi :: Filter) = kmLeaf (bind k) (T.pack $ fiName fi) $ notFoundToAER_ (selectFilter fi)

collapseLevelKeymap :: Keymap (ComponentEventM' MainTree)
collapseLevelKeymap =
  kmMake
    "Collapse level"
    (map (hide . collapseLevelKeymapItem) [1 .. 9])
    `kmAddAddlDesc` [("1-9", "Collapse to level")]
 where
  collapseLevelKeymapItem :: Int -> (Binding, KeymapItem (ComponentEventM' MainTree))
  collapseLevelKeymapItem i =
    kmLeaf
      (bind $ unsafeSingleDigitUIntToChar i)
      ("Collapse level " <> (T.show i))
      ( do
          -- NB there's some double work going on here but I think it's fine.
          normalFilter <- gets (cValue . mtSelectedFilter)
          actx <- ask
          model' <- liftIO $ getModel (acModelServer actx)
          root <- gets mtRoot
          notFoundToAER_ $ do
            subtree <- liftEither $ runFilter (appContext2FilterContext actx) normalFilter root model'
            modifyHideHierarchyFilter $ const $ subtreeLevelHHF i subtree
      )
  subtreeLevelHHF i subtree = HideHierarchyFilter (Set.fromList $ forestIdsAtLevel i (stForest subtree))

deleteKeymap :: Keymap (ComponentEventM' MainTree)
deleteKeymap =
  kmMake
    "Delete"
    -- TOOD some undo would be nice, lol.
    [ (kmLeaf_ (bind 'D') "Subtree" $ withCur $ \cur -> modifyModelAsync (deleteSubtree cur))
    , (kmLeaf_ (bind 'd') "Single & splice" $ withCur $ \cur -> modifyModelAsync (deleteSingleSplice cur))
    ]

setStatusKeymap :: Keymap (ComponentEventM' MainTree)
setStatusKeymap =
  kmMake
    "Set status"
    [ kmLeaf_ (bind ' ') "None" (setStatus None)
    , kmLeaf_ (bind 'n') "Next" (setStatus $ Next)
    , kmLeaf_ (bind 'w') "Waiting" (setStatus $ Waiting)
    , kmLeaf_ (bind 'p') "Project" (setStatus $ Project)
    , kmLeaf_ (bind 'l') "Later" (setStatus $ Later)
    , kmLeaf_ (bind 'i') "WIP" (setStatus $ WIP)
    , kmLeaf_ (binding KEnter []) "Done" (setStatus $ Done)
    , kmLeaf_ (bind 'x') "Canceled" (setStatus $ Srtd.Attr.Canceled)
    , kmLeaf_ (bind 's') "Someday" (setStatus $ Someday)
    , kmLeaf_ (bind 'o') "Open" (setStatus $ Open)
    , kmLeaf_ (bind 't') "Touch" touchLastStatusModified
    ]

editDateKeymap :: Keymap (ComponentEventM' MainTree)
editDateKeymap =
  kmMake
    "Edit date"
    $ ( map
          mkDateEditShortcut
          [ (bind 'd', "Deadline", ALens' $ datesL . deadlineL)
          , (bind 'g', "Goalline", ALens' $ datesL . goallineL)
          , (bind 's', "Scheduled", ALens' $ datesL . scheduledL)
          , (bind 'r', "Remind", ALens' $ datesL . remindL)
          ]
      )
      ++ [ kmLeaf_
             (ctrl 'd')
             "Delete all"
             ( withCur $ \cur -> do
                 ztime <- asks acZonedTime
                 let f = setLastModified (zonedTimeToUTC ztime) . (datesL .~ noDates)
                 modifyModelAsync (modifyAttrByEID cur f)
             )
         , kmLeaf_
             (binding (KChar 'd') [MCtrl, MMeta])
             "Delete all ancestors"
             ( withCur $ \cur -> do
                 utime <- asks $ zonedTimeToUTC . acZonedTime
                 let f attr
                       | isAttrDatesEmpty (dates attr) = attr
                       | otherwise = setLastModified utime . (datesL .~ noDates) $ attr
                 modifyModelAsync (modifyAncestorAttrsByEID cur f)
             )
         ]
 where
  mkDateEditShortcut (kb, label, l0) = kmLeaf_ kb label $ withCurWithAttr $ \(cur, ((attr, _), _)) ->
    let cb date' = do
          ztime <- asks acZonedTime
          let f = setLastModified (zonedTimeToUTC ztime) . (runALens' l0 .~ date')
          modifyModelAsync $ modifyAttrByEID cur f
          callIntoTreeView $ TV.moveToEID cur
          return Continue
        mkDateEdit = dateSelectOverlay (attr ^. runALens' l0) ("Edit " <> label)
     in pushOverlay mkDateEdit cb (return Continue) ignoreEvent

moveSubtreeModeKeymap :: Keymap (ComponentEventM' MainTree)
moveSubtreeModeKeymap =
  kmSetSticky $
    kmMake
      "Move subtree mode"
      -- SOMEDAY Can we reduce the number of different options? E.g., ("next based on preorder relative to self", "next based on siblings relative to parent") - Prob think about indicating the *target* relative to sth.
      [ (kmLeaf_ (bind 'j') "Down" $ moveCurRelativeDynamic dtoNextPreorder)
      , (kmLeaf_ (bind 'k') "Up" $ moveCurRelativeDynamic dtoPrevPreorder)
      , (kmLeaf_ (bind 'J') "Down same level" $ moveCurRelative goNextSibling insAfter)
      , (kmLeaf_ (bind 'K') "Up same level" $ moveCurRelative goPrevSibling insBefore)
      , (kmLeaf_ (bind 'h') "Before parent" $ moveCurRelative goParent insBefore)
      , (kmLeaf_ (bind '<') "After parent" $ moveCurRelative goParent insAfter)
      , (kmLeaf_ (bind 'L') "Last child of next" $ moveCurRelative goNextSibling insLastChild)
      , (kmLeaf_ (bind 'l') "First child of next" $ moveCurRelative goNextSibling insFirstChild)
      , (kmLeaf_ (bind '>') "Last child of previous" $ moveCurRelative goPrevSibling insLastChild)
      -- NB 'H' is not used and that's fine IMHO. I'm not sure why but these bindings were the most intuitive.
      -- SOMEDAY hierarchy-breaking '<' (dedent)
      ]

yankKeymap :: Keymap (ComponentEventM' MainTree)
yankKeymap =
  kmMake
    "Yank"
    [ kmLeaf (bind 'y') "Copy subtree to clipboard" copyCurToClipboard
    , kmLeaf_ (bind 'x') "Cut subtree to clipboard" cutCurToClipboard
    ]

pasteKeymap :: Keymap (ComponentEventM' MainTree)
pasteKeymap =
  kmMake
    "Paste"
    [ kmLeaf (bind 'p') "After current item" $ pasteClipboardRel insAfter
    , kmLeaf (bind 'P') "Before current item" $ pasteClipboardRel insBefore
    , kmLeaf (bind 's') "As last child" $ pasteClipboardRel insLastChild
    , kmLeaf (bind 'S') "As first child" $ pasteClipboardRel insFirstChild
    ]

openExternallyKeymap :: Keymap (ComponentEventM' MainTree)
openExternallyKeymap =
  kmMake
    "Open externally"
    [ ( kmLeaf_ (bind 'l') "First link in name" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) ->
          whenJust (findFirstMatch urlRegex name) $ \url -> liftIO (openURL url)
      )
    , ( kmLeaf_ (bind 'y') "Copy to clipboard" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) ->
          liftIO $ setClipboard name
      )
    , ( kmLeaf_ (bind 'x') "Copy first hex code" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) ->
          whenJust (findFirstMatch hexNumberRegex name) $ \code -> liftIO (setClipboard code)
      )
    , ( kmLeaf_ (bind 'n') "Copy first non-negative integer" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) ->
          whenJust (findFirstMatch decimalNonNegIntegerRegex name) $ \code -> liftIO (setClipboard code)
      )
    ]

sortRootKeymap :: Keymap (ComponentEventM' MainTree)
sortRootKeymap = _mkSortKeymap withRoot "Sort root by"

sortKeymap :: Keymap (ComponentEventM' MainTree)
sortKeymap =
  kmAddItems
    (_mkSortKeymap withCur "Sort by")
    [kmSub (bind 'R') sortRootKeymap]

-- | Either `withCur` or `withRoot`. Used to unify sorting.
type WithFunc =
  (EID -> ComponentEventM MainTree ()) ->
  (ComponentEventM MainTree ())

_mkSortKeymap :: WithFunc -> Text -> Keymap (ComponentEventM' MainTree)
_mkSortKeymap withFunc name =
  kmMake
    name
    $ (kmSub (bind 'D') $ kmMake "Deep" (mkItems sortDeepBelow))
      : mkItems sortShallowBelow
 where
  -- For some reason, I have to explicitly state that 'sorter' has a `?mue` context. Otherwise, it
  -- forgets about it and the code doesn't type check. Have to do this in both places.
  mkItems (sorter :: ((?mue :: ModelUpdateEnv) => a)) =
    [kmLeaf_ (bind 't') "Actionability" $ sortFuncBy sorter compareActionabilityForSort]
  -- For some reason, I have to explicitly specify the type of this function to specify when all
  -- the implicit parameters are supposed to be bound. This is in GHC2024 and probably related to this:
  -- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0287-simplify-subsumption.rst#motivation
  sortFuncBy ::
    ((?mue :: ModelUpdateEnv) => (Label -> Label -> Ordering) -> EID -> Model -> Model) ->
    (Label -> Label -> Ordering) ->
    ComponentEventM MainTree ()
  sortFuncBy sorter ord =
    let run root = modifyModelAsync (sorter ord root)
     in withFunc $ \root -> run root
  -- comparison function that puts notes first (which is what we usually want)
  compareActionabilityForSort :: Label -> Label -> Ordering
  compareActionabilityForSort l1 l2 = case (isNote l1, isNote l2) of
    (True, False) -> LT
    (True, True) -> EQ
    _ -> compare (gGlobalActionability l1) (gGlobalActionability l2)
  isNote l = (status . fst $ l) == None && gGlobalActionability l == None

goKeymap :: Keymap (ComponentEventM' MainTree)
goKeymap =
  kmMake
    "Go to"
    -- We need to redefine 'g' b/c we just overwrote the default binding from list.
    [ kmLeaf_ (bind 'g') "Top" $ callIntoTreeView $ TV.moveToBeginning
    , -- For completeness
      kmLeaf_ (bind 'e') "End" $ callIntoTreeView $ TV.moveToEnd
    , kmLeaf (bind 'C') "Clipboard" $ notFoundToAER_ $ moveRootToEID Clipboard
    , kmLeaf (bind 'I') "Inbox" $ notFoundToAER_ $ moveRootToEID Inbox
    , kmLeaf (bind 'V') "Vault" $ notFoundToAER_ $ moveRootToEID Vault
    , ( kmLeaf (binding KBS []) "De-hoist, keep pos" $ do
          -- SOMEDAY some code duplication vs the other de-hoist.
          mt <- get
          case breadcrumbs . mtSubtree $ mt of
            [] -> return Continue
            (par, _) : _ -> notFoundToAER_ $ do
              moveRootToEID par
              -- This is to stay at the current position, but if the subtree is empty, it
              -- should still do something for ergonomics, so we instead behave like the regular
              -- "de-hoist".
              let tgt = fromMaybe (mtRoot mt) (mtCur mt)
              replaceExceptT callIntoTreeView $ TV.moveToEID tgt
      )
    , ( kmLeaf (binding KEnter []) "Hoist 1 step, keep pos" $ withCurWithAttrOrElse (return Continue) $ \(cur, llabel) ->
          case reverse (gLocalBreadcrumbs llabel) of
            [] ->
              -- toplevel element, behave like Hoist (this is prob intended)
              notFoundToAER_ $ moveRootToEID cur
            (par : _) -> notFoundToAER_ $ do
              moveRootToEID (gEID par)
              replaceExceptT callIntoTreeView $ TV.moveToEID cur
      )
    , -- SOMEDAY I think some of this functionality should be in TreeView
      kmLeaf_ (bind 't') "Window top" $ withViewport $ \vp -> do
        let tgtIx = (vp ^. vpTop) + Config.scrolloff
        callIntoTreeView $ TV.moveToIndex tgtIx
    , kmLeaf_ (bind 'b') "Window bottom" $ withViewport $ \vp -> do
        let tgtIx = vp ^. vpTop + snd (vp ^. vpSize) - Config.scrolloff - 1
        callIntoTreeView $ TV.moveToIndex tgtIx
    , kmLeaf_ (bind 'c') "Window center" $ withViewport $ \vp -> do
        let tgtIx = vp ^. vpTop + snd (vp ^. vpSize) `div` 2
        callIntoTreeView $ TV.moveToIndex tgtIx
    ]
 where
  withViewport go = do
    mvp <- liftEventM $ lookupViewport =<< gets (getName . TV.tvList . mtTreeView)
    maybe (return ()) go mvp

searchKeymap :: Keymap (ComponentEventM' MainTree)
searchKeymap =
  kmSetSticky $
    kmMake
      "Find mode"
      [ ( kmLeaf_ (bind '/') "Search" $ do
            oldSearchRx <- use mtSearchRxL
            let
              initText = maybe "" cwsSource oldSearchRx
              onEvent = \case
                ValueChanged mv' -> mtSearchRxL .= maybeEmptyToMaybe mv'
              onConfirm (rx, ctype) = do
                assign mtSearchRxL (Just rx)
                callIntoTreeView $ case ctype of
                  RegularConfirm -> TV.searchForRxAction TV.Forward True
                  AltConfirm -> TV.searchForRxSiblingAction TV.Forward
                return Continue
              onCanceled = aerVoid $ mtSearchRxL .= oldSearchRx
             in
              pushOverlay (compilingRegexEntry initText) onConfirm onCanceled onEvent
        )
      , (kmLeaf_ (bind 'n') "Next match" $ callIntoTreeView $ TV.searchForRxAction TV.Forward False)
      , (kmLeaf_ (bind 'N') "Prev match" $ callIntoTreeView $ TV.searchForRxAction TV.Backward False)
      , (kmLeaf_ (meta 'n') "Next sibling match" $ callIntoTreeView $ TV.searchForRxSiblingAction TV.Forward)
      , -- SOMEDAY it's pretty fucking annoying that we can't bind alt-shift (b/c it's occupied by aerospace)
        ( kmLeaf_ (ctrl 'n') "Next sibling match" $
            callIntoTreeView $
              TV.searchForRxSiblingAction TV.Backward
        )
      , setSticky False (kmLeaf_ (bind 'l') "Clear" $ mtSearchRxL .= Nothing)
      , setSticky False (kmLeaf_ (ctrl 'l') "Clear" $ mtSearchRxL .= Nothing)
      -- TODO WIP design interaction pattern for search filter (using singleItemQueryFlatFilter)
      -- I feel the _very_ general filter infra may have reached its limits.
      ]

-- | Catchall keymap for variants and stuff
spaceKeymap :: Keymap (ComponentEventM' MainTree)
spaceKeymap =
  kmMake
    "Space"
    [ kmLeaf
        (bind '-')
        "Toggle collapse children"
        ( withCurOrElse (return Continue) $ \cur -> do
            -- See also collapseLevelKeymap
            normalFilter <- gets (cValue . mtSelectedFilter)
            actx <- ask
            model' <- liftIO $ getModel (acModelServer actx)
            root <- gets mtRoot
            notFoundToAER_ $ do
              -- Fetch a new model here. This lets us do this even if this node is collapsed.
              subtree <- liftEither $ runFilter (appContext2FilterContext actx) normalFilter root model'
              (Node _ cs) <- liftEither $ maybeToEither IdNotFoundError $ forestFindTree cur (stForest subtree)
              let c_eids = [gEID c | (Node c _) <- cs]
              -- This allows to "undo" the child collapsing by child-collapsing again.
              let f hhf =
                    let val = hhfIsCollapsed cur hhf || (not $ all (\eid -> hhfIsCollapsed eid hhf) c_eids)
                     in hhfSetCollapseds c_eids val hhf
              modifyHideHierarchyFilter (hhfSetCollapseds [cur] False . f)
        )
    , kmLeaf_ (bind 'R') "Global refile" $ pushRefileOverlay Vault "Global refile"
    , kmLeaf_ (bind 'r') "Local refile" $ do
        root <- gets mtRoot
        pushRefileOverlay root "Local refile"
    , kmLeaf_ (bind 'n') "New as parent" $ withCur $ \cur -> do
        -- Copied from 'pushInsertNewItemRelToCur' but that function can't handle "insert as parent".
        let cb name = do
              ztime <- asks acZonedTime
              let attr = attrMinimal (zonedTimeToUTC ztime) name
              uuid <- liftIO nextRandom
              eok <- runExceptT $ modifyModelSync $ insertNewNormalAsParentWithNewId uuid attr cur
              case eok of
                Left _err -> return Canceled
                Right () -> do
                  let eid = EIDNormal uuid
                  callIntoTreeView $ TV.moveToEID eid
                  return Continue
        pushOverlay (newNodeOverlay "" "New Item as Parent") cb (return Continue) absurd
    , kmLeaf_ (bind 'j') "Quick jump" $ do
        tv <- gets mtTreeView
        let cb (mCompiledRegex, eid) = do
              callIntoTreeView $ do
                TV.moveToEID eid
                whenJust mCompiledRegex $ \rxs ->
                  TV.tvSearchRxL .= Just rxs
              return Continue
        s <- maybe "" cwsSource <$> gets (TV.tvSearchRx . mtTreeView)
        pushOverlay
          (QF.quickFilterFromTreeView QF.NodeSelection tv s "Quick jump")
          cb
          (return Continue)
          absurd
    ]

-- SOMEDAY these actions should be functions in MainTree
viewportKeymap :: Keymap (ComponentEventM' MainTree)
viewportKeymap =
  kmMake
    "Viewport"
    -- These all don't respect item height != 1
    -- These all respect our scrolloff without doing this manually b/c they're dominated by our
    -- visibility requests in the rendering routine.
    -- SOMEDAY maybe these should be in MainTree
    [ kmLeaf_ (bind 't') "Align to top" $ withSelIxViewportName $ \seli _vp n -> do
        setTop (viewportScroll n) seli
    , kmLeaf_ (bind 'b') "Align to bottom" $ withSelIxViewportName $ \seli vp n -> do
        let vpHeight = snd (vp ^. vpSize)
            topOff = max 0 (seli - vpHeight)
        setTop (viewportScroll n) topOff
    , kmLeaf_ (bind 'z') "Align to center" $ withSelIxViewportName $ \seli vp n -> do
        let vpHeight = snd (vp ^. vpSize)
            topOff = max 0 (seli - vpHeight `div` 2)
        setTop (viewportScroll n) topOff
    , kmLeaf_ (bind 'j') "Scroll down" $ callIntoTreeView $ TV.scrollBy 3
    , kmLeaf_ (bind 'k') "Scroll up" $ callIntoTreeView $ TV.scrollBy (-3)
    ]
 where
  withSelIxViewportName ::
    (Int -> Viewport -> AppResourceName -> EventM AppResourceName MainTree ()) ->
    ComponentEventM MainTree ()
  withSelIxViewportName go = void $ runMaybeT $ do
    l <- gets (TV.tvList . mtTreeView)
    let n = getName l
    seli <- hoistMaybe (L.listSelected l)
    vp <- MaybeT $ liftEventM $ lookupViewport n
    lift $ liftEventM $ go seli vp n

debugKeymap :: Keymap (ComponentEventM' MainTree)
debugKeymap =
  kmMake
    "Debug"
    [ kmLeaf_ (bind 'n') "Log StatusActionabilityCounts for sel" $ withCurWithAttr $ \cura ->
        liftIO $ glogL INFO (show $ daNDescendantsByActionability . getDerivedAttr $ cura)
    ]

-- * Operations

-- ** Overlays

pushOverlay ::
  (AppComponent s, MonadState MainTree m, MonadIO m) =>
  -- | Component creator
  (AppResourceName -> s) ->
  -- | onConfirm
  (Return s -> ComponentEventM MainTree (AppEventReturn ())) ->
  -- | onCanceled
  (ComponentEventM MainTree (AppEventReturn ())) ->
  -- | onEvent
  (Event s -> ComponentEventM MainTree ()) ->
  m ()
pushOverlay mk onConfirm onCanceled onEvent = do
  hasExisting <- isJust <$> use mtOverlayL
  when hasExisting $
    let s =
          "MainTree: Pushing overlay onto an existing overlay. Old one will be thrown away. "
            ++ "This is likely not intended."
     in liftIO $ glogL WARNING s
  rootRname <- use mtResourceNameL
  let rname = rootRname <> "overlay"
  let ol = Overlay (mk rname) onConfirm onCanceled onEvent
  mtOverlayL .= Just ol

-- ** Item manipulation

pushInsertNewItemRelToCur ::
  InsertWalker IdLabel -> ComponentEventM MainTree ()
pushInsertNewItemRelToCur go = do
  state <- get
  ztime <- asks acZonedTime
  let (tgt', go') = case mtCur state of
        Just cur -> (cur, go)
        Nothing -> (mtRoot state, insLastChild)
  let cb name = do
        let attr = attrMinimal (zonedTimeToUTC ztime) name
        uuid <- liftIO nextRandom
        -- NB we *have* to use 'modifyModelSync' here b/c that will reload the model synchronously so we
        -- find the new EID below.
        -- SOMEDAY When this is async, we should just wait for the EID to appear in a ModelUpdated
        -- message.
        -- The following handles the error when our root was just deleted.
        -- SOMEDAY handle the error when the root is ok, but the parent below we wanna insert was
        -- deleted. Currently, nothing happens (which is fine but should be reported)
        eok <- runExceptT $ modifyModelSync $ insertNewNormalWithNewId uuid attr tgt' go'
        case eok of
          Left _err -> return Canceled
          Right () -> do
            let eid = EIDNormal uuid
            callIntoTreeView $ TV.moveToEID eid
            return Continue
  pushOverlay (newNodeOverlay "" "New Item") cb (return Continue) absurd

pushQuickAddToInbox :: ComponentEventM MainTree ()
pushQuickAddToInbox = do
  let cb name = do
        ztime <- asks acZonedTime
        let attr = attrMinimal (zonedTimeToUTC ztime) name
        uuid <- liftIO nextRandom
        modifyModelAsync $ insertNewNormalWithNewId uuid attr Inbox insLastChild
        return Continue
  pushOverlay (newNodeOverlay "" "Quick Add to INBOX") cb (return Continue) absurd

pushRefileOverlay :: EID -> Text -> ComponentEventM MainTree ()
pushRefileOverlay pickerRoot title = withCur $ \source -> do
  actx <- ask
  model' <- liftIO $ getModel (acModelServer actx)
  initSearch <- maybe "" cwsSource <$> gets (TV.tvSearchRx . mtTreeView)
  let mtv =
        TV.makeFromModel' (appContext2FilterContext actx) pickerRoot (refileDestinationFilter source) model'
  case mtv of
    Left _err -> return ()
    Right mkTreeView ->
      let
        mk rname =
          QF.quickFilterFromTreeView
            QF.RefileDestinationSelection
            (mkTreeView False Config.scrolloff (rname <> "treeview"))
            initSearch
            title
            rname
        cb (QF.RefileDestination insertion anchor) = do
          -- This must be sync so the temporary no-follow setting applies to the reload. Refile
          -- should leave the focus alone, just refile "away from" the current item.
          -- Alternatively, we could just not set this and then refile would follow if this is set,
          -- but that's a bit inconsistent b/c _global_ refile may refile into a different root and
          -- then we cannot follow (at least not without changing our root as well).
          -- MAYBE I guess we could retain the follow setting on local refile but not global. I'm
          -- not sure this would be more intuitive, though. Maybe it would be.
          withLensValue mtDoFollowItemL False $
            modifyModelSync_ $
              moveSubtreeRelToAnchor source anchor insertion
          return Continue
       in
        pushOverlay mk cb (return Continue) absurd

refileDestinationFilter :: EID -> Filter
refileDestinationFilter source =
  Filter
    { fiName = "refile destinations"
    , fiDesc = "Non-done items excluding the source subtree"
    , fiIncludeDone = False
    , fiPostprocess = filterIdForestWithIds isDestination
    }
 where
  isDestination eid llabel =
    eid /= source && all ((/= source) . gEID) (gLocalBreadcrumbs llabel)

setStatus :: (MonadState MainTree m, MonadReader AppContext m, MonadIO m) => Status -> m ()
setStatus status' = withCur $ \cur -> do
  ztime <- asks acZonedTime
  modifyModelAsync $
    let f = setLastStatusModified (zonedTimeToUTC ztime) . (statusL .~ status')
     in modifyAttrByEID cur f

touchLastStatusModified :: (MonadReader AppContext m, MonadIO m, MonadState MainTree m) => m ()
touchLastStatusModified = withCur $ \cur -> do
  ztime <- asks acZonedTime
  modifyModelAsync $
    let f = setLastStatusModified (zonedTimeToUTC ztime)
     in modifyAttrByEID cur f

-- ** Moving Nodes

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
--
-- TODO why is this sync?
moveCurRelative ::
  GoWalker LocalIdLabel -> InsertWalker IdLabel -> ComponentEventM MainTree ()
moveCurRelative go ins = withCur $ \cur -> do
  forest <- gets (stForest . mtSubtree)
  -- The temporary follow setting makes sure we follow our item around. This is sync so that the temporary setting works. (:())
  withLensValue mtDoFollowItemL True $
    modifyModelSync_ (moveSubtreeRelFromForest cur go ins forest)

-- SOMEDAY ^^ Same applies. Also, these could all be unified.
moveCurRelativeDynamic ::
  DynamicMoveWalker LocalIdLabel IdLabel ->
  ComponentEventM MainTree ()
moveCurRelativeDynamic dgo = withCur $ \cur -> do
  forest <- gets (stForest . mtSubtree)
  withLensValue mtDoFollowItemL True $
    modifyModelSync_ (moveSubtreeRelFromForestDynamic cur dgo forest)

copyCurToClipboard :: ComponentEventM MainTree (AppEventReturn ())
copyCurToClipboard = do
  mcur <- gets mtCur
  case mcur of
    Nothing -> return Continue
    Just cur -> do
      mserver <- asks acModelServer
      -- We need the current full model here to count IDs in the unfiltered subtree before generating
      -- UUIDs. This duplicates some work with the eventual model update; see
      -- 'copySubtreeToClipboardWithNewIds'.
      model <- liftIO $ getModel mserver
      case forestFindTree cur (forest model) of
        Nothing -> notFoundToAER_ $ replaceExceptT callIntoTreeView TV.reloadModel
        Just tree -> do
          -- This count can race with later model changes; see 'copySubtreeToClipboardWithNewIds'.
          uuids <- liftIO $ replicateM (length $ subtreeNormalIds tree) nextRandom
          modifyModelAsync $ copySubtreeToClipboardWithNewIds uuids cur
          return Continue

cutCurToClipboard :: ComponentEventM MainTree ()
cutCurToClipboard = withCur $ \cur ->
  modifyModelAsync $ cutSubtreeToClipboard cur

pasteClipboardRel :: InsertWalker IdLabel -> ComponentEventM MainTree (AppEventReturn ())
pasteClipboardRel insRequested = do
  mcur <- gets mtCur
  root <- gets mtRoot
  let (anchor, ins) = case mcur of
        Just cur -> (cur, insRequested)
        Nothing -> (root, insLastChild)
  mserver <- asks acModelServer
  -- This can race with another clipboard edit before the model update below. That's tolerable
  -- because the value is only used to focus the pasted node after reload.
  --
  -- SOMEDAY let model modification calls return data so this focus target can come from the same
  -- model transaction as the paste.
  mpayload <- clipboardFirstEntry <$> liftIO (getModel mserver)
  case mpayload of
    Nothing -> return Continue
    Just payload -> do
      notFoundToAER_ $ do
        modifyModelSync $ pasteFirstClipboardEntryRelTo anchor ins
        replaceExceptT callIntoTreeView $ TV.moveToEID payload

-- ** Model Modification

-- | Modify the model and *synchronously* pull the new model. Currently required when adding nodes.
--
-- SOMEDAY Synchronicity is a design issue really. Also, we're gonna get another ModelUpdated event
-- and then refresh a second time, which is bad.
modifyModelSync ::
  ((?mue :: ModelUpdateEnv) => Model -> Model) ->
  -- This is `ExceptT IdNotFoundError (EventM n MainTree) ()` but I'm not using it that much.
  ComponentEventMOrNotFound MainTree ()
modifyModelSync f = do
  mserver <- asks acModelServer
  liftIO $ do
    -- needs to be re-written when we go more async. Assumes that the model update is performed *synchronously*!
    -- SOMEDAY should we just not pull here (and thus remove everything after this) and instead rely on the ModelUpdated event?
    modifyModelOnServer mserver f
  replaceExceptT callIntoTreeView TV.reloadModel

modifyModelSync_ ::
  ((?mue :: ModelUpdateEnv) => Model -> Model) ->
  ComponentEventM MainTree ()
modifyModelSync_ f = void . notFoundToAER_ $ modifyModelSync f

-- | Modify the model asynchronously, i.e., *without* pulling a new model immediately. We get a
-- 'ModelUpdated' event and will pull a new model then. This is fine for most applications.
--
-- NB: It's not actually async right now b/c ModelServer doesn't operate async, but it could be in
-- the future.
--
-- NB: We currently *don't* change our focus based on the modified node. This is probably ok and
-- what the user expects, but should then be reviewed, if we ever get async here.
modifyModelAsync ::
  (MonadIO m, MonadReader AppContext m) =>
  ((?mue :: ModelUpdateEnv) => Model -> Model) ->
  m ()
modifyModelAsync f = do
  mserver <- asks acModelServer
  liftIO $ modifyModelOnServer mserver f

-- ** Navigation

moveRootToEID :: EID -> ComponentEventMOrNotFound MainTree ()
moveRootToEID eid = replaceExceptT callIntoTreeView $ TV.moveRootToEID eid

-- TODO WIP
--
-- - Harden the API of TreeView a bit. Maybe hide a bunch of stuff in an export list.
-- - Check for all TODOs just introduced in this branch.
-- - Functionality test.

-- ** Filters

-- | Reset filter of the tree view to match our currently selected one here.
resetTreeViewFilter :: ComponentEventMOrNotFound MainTree ()
resetTreeViewFilter = do
  fi <- gets mtFilter
  replaceExceptT callIntoTreeView $ TV.replaceFilter fi
 where
  mtFilter :: MainTree -> Filter
  mtFilter mt = chainFilters collapseFilter normalFilter
   where
    collapseFilter = hideHierarchyFilter . cValue . mtHideHierarchyFilter $ mt
    normalFilter = cValue . mtSelectedFilter $ mt

-- | Update the hierarchy-collapse filter and refresh the tree view.
modifyHideHierarchyFilter ::
  (HideHierarchyFilter -> HideHierarchyFilter) -> ComponentEventMOrNotFound MainTree ()
modifyHideHierarchyFilter f = C.runModifyLens mtHideHierarchyFilterL f

-- | Select a view filter and refresh the tree view.
selectFilter :: Filter -> ComponentEventMOrNotFound MainTree ()
selectFilter = C.runUpdateLens mtSelectedFilterL

-- * Rendering

-- SOMEDAY also deadline for the root (if any)?
renderRoot :: ZonedTime -> Label -> [IdLabel] -> Widget n
renderRoot ztime glabel breadcrumbs =
  hBox
    [statusW, str " ", pathW]
 where
  statusW = renderStatus False (gStatus glabel) (gGlobalActionability glabel)
  pathW = renderLabelWithBreadcrumbs ztime glabel breadcrumbs

renderLabelWithBreadcrumbs :: ZonedTime -> Label -> [IdLabel] -> Widget n
renderLabelWithBreadcrumbs ztime rootLabel breadcrumbs =
  rootW <+> renderBreadcrumbs BreadcrumbsLeafFirst ztime breadcrumbs
 where
  rootW = renderLabelShort ztime rootLabel

-- | Render the currently selected filter.
renderFilter :: Filter -> Widget n
renderFilter f = withDefAttr AppAttr.filter_label $ str (fiName f)

renderItemDetails :: ZonedTime -> LocalIdLabel -> Widget n
renderItemDetails ztime (eid, llabel) =
  padLeftRight 1 $
    withDefAttr (attrName "item_details") $
      vBox
        [ padBottom (Pad 1) topBox
        , -- TODO try a vertical line instead of padding. There should be some example in brick
          padBottom (Pad 1) $ hBox [padRight (Pad 5) leftBox, rightBox]
        , botBox
        ]
 where
  topBox =
    -- We cannot make this a table b/c `strWrap` has greedy growth and tables don't support that.
    -- NB in principle, we also don't *need* a table here but sth less general would be fine.
    vBox
      [ -- hBox [str "EID    ", str (showEIDShort eid)],
        -- hBox [str "Title  ", strWrapWith nameWrapSettings (name attr)]
        padBottom (Pad 1) $
          hBox
            [ renderStatus False (gStatus llabel) (gLocalActionability llabel)
            , str " "
            , strWrapWith nameWrapSettings (gName llabel)
            ]
      , -- SOMEDAY all these conversions are pretty fucking annoying. Maybe use lenses? Proper data
        -- structures for the different *Label things?
        renderBreadcrumbs
          BreadcrumbsLeafFirst
          ztime
          (map localIdLabel2IdLabel . gLocalBreadcrumbs $ llabel)
      ]
  botBox =
    vBox
      [hBox [str "EID  ", str (showEIDShort eid)]]
  nameWrapSettings = defaultWrapSettings {breakLongWords = True}
  -- tbl
  --   [ [str "EID", str (showEIDShort eid)],
  --     -- TODO strWrap doesn't work here. I'm getting TEInvalidCellSizePolicy
  --     -- I think I can't have greedy (not fixed) growth policy in a table.
  --     [str "Title", strWrap (name attr)]
  --   ]
  leftBox =
    tbl $
      -- SOMEDAY we may wanna make this not one big table but separate the bottom part out into
      -- another vBox element below the rest. Looks a bit strange re reserved space rn.
      [ sectionHeaderRow "Status"
      , [str "Status", str (show $ gStatus llabel)]
      , [str "Actionability", str (show $ gLocalActionability llabel)]
      , [str "Child Actionability", str (show $ gChildActionability llabel)]
      , [str "Parent Actionability", str (show $ gParentActionability llabel)]
      , spacerRow
      , sectionHeaderRow "Metadata"
      ]
        ++ mkAutodatesCells "" (gAutoDates llabel)
        ++ [spacerRow]
        ++ mkAutodatesCells "Latest " (gLatestAutodates llabel)
        ++ [spacerRow]
        ++ mkAutodatesCells "Earliest " (gEarliestAutodates llabel)
  rightBox =
    tbl $
      [sectionHeaderRow "Dates"]
        ++ mkDatesCells "" (gDates llabel)
        ++ [ spacerRow
           , sectionHeaderRow "Implied Dates"
           ]
        ++ mkDatesCells "" (gImpliedDates llabel)
        ++ [ spacerRow
           , sectionHeaderRow "Descendant Dates"
           ]
        ++ mkDatesCells "Earliest " (gEarliestDates llabel)
        ++ [spacerRow]
        ++ mkDatesCells "Latest" (gLatestDates llabel)
  sectionHeaderRow s = [withAttr sectionHeaderAttr (str s), emptyWidget]
  spacerRow = [str " ", emptyWidget]
  mkAutodatesCells prefix ad =
    [ [str (prefix ++ "Created"), renderUTCTime (created ad)]
    , [str (prefix ++ "Modified"), renderUTCTime (lastModified ad)]
    , [str (prefix ++ "Status Modified"), renderUTCTime (lastStatusModified ad)]
    ]
  mkDatesCells prefix ds =
    [ [str (prefix ++ "Deadline"), renderMDate (deadline ds)]
    , [str (prefix ++ "Goalline"), renderMDate (goalline ds)]
    , [str (prefix ++ "Scheduled"), renderMDate (scheduled ds)]
    , [str (prefix ++ "Remind"), renderMDate (remind ds)]
    ]
  renderMDate :: Maybe DateOrTime -> Widget n
  -- For some reason, emptyWidget doesn't work with `setWidth`.
  renderMDate d = setWidth 21 . maybe (str " ") (str . prettyAbsolute (zonedTimeZone ztime)) $ d
  renderUTCTime :: UTCTime -> Widget n
  renderUTCTime d = renderMDate (Just . DateAndTime $ d)
  tbl =
    renderTable
      . surroundingBorder False
      . columnBorders False
      . rowBorders False
      . setDefaultColAlignment AlignLeft
      . table
      . map padFirstCell
  padFirstCell [] = []
  padFirstCell (h : t) = padRight (Pad 2) h : t
  sectionHeaderAttr = attrName "section_header"

-- * Component instance

-- | We use `Confirmed ()` to indicate that the user *asked* to exit and `Canceled ()` to indicate
-- that there was some kind of issue and the tab had to close (e.g., the parent was deleted.)
--
-- SOMEDAY that's a bit nasty.
instance AppComponent MainTree where
  type Return MainTree = ()

  -- TODO we want some events don't we?
  type Event MainTree = ()

  renderComponentWithOverlays
    s@MainTree
      { mtTreeView =
        mtTreeView@(TV.TreeView {tvSubtree, tvDoFollowItem})
      , mtSelectedFilter
      , mtShowDetails
      , mtOverlay
      } =
      (box, catMaybes [ovl, detailsOvl])
     where
      Subtree {rootLabel, breadcrumbs} = cValue tvSubtree

      -- NOT `hAlignRightLayer` b/c that breaks background colors in light mode for some reason.
      headrow =
        withDefAttr AppAttr.header_row $
          hBox
            [ padRight Max $ renderRoot now rootLabel breadcrumbs
            , str "  "
            , renderStatusActionabilityCounts . daNDescendantsByActionability . getDerivedAttr $ rootLabel
            , str "   "
            , renderFilter . cValue $ mtSelectedFilter
            , str " "
            , doFollowBox
            ]
      doFollowBox = withDefAttr AppAttr.follow_box $ str (if tvDoFollowItem then "(follow)" else "(keep)")
      listW = renderComponent mtTreeView
      cmdBarW = almostEmptyWidget -- nothing here yet, just reserving some space
      -- box = headrow <=> listW <=> statusBarW <=> cmdBarW
      box = vBox [headrow, listW, statusBarW BreadcrumbsLeafFirst now (mtCurWithAttr s), cmdBarW]
      detailsOvl = case (mtShowDetails, mtCurWithAttr s) of
        (True, Just illabel) -> Just ("Item Details", renderItemDetails now illabel)
        _ -> Nothing
      ovl = mtOverlay <&> \(Overlay {olState = ols}) -> (componentTitle ols, renderComponent ols)
      now = acZonedTime ?actx

  -- NB we normally return Continue. There's noting properly to confirm or cancel here.
  -- We return Confirmed if the user requests to close the tab and Canceled if there was an error
  -- (specifically, our subtree was deleted)
  handleEvent ev = case ev of
    (AppEvent (ModelUpdated _)) -> do
      -- NB the return value of tryRouteToOverlay is the return value of the *handler* in case the
      -- overlay returned. This is practically always Continue in our case.
      -- SOMEDAY should be handled if this ever changes.
      --
      -- Note: errors (specifically, deleted roots of the subtrees) are still handled correctly.
      -- Components return Canceled in this case, and (1) for the overlay, that will close it before
      -- returning Continue for *ourselves* and (2) for the tree view, this result is actually used,
      -- and would have us return Canceled.
      _returnIsAlwaysContinue <- tryRouteToOverlay
      routeToTreeView
    -- TODO I think I should process Tick somehow. E.g. filters depending on time, or something.
    -- though again, if anything actually depends on time, we should prob include it during keypress as well.
    (AppEvent Tick) -> return Continue
    (VtyEvent _) -> routeToOverlayOr routeToSelf
    -- SOMEDAY if we have several active *visible/clickable* widgets, we may wanna route based on rname,
    -- like in QuickFilter.
    SomeMouse -> routeToOverlayOr routeToTreeView
   where
    routeToOverlayOr actNoOverlay = do
      mol <- use mtOverlayL
      case mol of
        Just (Overlay ols onConfirm onCanceled onEvent) -> do
          (ols', (res, events)) <- nestComponentEventM ols (handleEvent ev)
          mapM_ onEvent events
          mtOverlayL .= Just (Overlay ols' onConfirm onCanceled onEvent)
          case res of
            Continue -> return Continue
            Confirmed x -> mtOverlayL .= Nothing >> onConfirm x
            Canceled -> mtOverlayL .= Nothing >> onCanceled
        Nothing -> actNoOverlay

    tryRouteToOverlay = routeToOverlayOr (return Continue)

    routeToSelf = case ev of
      VtyKeyEvent key mods -> kmzDispatch mtKeymapL key mods routeToTreeView
      -- vvv never happens b/c caught above.
      _miscEvents -> routeToTreeView

    routeToTreeView = callIntoTreeView $ handleEvent ev

  componentKeyDesc s = case mtOverlay s of
    Nothing -> kmzDesc . mtKeymap $ s
    Just (Overlay {olState = ols}) -> (componentKeyDesc ols) {kdIsToplevel = False} -- always show key help for overlays

  -- "Root name - Realm" unless the Realm is the root itself, or higher.
  componentTitle s = T.pack $ pathStr
   where
    -- pathStr = intercalate " < " $ name (rootAttr mtSubtree) : [name attr | (_, attr) <- (breadcrumbs mtSubtree)]
    pathStr = case mRealmBreadcrumb of
      Nothing -> rootName
      Just (_, (c, _)) -> rootName ++ " - " ++ name c
    mRealmBreadcrumb = case reverse (breadcrumbs . mtSubtree $ s) of
      _ : c : _ -> Just c
      _ -> Nothing
    rootName = name (fst . rootLabel . mtSubtree $ s)

-- TODO killme
{- handleEventOld ev =
  -- TODO process the Tick event and update its filter b/c last-modified now depends on time (in a hacky way). - Or maybe explicitly don't and add a manual refresh??
  wrappingActions $
    case ev of
      -- Keymap
      (VtyEvent (Vty.EvKey key mods)) -> do
        keymap <- use mtKeymapL
        liftIO $ glogL DEBUG $ "handle a key from keymap"
        case kmzLookup keymap key mods of
          NotFound -> case ev of
            -- Code for keymap. We handle this here so that we can bind Backspace in a submap
            -- (and also Esc, though that's a bit too funky for my taste). NB this is a bit nasty,
            -- having some abstraction here would be good if we need it again.
            -- SOMEDAY slightly inconsistent: if the user should expect BS to always go up, we
            -- shouldn't bind it to anything else.
            (VtyEvent (Vty.EvKey KEsc [])) -> aerVoid $ mtKeymapL %= kmzResetRoot
            (VtyEvent (Vty.EvKey KBS [])) -> aerVoid $ mtKeymapL %= kmzUp
            _ -> do
              liftIO $ glogL DEBUG "handle fallback"
              handleFallback ev
          LeafResult act nxt -> do
            liftIO $ glogL DEBUG "handle leaf"
            res <- runAppEventAction act
            mtKeymapL .= nxt
            return res
          SubmapResult sm -> do
            liftIO $ glogL DEBUG "handle submap"
            mtKeymapL .= sm
            aerContinue
      _miscEvents -> handleFallback ev
 where
  wrappingActions actMain = notFoundToAER $ do
    -- <<Global updates that should happen even if there's an active overlay would go here.>>
    case ev of
      -- TODO what happens if the current root disappears?
      -- Then TreeView's handleEvent should return Canceled but (1) I need to check it does and (2) that would also be ignored here. I think the structure is kinda bad. Maybe we need a combinator at the level of AppEventReturn?
      -- Also not clear to me why we need to process it here.
      -- This whole structure is kinda brittle tbh.
      (AppEvent (ModelUpdated _)) -> void $ lift $ callIntoTreeView $ handleEvent ev
      _ -> return ()

    -- Now pass the event to either the overlay or to our main function.
    -- (these don't raise errors anymore)
    -- TODO actually why? What happens when there is a QuickFilter and reloading fails?
    lift $ do
      mol <- use mtOverlayL
      case mol of
        Just (Overlay ols onConfirm onCanceled onEvent) -> do
          (ols', (res, events)) <- nestAppEventM ols (handleEvent ev)
          mapM_ onEvent events
          mtOverlayL .= Just (Overlay ols' onConfirm onCanceled onEvent)
          case res of
            Continue -> aerContinue
            Confirmed x -> mtOverlayL .= Nothing >> onConfirm x
            Canceled -> mtOverlayL .= Nothing >> onCanceled
        Nothing -> actMain
  handleFallback e = callIntoTreeView $ handleEvent e -}
