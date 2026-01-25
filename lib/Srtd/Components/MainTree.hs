{-# HLINT ignore "Redundant bracket" #-}
-- For overlay management
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Srtd.Components.MainTree (MainTree (..), make) where

import Brick hiding (on)
import Brick.BChan (writeBChan)
-- hiding to avoid a name clash

import Brick.Keybindings (Binding, bind, ctrl, meta)
import Brick.Keybindings.KeyConfig (binding)
import Brick.Widgets.List qualified as L
import Brick.Widgets.Table
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.CircularList qualified as CList
import Data.Functor (void)
import Data.List (intersperse)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, zonedTimeToUTC, zonedTimeZone)
import Data.Tree (Tree (Node))
import Data.UUID.V4 (nextRandom)
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Lens.Micro.Platform
import Srtd.AppAttr qualified as AppAttr
import Srtd.Attr hiding (Canceled)
import Srtd.Attr qualified (Status (Canceled))
import Srtd.BrickHelpers
import Srtd.Component
import Srtd.Component qualified as Component
import Srtd.Components.Attr (
  actionabilityAttr,
  mostUrgentDateAttr,
  renderMostUrgentDateMaybe,
  renderStatus,
 )
import Srtd.Components.CompilingTextEntry
import Srtd.Components.DateSelectOverlay (dateSelectOverlay)
import Srtd.Components.NewNodeOverlay (newNodeOverlay)
import Srtd.Components.TreeView qualified as TV
import Srtd.Config qualified as Config
import Srtd.Data.IdTree
import Srtd.Data.MapLike qualified as MapLike
import Srtd.Data.TreeZipper
import Srtd.Dates (DateOrTime (..), prettyAbsolute)
import Srtd.Keymap
import Srtd.Log
import Srtd.Model
import Srtd.ModelServer
import Srtd.Util
import System.Hclip (setClipboard)
import Text.Regex.TDFA.Common (Regex)
import Text.Wrap (WrapSettings (..), defaultWrapSettings)

-- * Overlay infra (must come early b/c Template Haskell)

-- SOMEDAY can we make this a more general thing? Then also review how specific types have to be.
-- (probably not very specific.)

-- | A class for overlays. Two callbacks define interruptible computations. Note that this doesn't
-- store state of the computation *itself*.
--
-- SOMEDAY are we over-generalizing here? Compare this to explicitly naming the overlays we might have open.
--
-- Might also be related to that question of representation of having a data structure vs the list
-- of filled fields in some priority order (e.g., for dates rendering).
data Overlay = forall s a b. (AppComponent s a b) => Overlay
  { olState :: s
  , olOnContinue :: (?actx :: AppContext) => a -> EventM AppResourceName MainTree (AppEventReturn () ())
  , olOnConfirm :: (?actx :: AppContext) => b -> EventM AppResourceName MainTree (AppEventReturn () ())
  , olOnCanceled :: (?actx :: AppContext) => EventM AppResourceName MainTree (AppEventReturn () ())
  }

-- | Helper to use for 'olOnContinue' or 'olOnConfirm' when you want to ignore these events.
overlayNoop :: (Monad m) => p -> m (AppEventReturn () b)
overlayNoop _ = aerContinue

-- * Main Data Structure

data MainTree = MainTree
  { mtFilters :: CList.CList Filter
  , mtTreeView :: TV.TreeView
  , mtResourceName :: AppResourceName
  -- ^ Top-level resource name for this component. We can assign anything nested below (or "above") it.
  , mtKeymap :: KeymapZipper (AppEventAction MainTree () ())
  , mtShowDetails :: Bool
  -- ^ Whether or not to show the details view. This is not implemented as a full overlay
  -- component for simplicity.
  , mtOverlay :: Maybe Overlay
  -- ^ Active overlay, if any. If this is 'Just', events are forwarded to the overlay.
  --
  -- SOMEDAY do I want to make a wrapper for "things that have overlays" that *consistenly* handles everything?
  , mtHideHierarchyFilter :: HideHierarchyFilter
  }

suffixLenses ''MainTree

-- The first one is default selected.
defaultFilters :: [Filter]
defaultFilters =
  [ f_notDone
  , f_deepByDates
  , f_NotDelayedByLastModified
  , f_flatByDates
  , f_nextFlatByDates
  , f_waitingFlatByDates
  , f_stalledProjects
  , f_projectOverview
  , f_all
  ]

-- ** Convenience Accessors

mtSubtree :: MainTree -> Subtree
mtSubtree = TV.tvSubtree . mtTreeView

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
  EventM n MainTree a -> (EID -> EventM n MainTree a) -> EventM n MainTree a
withCurOrElse dflt go = maybe dflt go =<< gets mtCur

withCur ::
  (EID -> EventM n MainTree ()) -> EventM n MainTree ()
withCur = withCurOrElse (return ())

withCurWithAttr ::
  (LocalIdLabel -> EventM n MainTree ()) -> EventM n MainTree ()
withCurWithAttr = withCurWithAttrOrElse (return ())

withCurWithAttrOrElse ::
  EventM n MainTree a -> (LocalIdLabel -> EventM n MainTree a) -> EventM n MainTree a
withCurWithAttrOrElse dflt go = maybe dflt go =<< gets mtCurWithAttr

withRoot ::
  (EID -> EventM n MainTree ()) -> EventM n MainTree ()
withRoot go = go =<< gets mtRoot

-- * API

-- | Create new 'MainTree'
make :: (?actx :: AppContext) => EID -> Model -> AppResourceName -> Either IdNotFoundError MainTree
make root = makeWithFilters root (CList.fromList defaultFilters) emptyHideHierarchyFilter True

-- | filters must not be empty.
makeWithFilters ::
  (?actx :: AppContext) =>
  EID ->
  CList.CList Filter ->
  HideHierarchyFilter ->
  Bool ->
  Model ->
  AppResourceName ->
  Either IdNotFoundError MainTree
makeWithFilters root filters hhf doFollowItem model rname = do
  tv <-
    TV.makeFromModel
      root
      (chainFilters (hideHierarchyFilter hhf) (fromJust $ CList.focus filters))
      doFollowItem
      Config.scrolloff
      model
      (TreeFor rname)
  return $
    MainTree
      { mtFilters = filters
      , mtHideHierarchyFilter = hhf
      , mtTreeView = tv
      , mtResourceName = rname
      , mtKeymap = keymapToZipper rootKeymap
      , mtShowDetails = False
      , mtOverlay = Nothing
      }

-- | Toplevel app resource name, including all contained resources. This is a "cloning" routine.
--
-- TODO also handle overlays. To do this, AppComponent needs a function, which is prob a good idea.
setResourceName :: AppResourceName -> MainTree -> MainTree
setResourceName rname = (mtResourceNameL .~ rname) . (mtTreeViewL %~ TV.setResourceName rname)

-- * Keymaps

rootKeymap :: Keymap (AppEventAction MainTree () ())
rootKeymap =
  kmMake
    "Tree View"
    ( [ kmLeafA_ (bind 'n') "New as next sibling" $ pushInsertNewItemRelToCur insAfter
      , kmLeafA_ (bind 'N') "New as prev sibling" $ pushInsertNewItemRelToCur insBefore
      , kmLeafA_ (bind 'S') "New as first child" $ pushInsertNewItemRelToCur insFirstChild
      , kmLeafA_ (bind 's') "New as last child" $ pushInsertNewItemRelToCur insLastChild
      , ( kmLeafA_ (bind 'e') "Edit name" $ do
            state <- get
            case mtCurWithAttr state of
              Just (cur, ((curAttr, _), _)) -> do
                let oldName = name curAttr
                let cb name' = aerVoid $ do
                      let f = setLastModified (zonedTimeToUTC . acZonedTime $ ?actx) . (nameL .~ name')
                      modifyModelAsync $ modifyAttrByEID cur f
                      zoom mtTreeViewL $ TV.moveToEID cur
                pushOverlay (newNodeOverlay oldName "Edit Item") overlayNoop cb aerContinue
              Nothing -> return ()
        )
      , kmSub (ctrl 't') debugKeymap
      , ( kmLeafA_ (bind 'T') "New tab" $ do
            state <- get
            liftIO $
              writeBChan (acAppChan ?actx) $
                PushTab (\rname -> SomeAppComponent $ setResourceName rname state)
        )
      , (kmLeafA (bind 'q') "Close tab / quit" $ return $ Confirmed ())
      , ( kmLeafA_ (bind ']') "Next tab" $
            liftIO $
              writeBChan (acAppChan ?actx) $
                NextTab
        )
      , ( kmLeafA_ (bind '[') "Prev tab" $
            liftIO $
              writeBChan (acAppChan ?actx) $
                PrevTab
        )
      , ( kmLeafA_ (bind '}') "Swap tab next" $
            liftIO $
              writeBChan (acAppChan ?actx) $
                SwapTabNext
        )
      , ( kmLeafA_ (bind '{') "Swap tab prev" $
            liftIO $
              writeBChan (acAppChan ?actx) $
                SwapTabPrev
        )
      , ( kmLeafA_ (binding (KChar 'j') [MMeta]) "Move subtree down same level" $
            moveCurRelative goNextSibling insAfter
        )
      , ( kmLeafA_ (binding (KChar 'k') [MMeta]) "Move subtree up same level" $
            moveCurRelative goPrevSibling insBefore
        )
      , (kmLeafA_ (bind '<') "Move subtree after parent" $ moveCurRelative goParent insAfter)
      , ( kmLeafA_ (bind '>') "Move subtree last child of previous" $
            moveCurRelative goPrevSibling insLastChild
        )
      , -- (kmSub (bind 'm') moveSingleModeKeymap),
        (kmSub (bind 'M') moveSubtreeModeKeymap)
      , (kmSub (bind 'D') deleteKeymap)
      , ( kmLeafA (binding KEnter []) "Hoist" $ withCurOrElse aerContinue $ \cur -> do
            notFoundToAER_ $ moveRootToEID cur
        )
      , ( kmLeafA (binding KBS []) "De-hoist" $ do
            mt <- get
            case breadcrumbs . mtSubtree $ mt of
              [] -> aerContinue
              (par, _) : _ -> notFoundToAER_ $ do
                moveRootToEID par
                zoom mtTreeViewL $ lift $ TV.moveToEID (mtRoot mt)
        )
      , (kmSub (bind ';') sortKeymap)
      , (kmLeafA_ (bind 'h') "Go to parent" (zoom mtTreeViewL $ TV.moveGoWalkerFromCur goParent))
      , (kmLeafA_ (bind 'J') "Go to next sibling" (zoom mtTreeViewL $ TV.moveGoWalkerFromCur goNextSibling))
      , (kmLeafA_ (bind 'K') "Go to prev sibling" (zoom mtTreeViewL $ TV.moveGoWalkerFromCur goPrevSibling))
      , (kmLeafA_ (bind 'H') "Go to next uncle" (zoom mtTreeViewL $ TV.moveGoWalkerFromCur goNextAncestor))
      , (kmSub (bind 't') setStatusKeymap)
      , (kmSub (bind 'o') openExternallyKeymap)
      , (kmLeafA (bind ',') "Prev filter" $ notFoundToAER_ cyclePrevFilter)
      , (kmLeafA (bind '.') "Next filter" $ notFoundToAER_ cycleNextFilter)
      , (kmSub (bind 'd') editDateKeymap)
      , (kmLeafA_ (bind '`') "Toggle details overlay" (mtShowDetailsL %= not))
      , (kmSub (bind 'g') goKeymap)
      , (kmSub (bind 'f') searchKeymap)
      , -- SOMEDAY bind '/' to directly go to search. A bit of duplication b/c ESC should *not* go to
        -- the submenu in this case.
        (kmSub (bind 'v') viewKeymap)
      , ( kmLeafA
            (bind '-')
            "Un/collapse"
            ( withCurOrElse aerContinue $ \cur ->
                notFoundToAER_ $
                  modifyHideHierarchyFilter (hhfToggle cur)
            )
        )
      , ( kmLeafA
            (bind '0')
            "Uncollapse all"
            ( notFoundToAER_ $
                modifyHideHierarchyFilter (const emptyHideHierarchyFilter)
            )
        )
      , kmSub (bind ' ') spaceKeymap
      , kmLeafA_ (bind 'j') "Move down" (zoom mtTreeViewL $ TV.moveBy 1)
      , kmLeafA_ (bind 'k') "Move up" (zoom mtTreeViewL $ TV.moveBy (-1))
      , kmSub (bind 'z') viewportKeymap
      ]
    )
    `kmUnion` collapseLevelKeymap

-- TODO unclean that we have a static keymap but the list of filters is dynamic from the perspective of MainTree.
-- I don't think filters need to be dynamic.
-- Maybe the layout of the filters needs reworking. Maybe the filter structure shouldn't store the name.
viewKeymap :: Keymap (AppEventAction MainTree () ())
viewKeymap =
  kmMake
    "Filter"
    ( map
        -- TODO addressing by description suuuucks and is dangerous.
        mkMapping
        [ ('n', "not done")
        , ('a', "all")
        , ('u', "by simple urgency")
        , ('F', "flat, by simple urgency")
        , ('N', "flat next, by simple urgency")
        , ('W', "flat waiting, by urgency + age")
        , ('m', "non-delayed by last modified")
        , ('s', "stalled projects")
        , ('p', "projects by urgency")
        ]
        ++ [ kmLeafA_ (ctrl 'f') "Toggle follow item" (mtDoFollowItemL %= not)
           ]
    )
 where
  mkMapping (k :: Char, s :: String) = kmLeafA (bind k) (T.pack s) $ notFoundToAER_ (selectFilterByName s)

collapseLevelKeymap :: Keymap (AppEventAction MainTree () ())
collapseLevelKeymap =
  kmMake
    "Collapse level"
    (map (hide . collapseLevelKeymapItem) [1 .. 9])
    `kmAddAddlDesc` [("1-9", "Collapse to level")]
 where
  collapseLevelKeymapItem :: Int -> (Binding, KeymapItem (AppEventAction MainTree () ()))
  collapseLevelKeymapItem i =
    kmLeafA
      (bind $ unsafeSingleDigitUIntToChar i)
      ("Collapse level " <> (T.show i))
      ( do
          -- NB there's some double work going on here but I think it's fine.
          normalFilter <- gets (fromJust . CList.focus . mtFilters)
          model' <- liftIO $ getModel (acModelServer ?actx)
          root <- gets mtRoot
          notFoundToAER_ $ do
            subtree <- pureET $ translateAppFilterContext $ runFilter normalFilter root model'
            modifyHideHierarchyFilter $ const $ subtreeLevelHHF i subtree
      )
  subtreeLevelHHF i subtree = HideHierarchyFilter (Set.fromList $ forestIdsAtLevel i (stForest subtree))

deleteKeymap :: Keymap (AppEventAction MainTree () ())
deleteKeymap =
  kmMake
    "Delete"
    -- TOOD some undo would be nice, lol.
    [ (kmLeafA_ (bind 'D') "Subtree" $ withCur $ \cur -> modifyModelAsync (deleteSubtree cur))
    , (kmLeafA_ (bind 'd') "Single & splice" $ withCur $ \cur -> modifyModelAsync (deleteSingleSplice cur))
    ]

setStatusKeymap :: Keymap (AppEventAction MainTree () ())
setStatusKeymap =
  kmMake
    "Set status"
    [ kmLeafA_ (bind ' ') "None" (setStatus None)
    , kmLeafA_ (bind 'n') "Next" (setStatus $ Next)
    , kmLeafA_ (bind 'w') "Waiting" (setStatus $ Waiting)
    , kmLeafA_ (bind 'p') "Project" (setStatus $ Project)
    , kmLeafA_ (bind 'l') "Later" (setStatus $ Later)
    , kmLeafA_ (bind 'i') "WIP" (setStatus $ WIP)
    , kmLeafA_ (binding KEnter []) "Done" (setStatus $ Done)
    , kmLeafA_ (bind 'x') "Canceled" (setStatus $ Srtd.Attr.Canceled)
    , kmLeafA_ (bind 's') "Someday" (setStatus $ Someday)
    , kmLeafA_ (bind 'o') "Open" (setStatus $ Open)
    , kmLeafA_ (bind 't') "Touch" touchLastStatusModified
    ]

editDateKeymap :: Keymap (AppEventAction MainTree () ())
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
      ++ [ kmLeafA_
             (ctrl 'd')
             "Delete all"
             ( withCur $ \cur -> do
                 let f = setLastModified (zonedTimeToUTC $ acZonedTime ?actx) . (datesL .~ noDates)
                 modifyModelAsync (modifyAttrByEID cur f)
             )
         ]
 where
  mkDateEditShortcut (kb, label, l0) = kmLeafA_ kb label $ withCurWithAttr $ \(cur, ((attr, _), _)) ->
    let cb date' = aerVoid $ do
          let f = setLastModified (zonedTimeToUTC $ acZonedTime ?actx) . (runALens' l0 .~ date')
          modifyModelAsync $ modifyAttrByEID cur f
          zoom mtTreeViewL $ TV.moveToEID cur
        mkDateEdit = dateSelectOverlay (attr ^. runALens' l0) ("Edit " <> label)
     in pushOverlay mkDateEdit overlayNoop cb aerContinue

moveSubtreeModeKeymap :: Keymap (AppEventAction MainTree () ())
moveSubtreeModeKeymap =
  sticky $
    kmMake
      "Move subtree mode"
      -- SOMEDAY Can we reduce the number of different options? E.g., ("next based on preorder relative to self", "next based on siblings relative to parent") - Prob think about indicating the *target* relative to sth.
      [ (kmLeafA_ (bind 'j') "Down" $ moveCurRelativeDynamic dtoNextPreorder)
      , (kmLeafA_ (bind 'k') "Up" $ moveCurRelativeDynamic dtoPrevPreorder)
      , (kmLeafA_ (bind 'J') "Down same level" $ moveCurRelative goNextSibling insAfter)
      , (kmLeafA_ (bind 'K') "Up same level" $ moveCurRelative goPrevSibling insBefore)
      , (kmLeafA_ (bind 'h') "Before parent" $ moveCurRelative goParent insBefore)
      , (kmLeafA_ (bind '<') "After parent" $ moveCurRelative goParent insAfter)
      , (kmLeafA_ (bind 'L') "Last child of next" $ moveCurRelative goNextSibling insLastChild)
      , (kmLeafA_ (bind 'l') "First child of next" $ moveCurRelative goNextSibling insFirstChild)
      , (kmLeafA_ (bind '>') "Last child of previous" $ moveCurRelative goPrevSibling insLastChild)
      -- NB 'H' is not used and that's fine IMHO. I'm not sure why but these bindings were the most intuitive.
      -- SOMEDAY hierarchy-breaking '<' (dedent)
      ]

openExternallyKeymap :: Keymap (AppEventAction MainTree () ())
openExternallyKeymap =
  kmMake
    "Open externally"
    [ ( kmLeafA_ (bind 'l') "First link in name" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) ->
          whenJust (findFirstMatch urlRegex name) $ \url -> liftIO (openURL url)
      )
    , ( kmLeafA_ (bind 'y') "Copy to clipboard" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) ->
          liftIO $ setClipboard name
      )
    , ( kmLeafA_ (bind 'x') "Copy first hex code" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) ->
          whenJust (findFirstMatch hexNumberRegex name) $ \code -> liftIO (setClipboard code)
      )
    , ( kmLeafA_ (bind 'n') "Copy first non-negative integer" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) ->
          whenJust (findFirstMatch decimalNonNegIntegerRegex name) $ \code -> liftIO (setClipboard code)
      )
    ]

sortRootKeymap :: Keymap (AppEventAction MainTree () ())
sortRootKeymap = _mkSortKeymap withRoot "Sort root by"

sortKeymap :: Keymap (AppEventAction MainTree () ())
sortKeymap =
  kmAddItems
    (_mkSortKeymap withCur "Sort by")
    [kmSub (bind 'R') sortRootKeymap]

-- | Either `withCur` or `withRoot`. Used to unify sorting.
type WithFunc =
  (EID -> EventM AppResourceName MainTree ()) ->
  (EventM AppResourceName MainTree ())

_mkSortKeymap :: WithFunc -> Text -> Keymap (AppEventAction MainTree () ())
_mkSortKeymap withFunc name =
  kmMake
    name
    $ (kmSub (bind 'D') $ kmMake "Deep" (mkItems sortDeepBelow))
      : mkItems sortShallowBelow
 where
  -- For some reason, I have to explicitly state that 'sorter' has a `?mue` context. Otherwise, it
  -- forgets about it and the code doesn't type check. Have to do this in both places.
  mkItems (sorter :: ((?mue :: ModelUpdateEnv) => a)) =
    [kmLeafA_ (bind 't') "Actionability" $ sortFuncBy sorter compareActionabilityForSort]
  -- For some reason, I have to explicitly specify the type of this function to specify when all
  -- the implicit parameters are supposed to be bound. This is in GHC2024 and probably related to this:
  -- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0287-simplify-subsumption.rst#motivation
  sortFuncBy ::
    ((?mue :: ModelUpdateEnv) => (Label -> Label -> Ordering) -> EID -> Model -> Model) ->
    (Label -> Label -> Ordering) ->
    ((?actx :: AppContext) => EventM AppResourceName MainTree ())
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

goKeymap :: Keymap (AppEventAction MainTree () ())
goKeymap =
  kmMake
    "Go to"
    -- We need to redefine 'g' b/c we just overwrote the default binding from list.
    [ kmLeafA_ (bind 'g') "Top" $ zoom mtTreeViewL $ TV.moveToBeginning
    , -- For completeness
      kmLeafA_ (bind 'e') "End" $ zoom mtTreeViewL $ TV.moveToEnd
    , ( kmLeafA (binding KBS []) "De-hoist, keep pos" $ do
          -- SOMEDAY some code duplication vs the other de-hoist.
          mt <- get
          case breadcrumbs . mtSubtree $ mt of
            [] -> aerContinue
            (par, _) : _ -> notFoundToAER_ $ do
              moveRootToEID par
              -- This is to stay at the current position, but if the subtree is empty, it
              -- should still do something for ergonomics, so we instead behave like the regular
              -- "de-hoist".
              let tgt = fromMaybe (mtRoot mt) (mtCur mt)
              zoom mtTreeViewL $ lift $ TV.moveToEID tgt
      )
    , ( kmLeafA (binding KEnter []) "Hoist 1 step, keep pos" $ withCurWithAttrOrElse aerContinue $ \(cur, llabel) ->
          case reverse (gLocalBreadcrumbs llabel) of
            [] ->
              -- toplevel element, behave like Hoist (this is prob intended)
              notFoundToAER_ $ moveRootToEID cur
            (par : _) -> notFoundToAER_ $ do
              moveRootToEID (gEID par)
              zoom mtTreeViewL $ lift $ TV.moveToEID cur
      )
    , -- SOMEDAY I think some of this functionality should be in TreeView
      kmLeafA_ (bind 't') "Window top" $ withViewport $ \vp -> do
        let tgtIx = (vp ^. vpTop) + Config.scrolloff
        zoom mtTreeViewL $ TV.moveToIndex tgtIx
    , kmLeafA_ (bind 'b') "Window bottom" $ withViewport $ \vp -> do
        let tgtIx = vp ^. vpTop + snd (vp ^. vpSize) - Config.scrolloff - 1
        zoom mtTreeViewL $ TV.moveToIndex tgtIx
    , kmLeafA_ (bind 'c') "Window center" $ withViewport $ \vp -> do
        let tgtIx = vp ^. vpTop + snd (vp ^. vpSize) `div` 2
        zoom mtTreeViewL $ TV.moveToIndex tgtIx
    ]
 where
  withViewport go = do
    mvp <- lookupViewport =<< gets (getName . TV.tvList . mtTreeView)
    maybe (return ()) go mvp

searchKeymap :: Keymap (AppEventAction MainTree () b)
searchKeymap =
  sticky $
    kmMake
      "Find mode"
      [ ( kmLeafA_ (bind '/') "Search" $ do
            oldSearchRx <- use mtSearchRxL
            let
              initText = maybe "" cwsSource oldSearchRx
              onContinue = aerVoid . assign mtSearchRxL
              onConfirm (rx, ctype) = do
                assign mtSearchRxL (Just rx)
                zoom mtTreeViewL $ case ctype of
                  RegularConfirm -> TV.searchForRxAction TV.Forward True
                  AltConfirm -> TV.searchForRxSiblingAction TV.Forward
                aerContinue
              onCanceled = aerVoid $ assign mtSearchRxL oldSearchRx
             in
              pushOverlay (compilingRegexEntry initText) onContinue onConfirm onCanceled
        )
      , (kmLeafA_ (bind 'n') "Next match" $ zoom mtTreeViewL $ TV.searchForRxAction TV.Forward False)
      , (kmLeafA_ (bind 'N') "Prev match" $ zoom mtTreeViewL $ TV.searchForRxAction TV.Backward False)
      , (kmLeafA_ (meta 'n') "Next sibling match" $ zoom mtTreeViewL $ TV.searchForRxSiblingAction TV.Forward)
      , -- SOMEDAY it's pretty fucking annoying that we can't bind alt-shift (b/c it's occupied by aerospace)
        ( kmLeafA_ (ctrl 'n') "Next sibling match" $
            zoom mtTreeViewL $
              TV.searchForRxSiblingAction TV.Backward
        )
      , (kmLeafA_ (bind 'l') "Clear" $ mtSearchRxL .= Nothing)
      , (kmLeafA_ (ctrl 'l') "Clear" $ mtSearchRxL .= Nothing)
      -- TODO WIP design interaction pattern for search filter (using singleItemQueryFlatFilter)
      -- I feel the _very_ general filter infra may have reached its limits.
      ]

-- | Catchall keymap for variants and stuff
spaceKeymap :: Keymap (AppEventAction MainTree () ())
spaceKeymap =
  kmMake
    "Space"
    [ kmLeafA
        (bind '-')
        "Toggle collapse children"
        ( withCurOrElse aerContinue $ \cur -> do
            -- See also collapseLevelKeymap
            normalFilter <- gets (fromJust . CList.focus . mtFilters)
            model' <- liftIO $ getModel (acModelServer ?actx)
            root <- gets mtRoot
            notFoundToAER_ $ do
              -- Fetch a new model here. This lets us do this even if this node is collapsed.
              subtree <- pureET $ translateAppFilterContext $ runFilter normalFilter root model'
              (Node _ cs) <- pureET $ maybeToEither IdNotFoundError $ forestFindTree cur (stForest subtree)
              let c_eids = [gEID c | (Node c _) <- cs]
              -- This allows to "undo" the child collapsing by child-collapsing again.
              let f hhf =
                    let val = hhfIsCollapsed cur hhf || (not $ all (\eid -> hhfIsCollapsed eid hhf) c_eids)
                     in hhfSetCollapseds c_eids val hhf
              modifyHideHierarchyFilter (hhfSetCollapseds [cur] False . f)
        )
    , kmLeafA_ (bind 'n') "New as parent" $ withCur $ \cur -> do
        -- Copied from 'pushInsertNewItemRelToCur' but that function can't handle "insert as parent".
        let cb name = do
              let attr = attrMinimal (zonedTimeToUTC . acZonedTime $ ?actx) name
              uuid <- liftIO nextRandom
              eok <- runExceptT $ modifyModelSync $ insertNewNormalAsParentWithNewId uuid attr cur
              case eok of
                Left _err -> return Canceled
                Right () -> do
                  let eid = EIDNormal uuid
                  zoom mtTreeViewL $ TV.moveToEID eid
                  aerContinue
        pushOverlay (newNodeOverlay "" "New Item as Parent") overlayNoop cb aerContinue
    ]

viewportKeymap :: Keymap (AppEventAction MainTree () ())
viewportKeymap =
  kmMake
    "Viewport"
    -- These all don't respect item height != 1
    -- These all respect our scrolloff without doing this manually b/c they're dominated by our
    -- visibility requests in the rendering routine.
    -- SOMEDAY maybe these should be in MainTree
    [ kmLeafA_ (bind 't') "Align to top" $ withSelIxViewportName $ \seli _vp n -> do
        setTop (viewportScroll n) seli
    , kmLeafA_ (bind 'b') "Align to bottom" $ withSelIxViewportName $ \seli vp n -> do
        let vpHeight = snd (vp ^. vpSize)
            topOff = max 0 (seli - vpHeight)
        setTop (viewportScroll n) topOff
    , kmLeafA_ (bind 'z') "Align to center" $ withSelIxViewportName $ \seli vp n -> do
        let vpHeight = snd (vp ^. vpSize)
            topOff = max 0 (seli - vpHeight `div` 2)
        setTop (viewportScroll n) topOff
    , kmLeafA_ (bind 'j') "Scroll down" $ zoom mtTreeViewL $ TV.scrollBy 3
    , kmLeafA_ (bind 'k') "Scroll up" $ zoom mtTreeViewL $ TV.scrollBy (-3)
    ]
 where
  withSelIxViewportName go = void $ runMaybeT $ do
    l <- gets (TV.tvList . mtTreeView)
    let n = getName l
    seli <- hoistMaybe (L.listSelected l)
    vp <- MaybeT $ lookupViewport n
    lift $ go seli vp n

debugKeymap :: Keymap (AppEventAction MainTree () ())
debugKeymap =
  kmMake
    "Debug"
    [ kmLeafA_ (bind 'n') "Log StatusActionabilityCounts for sel" $ withCurWithAttr $ \cura ->
        liftIO $ glogL INFO (show $ daNDescendantsByActionability . getDerivedAttr $ cura)
    ]

-- * Operations

-- ** Overlays

pushOverlay ::
  (AppComponent s a b) =>
  (AppResourceName -> s) ->
  ((?actx :: AppContext) => a -> EventM AppResourceName MainTree (AppEventReturn () ())) ->
  ((?actx :: AppContext) => b -> EventM AppResourceName MainTree (AppEventReturn () ())) ->
  ((?actx :: AppContext) => EventM AppResourceName MainTree (AppEventReturn () ())) ->
  EventM AppResourceName MainTree ()
pushOverlay mk onContinue onConfirm onCanceled = do
  hasExisting <- isJust <$> use mtOverlayL
  when hasExisting $
    let s =
          "MainTree: Pushing overlay onto an existing overlay. Old one will be thrown away. "
            ++ "This is likely not intended."
     in liftIO $ glogL WARNING s
  rootRname <- use mtResourceNameL
  let rname = Component.OverlayFor 0 rootRname
  let ol = Overlay (mk rname) onContinue onConfirm onCanceled
  mtOverlayL .= Just ol

-- ** Item manipulation

pushInsertNewItemRelToCur ::
  (?actx :: AppContext) => InsertWalker IdLabel -> EventM AppResourceName MainTree ()
pushInsertNewItemRelToCur go = do
  state <- get
  let (tgt', go') = case mtCur state of
        Just cur -> (cur, go)
        Nothing -> (mtRoot state, insLastChild)
  let cb name = do
        let attr = attrMinimal (zonedTimeToUTC . acZonedTime $ ?actx) name
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
            zoom mtTreeViewL $ TV.moveToEID eid
            aerContinue
  pushOverlay (newNodeOverlay "" "New Item") overlayNoop cb aerContinue

setStatus :: (?actx :: AppContext) => Status -> EventM n MainTree ()
setStatus status' = withCur $ \cur ->
  modifyModelAsync $
    let f = setLastStatusModified (zonedTimeToUTC $ acZonedTime ?actx) . (statusL .~ status')
     in modifyAttrByEID cur f

touchLastStatusModified :: (?actx :: AppContext) => EventM n MainTree ()
touchLastStatusModified = withCur $ \cur ->
  modifyModelAsync $
    let f = setLastStatusModified (zonedTimeToUTC $ acZonedTime ?actx)
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
moveCurRelative ::
  (?actx :: AppContext) => GoWalker LocalIdLabel -> InsertWalker IdLabel -> EventM n MainTree ()
moveCurRelative go ins = withCur $ \cur -> do
  forest <- gets (stForest . mtSubtree)
  -- The temporary follow setting makes sure we follow our item around. This is sync so that the temporary setting works. (:())
  withLensValue mtDoFollowItemL True $
    modifyModelSync_ (moveSubtreeRelFromForest cur go ins forest)

-- SOMEDAY ^^ Same applies. Also, these could all be unified.
moveCurRelativeDynamic ::
  (?actx :: AppContext) =>
  DynamicMoveWalker LocalIdLabel IdLabel ->
  EventM n MainTree ()
moveCurRelativeDynamic dgo = withCur $ \cur -> do
  forest <- gets (stForest . mtSubtree)
  withLensValue mtDoFollowItemL True $
    modifyModelSync_ (moveSubtreeRelFromForestDynamic cur dgo forest)

-- ** Model Modification

-- | Modify the model and *synchronously* pull the new model. Currently required when adding nodes.
--
-- SOMEDAY Synchronicity is a design issue really. Also, we're gonna get another ModelUpdated event
-- and then refresh a second time, which is bad.
modifyModelSync ::
  (?actx :: AppContext) =>
  ((?mue :: ModelUpdateEnv) => Model -> Model) ->
  -- This is `ExceptT IdNotFoundError (EventM n MainTree) ()` but I'm not using it that much.
  EventMOrNotFound n MainTree ()
modifyModelSync f = do
  liftIO $ do
    -- needs to be re-written when we go more async. Assumes that the model update is performed *synchronously*!
    -- SOMEDAY should we just not pull here (and thus remove everything after this) and instead rely on the ModelUpdated event?
    modifyModelOnServer (acModelServer ?actx) f
  zoom mtTreeViewL TV.reloadModel

modifyModelSync_ ::
  (?actx :: AppContext) =>
  ((?mue :: ModelUpdateEnv) => Model -> Model) ->
  EventM n MainTree ()
modifyModelSync_ f = void . notFoundToAER_ $ modifyModelSync f

reloadModel :: (?actx :: AppContext) => EventMOrNotFound AppResourceName MainTree ()
reloadModel = zoom mtTreeViewL $ TV.reloadModel

-- | Modify the model asynchronously, i.e., *without* pulling a new model immediately. We get a
-- 'ModelUpdated' event and will pull a new model then. This is fine for most applications.
--
-- NB: It's not actually async right now b/c ModelServer doesn't operate async, but it could be in
-- the future.
--
-- NB: We currently *don't* change our focus based on the modified node. This is probably ok and
-- what the user expects, but should then be reviewed, if we ever get async here.
modifyModelAsync ::
  (?actx :: AppContext) =>
  ((?mue :: ModelUpdateEnv) => Model -> Model) ->
  EventM n MainTree ()
modifyModelAsync f = liftIO $ modifyModelOnServer (acModelServer ?actx) f

-- ** Navigation

moveRootToEID :: (?actx :: AppContext) => EID -> EventMOrNotFound AppResourceName MainTree ()
moveRootToEID eid = zoom mtTreeViewL $ TV.moveRootToEID eid

-- TODO WIP
--
-- - Harden the API of TreeView a bit. Maybe hide a bunch of stuff in an export list.
-- - Check for all TODOs just introduced in this branch.
-- - Functionality test.

-- ** Filters

-- | Reset filter of the tree view to match our currently selected one here.
-- SOMEDAY this shouldn't exist. But I don't wanna wrap the clist infra around everything.
resetTreeViewFilter :: (?actx :: AppContext) => EventMOrNotFound AppResourceName MainTree ()
resetTreeViewFilter = do
  fi <- gets mtFilter
  zoom mtTreeViewL $ TV.replaceFilter fi
 where
  mtFilter :: MainTree -> Filter
  mtFilter mt = chainFilters collapseFilter normalFilter
   where
    collapseFilter = hideHierarchyFilter . mtHideHierarchyFilter $ mt
    -- NB we know that filters will be non-empty.
    normalFilter = fromJust . CList.focus . mtFilters $ mt

modifyHideHierarchyFilter ::
  (?actx :: AppContext) =>
  (HideHierarchyFilter -> HideHierarchyFilter) -> EventMOrNotFound AppResourceName MainTree ()
modifyHideHierarchyFilter f = mtHideHierarchyFilterL %= f >> resetTreeViewFilter

cycleNextFilter :: (?actx :: AppContext) => EventMOrNotFound AppResourceName MainTree ()
cycleNextFilter = do
  mtFiltersL %= CList.rotR
  resetTreeViewFilter

cyclePrevFilter :: (?actx :: AppContext) => EventMOrNotFound AppResourceName MainTree ()
cyclePrevFilter = do
  mtFiltersL %= CList.rotL
  resetTreeViewFilter

selectFilterByName ::
  (?actx :: AppContext) => String -> EventMOrNotFound AppResourceName MainTree ()
selectFilterByName s = do
  -- We don't use the following short form for updating to propagate the error right.
  -- mtFiltersL %= (fromMaybe <*> CList.findRotateTo ((== s) . fiName))
  mnewFilters <- CList.findRotateTo ((== s) . fiName) <$> gets mtFilters
  case mnewFilters of
    -- NB this isn't quite the right error but w/e
    Nothing -> throwError IdNotFoundError
    Just newFilters -> do
      mtFiltersL .= newFilters
      resetTreeViewFilter

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
renderLabelWithBreadcrumbs ztime rootLabel breadcrumbs = rootW <+> renderBreadcrumbs ztime breadcrumbs
 where
  rootW = renderLabelShort ztime rootLabel

-- NB we don't shade colors in the breadcrumbs so they "pop out", but I consider this a feature
-- rather than a bug for now.
renderBreadcrumbs :: ZonedTime -> [IdLabel] -> Widget n
renderBreadcrumbs ztime breadcrumbs =
  withAttr AppAttr.breadcrumbs $
    hBox [x | (_, lbl) <- breadcrumbs, x <- [str " < ", renderLabelShort ztime lbl]]

-- | Render a short (one-line version of the item) with dates but without status
renderLabelShort :: ZonedTime -> Label -> Widget n
renderLabelShort ztime (attr, dattr) =
  hBox
    [ str (name attr)
    , maybe
        emptyWidget
        wrapDateWidget
        (renderMostUrgentDateMaybe ztime False (dates attr) (daImpliedDates dattr))
    ]
 where
  wrapDateWidget w =
    -- SOMEDAY it's kinda stupid that this does the same calculation again as above
    let battr = mostUrgentDateAttr ztime False (dates attr) (daImpliedDates dattr)
     in hBox [str " ", withAttr battr (str "["), w, withAttr battr (str "]")]

-- Currently we only render the currently selected filter.
renderFilters :: CList.CList Filter -> Widget n
renderFilters fs = maybe emptyWidget go (CList.focus fs)
 where
  go f = withDefAttr AppAttr.filter_label $ str (fiName f)

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

renderStatusActionabilityCounts :: StatusActionabilityCounts -> Widget n
renderStatusActionabilityCounts sac =
  -- SOMEDAY There _may_ be a performance issue b/c we vary the created set of widget on selection
  -- move.
  -- SOMEDAY show Done/Canceled statuses?
  hBox . concat $
    [ intersperse sepIntraGroup
        . catMaybes
        $ [ maybeRenderIndicatorSingle WIP
          , maybeRenderIndicatorSingle Next
          , maybeRenderIndicatorSingle Waiting
          -- , maybeRenderIndicatorSingle Later
          -- , maybeRenderIndicatorSingle Open
          ]
    , [sepSingleProjects]
    , intersperse sepIntraGroup . catMaybes $
        map maybeRenderIndicatorProject displayedProjectActionabilities
          ++ [maybeStuckProjectsActionabilitiesW]
    ]
 where
  -- Not rendering Opens by actionability b/c I think that's too much information
  -- SOMEDAY alt, if n == 0, show the number and the indicator but in standard gray. Could make it less busy.
  sepMarkerCount = emptyWidget
  sepSingleProjects = str " | "
  sepIntraGroup = str " "
  maybeRenderIndicatorSingle s =
    let n = MapLike.findWithDefault 0 s . sacSingleStatuses $ sac
     in if n == 0
          then Nothing
          else
            Just $
              withAttr (actionabilityAttr False s) $
                hBox [renderStatus False s s, sepMarkerCount, str . show $ n]
  maybeRenderIndicatorProject a =
    let n = MapLike.findWithDefault 0 a . sacProjects $ sac
     in if n == 0
          then Nothing
          else
            Just $
              withAttr (actionabilityAttr False a) $
                hBox [renderStatus False Project a, sepMarkerCount, str . show $ n]
  -- Not displaying LATER b/c I think that's basically stuck and we need to prune down items.
  displayedProjectActionabilities = [WIP, Next, Waiting]
  maybeStuckProjectsActionabilitiesW =
    let
      totalProjects = MapLike.findWithDefault 0 Project $ sacSingleStatuses sac
      n = sacNStalledProjects sac
     in
      -- totalProjects
      --   - sum [MapLike.findWithDefault 0 a (sacProjects sac) | a <- displayedProjectActionabilities]

      if n == 0
        then Nothing
        else
          Just $
            withAttr (actionabilityAttr False Someday) $
              hBox [renderStatus False Project Someday, sepMarkerCount, str . show $ n]

-- * Component instance

-- | We use `Confirmed ()` to indicate that the user *asked* to exit and `Canceled ()` to indicate
-- that there was some kind of issue and the tab had to close (e.g., the parent was deleted.)
--
-- SOMEDAY that's a bit nasty.
instance AppComponent MainTree () () where
  renderComponentWithOverlays
    s@MainTree
      { mtTreeView =
        mtTreeView@(TV.TreeView {tvSubtree = Subtree {rootLabel, breadcrumbs}, tvDoFollowItem})
      , mtFilters
      , mtShowDetails
      , mtOverlay
      } =
      (box, catMaybes [ovl, detailsOvl])
     where
      -- NOT `hAlignRightLayer` b/c that breaks background colors in light mode for some reason.
      headrow =
        withDefAttr AppAttr.header_row $
          hBox
            [ padRight Max $ renderRoot now rootLabel breadcrumbs
            , str "  "
            , renderStatusActionabilityCounts . daNDescendantsByActionability . getDerivedAttr $ rootLabel
            , str "   "
            , renderFilters mtFilters
            , str " "
            , doFollowBox
            ]
      doFollowBox = withDefAttr AppAttr.follow_box $ str (if tvDoFollowItem then "(follow)" else "(keep)")
      listW = renderComponent mtTreeView
      statusBarW =
        withDefAttr AppAttr.header_row $
          padRight Max (txt " CUR" <+> selectedBreadcrumbsW) <+> statusBarRightW
      selectedBreadcrumbsW = case mtCurWithAttr s of
        Nothing -> emptyWidget
        Just illabel ->
          overrideAttr AppAttr.breadcrumbs AppAttr.header_row $
            renderBreadcrumbs (acZonedTime ?actx) (map localIdLabel2IdLabel . gLocalBreadcrumbs $ illabel)
      statusBarRightW =
        hBox
          [ maybe emptyWidget (renderStatusActionabilityCounts . daNDescendantsByActionability . getDerivedAttr) $
              mtCurWithAttr s
          , -- Spacer b/c I find it hard to read stuff at the right side of the screen
            str " "
          ]
      cmdBarW = almostEmptyWidget -- nothing here yet, just reserving some space
      -- box = headrow <=> listW <=> statusBarW <=> cmdBarW
      box = vBox [headrow, listW, statusBarW, cmdBarW]
      detailsOvl = case (mtShowDetails, mtCurWithAttr s) of
        (True, Just illabel) -> Just ("Item Details", renderItemDetails now illabel)
        _ -> Nothing
      ovl = case mtOverlay of
        Just (Overlay ol _ _ _) -> Just $ (componentTitle ol, renderComponent ol)
        Nothing -> Nothing
      now = acZonedTime ?actx

  handleEvent ev =
    -- TODO process the Tick event and update its filter b/c last-modified now depends on time (in a hacky way). - Or maybe explicitly don't and add a manual refresh??
    wrappingActions $
      case ev of
        -- Keymap
        (VtyEvent (EvKey key mods)) -> do
          keymap <- use mtKeymapL
          liftIO $ glogL DEBUG $ "handle a key from keymap"
          case kmzLookup keymap key mods of
            NotFound -> case ev of
              -- Code for keymap. We handle this here so that we can bind Backspace in a submap
              -- (and also Esc, though that's a bit too funky for my taste). NB this is a bit nasty,
              -- having some abstraction here would be good if we need it again.
              -- SOMEDAY slightly inconsistent: if the user should expect BS to always go up, we
              -- shouldn't bind it to anything else.
              (VtyEvent (EvKey KEsc [])) -> aerVoid $ mtKeymapL %= kmzResetRoot
              (VtyEvent (EvKey KBS [])) -> aerVoid $ mtKeymapL %= kmzUp
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
        (AppEvent (ModelUpdated _)) -> reloadModel
        _ -> return ()

      -- Now pass the event to either the overlay or to our main function.
      -- (these don't raise errors anymore)
      lift $ do
        mol <- use mtOverlayL
        case mol of
          Just (Overlay ol onContinue onConfirm onCanceled) -> do
            -- SOMEDAY all of this wants some abstraction I think
            (ol', res) <- nestEventM ol (handleEvent ev)
            mtOverlayL .= Just (Overlay ol' onContinue onConfirm onCanceled)
            case res of
              Continue x -> onContinue x
              Confirmed x -> mtOverlayL .= Nothing >> onConfirm x
              Canceled -> mtOverlayL .= Nothing >> onCanceled
          Nothing -> actMain
    handleFallback e = zoom mtTreeViewL $ handleEvent e

  componentKeyDesc s = case mtOverlay s of
    Nothing -> kmzDesc . mtKeymap $ s
    Just (Overlay ol _ _ _) -> (componentKeyDesc ol) {kdIsToplevel = False} -- always show key help for overlays

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
