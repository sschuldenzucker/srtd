{-# HLINT ignore "Redundant bracket" #-}
-- For overlay management
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Srtd.Components.MainTree (MainTree (..), make) where

import Brick hiding (on)
import Brick.BChan (writeBChan)
import Brick.Keybindings (Binding, bind, ctrl)
import Brick.Keybindings.KeyConfig (binding)
import Brick.Widgets.List qualified as L
import Brick.Widgets.Table
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.CircularList qualified as CList
import Data.Function (on)
import Data.List (intercalate, intersperse)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, zonedTimeToUTC, zonedTimeZone)
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as Vec
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Graphics.Vty.Input (Button (..))
import Lens.Micro.Platform
import Srtd.AppAttr
import Srtd.Attr
import Srtd.BrickHelpers
-- TODO clean up that name clash
import Srtd.Component hiding (Overlay)
import Srtd.Component qualified as Component
import Srtd.Components.Attr (
  mostUrgentDateAttr,
  renderMostUrgentDate,
  renderMostUrgentDateMaybe,
  renderPastDate,
  renderStatus,
 )
import Srtd.Components.DateSelectOverlay (dateSelectOverlay)
import Srtd.Components.NewNodeOverlay (newNodeOverlay)
import Srtd.Components.TestOverlay (newTestOverlay)
import Srtd.Data.IdTree
import Srtd.Data.TreeZipper
import Srtd.Dates (DateOrTime (..), cropDate, prettyAbsolute)
import Srtd.Keymap
import Srtd.Log
import Srtd.Model
import Srtd.ModelServer
import Srtd.Todo
import Srtd.Util
import System.Hclip (setClipboard)
import System.Process (callProcess)
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))
import Text.Wrap (WrapSettings (..), defaultWrapSettings)

type MyList = L.List AppResourceName (Int, EID, LocalLabel)

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
  }

-- | Helper to use for 'olOnContinue' or 'olOnConfirm' when you want to ignore these events.
overlayNoop :: (Monad m) => p -> m (AppEventReturn () b)
overlayNoop _ = return (Continue ())

data MainTree = MainTree
  { mtRoot :: EID
  , mtFilters :: CList.CList Filter
  , mtSubtree :: Subtree
  , mtList :: MyList
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
  }

suffixLenses ''MainTree

mtFilter :: MainTree -> Filter
mtFilter = fromJust . CList.focus . mtFilters
-- ^ NB we know that filters will be non-empty.

-- The first one is default selected.
defaultFilters :: [Filter]
defaultFilters =
  [ f_hide_completed
  , f_identity
  ]

-- * Overlay infra

-- SOMEDAY can we make this a more general thing? Then also review how specific types have to be.
-- (probably not very specific.)

-- * Keymaps

rootKeymap :: Keymap (AppEventAction MainTree () ())
rootKeymap =
  kmMake
    "Tree View"
    [ kmLeafA_ (bind 'n') "New as next sibling" $ pushInsertNewItemRelToCur insAfter
    , kmLeafA_ (bind 'N') "New as prev sibling" $ pushInsertNewItemRelToCur insBefore
    , kmLeafA_ (bind 'S') "New as first child" $ pushInsertNewItemRelToCur insFirstChild
    , kmLeafA_ (bind 's') "New as last child" $ pushInsertNewItemRelToCur insLastChild
    , ( kmLeafA_ (bind 'e') "Edit name" $ do
          state <- get
          case mtCurWithAttr state of
            Just (cur, ((curAttr, _), _)) -> do
              let oldName = name curAttr
              let cb name' = do
                    let f = setLastModified (zonedTimeToUTC . acZonedTime $ ?actx) . (nameL .~ name')
                    liftIO $ modifyModelOnServer (acModelServer ?actx) (modifyAttrByEID cur f)
                    -- NB we wouldn't need to return anything here; it's just to make the interface happy (and also the most correct approximation for behavior)
                    mtListL %= scrollListToEID cur
                    return (Continue ())
              pushOverlay (newNodeOverlay oldName "Edit Item") overlayNoop cb
            Nothing -> return ()
      )
    , ( kmLeafA_ (ctrl 't') "Open test overlay" $
          pushOverlay (const newTestOverlay) overlayNoop overlayNoop
      )
    , ( kmLeafA_ (bind 'T') "New tab" $ do
          state <- get
          liftIO $
            writeBChan (acAppChan ?actx) $
              PushTab (\rname -> SomeAppComponent $ setResourceName rname state)
      )
    , ( kmLeafA (bind 'Q') "Close tab" $
          return Canceled
      )
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
    , ( kmLeafA_ (binding KEnter []) "Hoist" $ withCur $ \cur -> do
          rname <- mtResourceName <$> get
          model <- liftIO $ getModel (acModelServer ?actx)
          filters <- mtFilters <$> get
          put $ makeWithFilters cur filters model rname
      )
    , ( kmLeafA_ (binding KBS []) "De-hoist" $ do
          s <- get
          case s ^. mtSubtreeL . breadcrumbsL of
            [] -> return ()
            (parent, _) : _ -> do
              rname <- mtResourceName <$> get
              model <- liftIO $ getModel (acModelServer ?actx)
              filters <- mtFilters <$> get
              put $ makeWithFilters parent filters model rname & mtListL %~ scrollListToEID (mtRoot s)
      )
    , (kmSub (bind ',') sortCurKeymap)
    , (kmSub (bind ';') sortRootKeymap)
    , (kmLeafA_ (bind 'h') "Go to parent" (modify (mtGoSubtreeFromCur goParent)))
    , (kmLeafA_ (bind 'J') "Go to next sibling" (modify (mtGoSubtreeFromCur goNextSibling)))
    , (kmLeafA_ (bind 'K') "Go to prev sibling" (modify (mtGoSubtreeFromCur goPrevSibling)))
    , (kmSub (bind 't') setStatusKeymap)
    , (kmSub (bind 'o') openExternallyKeymap)
    , (kmLeafA_ (bind '.') "Next filter" cycleNextFilter)
    , (kmSub (bind 'd') editDateKeymap)
    , (kmLeafA_ (bind ' ') "Toggle details overlay" (mtShowDetailsL %= not))
    , (kmLeafA_ (bind 'q') "Quit" halt)
    ]

deleteKeymap :: Keymap (AppEventAction MainTree () ())
deleteKeymap =
  kmMake
    "Delete"
    -- TOOD some undo would be nice, lol.
    [ (kmLeafA_ (bind 'D') "Subtree" $ withCur $ \cur -> modifyModel (deleteSubtree cur))
    -- (kmLeaf (bind 's') "Single" $ withCur $ \cur -> modifyModel (deleteSingle cur))
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
    , kmLeafA_ (bind 's') "Someday" (setStatus $ Someday)
    , kmLeafA_ (bind 'o') "Open" (setStatus $ Open)
    , kmLeafA_ (bind 't') "Touch" touchLastStatusModified
    ]

-- | Helper newtype to be able to pass lenses around to functions without having to enable some
-- crazy type-level hackery. (ImpredicativeTypes no bueno. RankNTypes is not enough)
newtype AttrDateOrTimeLens = AttrDateOrTimeLens {runAttrDateOrTimeLens :: Lens' Attr (Maybe DateOrTime)}

editDateKeymap :: Keymap (AppEventAction MainTree () ())
editDateKeymap =
  kmMake
    "Edit date"
    $ map mkDateEditShortcut
    $ [ (bind 'd', "Deadline", AttrDateOrTimeLens $ datesL . deadlineL)
      , (bind 'g', "Goalline", AttrDateOrTimeLens $ datesL . goallineL)
      , (bind 's', "Scheduled", AttrDateOrTimeLens $ datesL . scheduledL)
      , (bind 'r', "Remind", AttrDateOrTimeLens $ datesL . remindL)
      ]
 where
  mkDateEditShortcut (kb, label, l0 :: AttrDateOrTimeLens) = kmLeafA_ kb label $ withCurWithAttr $ \(cur, ((attr, _), _)) ->
    let cb date' = do
          let f = setLastModified (zonedTimeToUTC $ acZonedTime ?actx) . (runAttrDateOrTimeLens l0 .~ date')
          -- NB we don't *have* to use 'modifyModel' here b/c it doesn't have to be sync.
          liftIO $ modifyModelOnServer (acModelServer ?actx) (modifyAttrByEID cur f)
          mtListL %= scrollListToEID cur
          return $ Continue ()
        mkDateEdit = dateSelectOverlay (attr ^. runAttrDateOrTimeLens l0) ("Edit " <> label)
     in pushOverlay mkDateEdit overlayNoop cb

moveSubtreeModeKeymap :: Keymap (AppEventAction MainTree () ())
moveSubtreeModeKeymap =
  sticky $
    kmMake
      "Move subtree mode"
      -- SOMEDAY clean up repetition
      -- TODO WIP I think these moveSubtree (and moveSingle) things can take a cleanup with their destinations.
      -- Can we reduce the number of different options? E.g., ("next based on preorder relative to self", "next based on siblings relative to parent") - Prob think about indicating the *target* relative to sth.
      [ -- TODO put this back in. Need to implement goNextPreorder etc. Also check if this structure makes sense actually.
        -- ( kmLeaf (bind 'j') "Down" $
        --     -- withRoot $ \root -> withCur $ \cur ->
        --     --   modifyModel (moveSubtreeBelow' root cur toBeforeNextPreorder)
        --     moveCurRelative goNextPreorder insBefore
        -- ),
        -- ( kmLeaf (bind 'k') "Up" $
        --     -- withRoot $ \root -> withCur $ \cur ->
        --     --   modifyModel (moveSubtreeBelow' root cur toAfterPrevPreorder)
        --     moveCurRelative goPrevPreorder insAfter
        -- ),
        (kmLeafA_ (bind 'j') "Down" $ moveCurRelativeDynamic dtoNextPreorder)
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
          whenJust (findFirstURL name) $ \url -> liftIO (openURL url)
      )
    , ( kmLeafA_ (bind 'y') "Copy to clipboard" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) ->
          liftIO $ setClipboard name
      )
    , ( kmLeafA_ (bind 'x') "Copy first hex code" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) ->
          whenJust (findFirstHexCode name) $ \code -> liftIO (setClipboard code)
      )
    ]

sortRootKeymap :: Keymap (AppEventAction MainTree () ())
sortRootKeymap = _mkSortKeymap withRoot "Sort root by"

sortCurKeymap :: Keymap (AppEventAction MainTree () ())
sortCurKeymap = _mkSortKeymap withCur "Sort selected by"

-- | Either `withCur` or `withRoot`. Used to unify sorting.
type WithFunc =
  (?actx :: AppContext) =>
  (EID -> EventM AppResourceName MainTree ()) ->
  EventM AppResourceName MainTree ()

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
  sortFuncBy (sorter :: ((?mue :: ModelUpdateEnv) => a)) ord = withFunc $ \root -> modifyModel (sorter ord root)
  -- comparison function that puts notes first (which is what we usually want)
  compareActionabilityForSort :: Label -> Label -> Ordering
  compareActionabilityForSort l1 l2 = case (isNote l1, isNote l2) of
    (True, False) -> LT
    (True, True) -> EQ
    _ -> compare (glActionability l1) (glActionability l2)
  isNote l = (status . fst $ l) == None && glActionability l == None

-- SOMEDAY these should be moved to another module.
findFirstURL :: String -> Maybe String
findFirstURL s = listToMaybe $ getAllTextMatches (s =~ urlPattern :: AllTextMatches [] String)
 where
  urlPattern :: String
  urlPattern = "(\\b[a-z]+://[a-zA-Z0-9./?=&-_%]+)"

findFirstHexCode :: String -> Maybe String
findFirstHexCode s = listToMaybe $ getAllTextMatches (s =~ pat :: AllTextMatches [] String)
 where
  pat :: String
  pat = "0x[0-9a-fA-F]+"

openURL :: String -> IO ()
openURL url = callProcess "open" [url]

-- * Operations

pushOverlay ::
  (AppComponent s a b) =>
  (AppResourceName -> s) ->
  ((?actx :: AppContext) => a -> EventM AppResourceName MainTree (AppEventReturn () ())) ->
  ((?actx :: AppContext) => b -> EventM AppResourceName MainTree (AppEventReturn () ())) ->
  EventM AppResourceName MainTree ()
pushOverlay mk onContinue onConfirm = do
  hasExisting <- isJust <$> use mtOverlayL
  when hasExisting $
    let s =
          "MainTree: Pushing overlay onto an existing overlay. Old one will be thrown away. "
            ++ "This is likely not intended."
     in liftIO $ glogL WARNING s
  rootRname <- use mtResourceNameL
  let rname = Component.OverlayFor 0 rootRname
  let ol = Overlay (mk rname) onContinue onConfirm
  mtOverlayL .= Just ol

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
        -- NB we *have* to use 'modifyModel' here b/c that will reload the model synchronously so we
        -- find the new EID below.
        -- SOMEDAY When this is async, we should just wait for the EID to appear in a ModelUpdated
        -- message.
        modifyModel $ insertNewNormalWithNewId uuid attr tgt' go'
        let eid = EIDNormal uuid
        mtListL %= scrollListToEID eid
        return (Continue ())
  pushOverlay (newNodeOverlay "" "New Item") overlayNoop cb

setStatus :: (?actx :: AppContext) => Status -> EventM n MainTree ()
setStatus status' = withCur $ \cur ->
  modifyModel $
    let f = setLastStatusModified (zonedTimeToUTC $ acZonedTime ?actx) . (statusL .~ status')
     in modifyAttrByEID cur f

touchLastStatusModified :: (?actx :: AppContext) => EventM n MainTree ()
touchLastStatusModified = withCur $ \cur ->
  modifyModel $
    let f = setLastStatusModified (zonedTimeToUTC $ acZonedTime ?actx)
     in modifyAttrByEID cur f

cycleNextFilter :: (?actx :: AppContext) => EventM n MainTree ()
cycleNextFilter = do
  mtFiltersL %= CList.rotR
  pullNewModel

make :: EID -> Model -> AppResourceName -> MainTree
make root = makeWithFilters root (CList.fromList defaultFilters)

-- | filters must not be empty.
makeWithFilters :: EID -> CList.CList Filter -> Model -> AppResourceName -> MainTree
makeWithFilters root filters model rname =
  MainTree
    { mtRoot = root
    , mtFilters = filters
    , mtSubtree = subtree
    , mtList = list
    , mtResourceName = rname
    , mtKeymap = keymapToZipper rootKeymap
    , mtShowDetails = False
    , mtOverlay = Nothing
    }
 where
  subtree = filterRun (fromJust $ CList.focus filters) root model
  list = forestToBrickList (MainListFor rname) $ stForest subtree

forestToBrickList :: AppResourceName -> STForest -> MyList
-- TODO when we have multiple tabs, MainList should be replaced by something that will actually be unique (take as an argument)
forestToBrickList rname forest = L.list rname (Vec.fromList contents) 1
 where
  contents = map (\(lvl, (i, attr)) -> (lvl, i, attr)) $ forestFlattenWithLevels . idForest $ forest

withSelAttr :: Bool -> Widget n -> Widget n
withSelAttr True = withDefAttr selectedItemRowAttr
withSelAttr False = id

renderRow :: ZonedTime -> Bool -> (Int, a, LocalLabel) -> Widget n
renderRow
  ztime
  sel
  ( lvl
    , _
    , llabel@((Attr {name, status, dates, autoDates = AttrAutoDates {lastStatusModified}}, _), _)
    ) =
    withSelAttr sel $
      hBox $
        -- previous version. We prob don't wanna bring this back b/c it's not flexible enough (e.g., we can't fill), and it's not very complicated anyways.
        -- alignColumns [AlignLeft, AlignLeft] [2, 80] [renderMaybeStatus sel status, renderName lvl name]
        -- Ideally we'd have a table-list hybrid but oh well. NB this is a bit hard b/c of widths and partial drawing.
        -- NB the `nameW` is a bit flakey. We need to apply padding in this order, o/w some things are not wide enough.
        -- I think it's so we don't have two greedy widgets or something.
        [indentW, statusW, str " ", padRight Max nameW, str " ", dateW, str " ", lastStatusModifiedW]
   where
    -- The first level doesn't take indent b/c deadlines are enough rn.
    indentW = str (concat (replicate (lvl + 1) "    "))
    dateW = renderMostUrgentDate ztime sel dates
    lastStatusModifiedW = renderPastDate ztime sel $ cropDate (zonedTimeZone ztime) (DateAndTime lastStatusModified)
    statusW = renderStatus sel status (llActionability llabel)
    nameW = strTruncateAvailable name

-- SOMEDAY also deadline for the root (if any)?
renderRoot :: ZonedTime -> Label -> [IdLabel] -> Widget n
renderRoot ztime glabel@(rootAttr, _) breadcrumbs =
  hBox
    [statusW, str " ", pathW]
 where
  statusW = renderStatus False (status rootAttr) (glActionability glabel)
  pathW = renderBreadcrumbs ztime glabel breadcrumbs

-- SOMEDAY annoying these are two duplicate functions.

renderBreadcrumbs :: ZonedTime -> Label -> [IdLabel] -> Widget n
renderBreadcrumbs ztime (rootAttr, _) breadcrumbs = pathW
 where
  pathW = hBox $ intersperse (str " < ") . map mkBreadcrumbW $ rootAttr : map (fst . snd) breadcrumbs
  wrapBreadcrumbWidget attr w =
    let battr = mostUrgentDateAttr ztime False (dates attr)
     in hBox [str " ", withAttr battr (str "["), w, withAttr battr (str "]")]
  mkBreadcrumbW attr =
    hBox
      [ str (name attr)
      , maybe emptyWidget (wrapBreadcrumbWidget attr) (renderMostUrgentDateMaybe ztime False (dates attr))
      ]

renderBreadcrumbsOnly :: ZonedTime -> [IdLabel] -> Widget n
renderBreadcrumbsOnly ztime breadcrumbs = pathW
 where
  pathW = hBox $ intersperse (str " < ") . map mkBreadcrumbW $ map (fst . snd) breadcrumbs
  wrapBreadcrumbWidget attr w =
    let battr = mostUrgentDateAttr ztime False (dates attr)
     in hBox [str " ", withAttr battr (str "["), w, withAttr battr (str "]")]
  mkBreadcrumbW attr =
    hBox
      [ str (name attr)
      , maybe emptyWidget (wrapBreadcrumbWidget attr) (renderMostUrgentDateMaybe ztime False (dates attr))
      ]

-- Currently we only render the currently selected filter.
renderFilters :: CList.CList Filter -> Widget n
renderFilters fs = maybe emptyWidget go (CList.focus fs)
 where
  go f = withDefAttr filterLabelAttr $ str (filterName f)

renderItemDetails :: ZonedTime -> LocalIdLabel -> Widget n
renderItemDetails ztime lillabel@(eid, llabel) =
  padLeftRight 1 $
    withDefAttr (attrName "item_details") $
      vBox
        [ padBottom (Pad 1) topBox
        , -- TODO try a vertical line instead of padding. There should be some example in brick
          padBottom (Pad 1) $ hBox [padRight (Pad 5) leftBox, rightBox]
        , botBox
        ]
 where
  (label@(attr, dattr), ldattr) = llabel
  topBox =
    -- We cannot make this a table b/c `strWrap` has greedy growth and tables don't support that.
    -- NB in principle, we also don't *need* a table here but sth less general would be fine.
    vBox
      [ -- hBox [str "EID    ", str (showEIDShort eid)],
        -- hBox [str "Title  ", strWrapWith nameWrapSettings (name attr)]
        padBottom (Pad 1) $
          hBox
            [ renderStatus False (status attr) (llActionability llabel)
            , str " "
            , strWrapWith nameWrapSettings (name attr)
            ]
      , -- SOMEDAY all these conversions are pretty fucking annoying. Maybe use lenses? Proper data
        -- structures for the different *Label things?
        str "< "
          <+> renderBreadcrumbsOnly
            ztime
            (map localIdLabel2IdLabel . ldBreadcrumbs $ ldattr)
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
      , [str "Status", str (show $ status attr)]
      , [str "Actionability", str (show $ llActionability llabel)]
      , [str "Global Actionability", str (show $ glActionability label)]
      , [str "Child Actionability", str (show $ daChildActionability dattr)]
      , [str "Parent Actionability", str (show $ ldParentActionability ldattr)]
      , spacerRow
      , sectionHeaderRow "Metadata"
      ]
        ++ mkAutodatesCells "" (autoDates attr)
        ++ [spacerRow]
        ++ mkAutodatesCells "Latest " (daLatestAutodates dattr)
        ++ [spacerRow]
        ++ mkAutodatesCells "Earliest " (daEarliestAutodates dattr)
  rightBox =
    tbl $
      [sectionHeaderRow "Dates"]
        ++ mkDatesCells "" (dates attr)
        ++ [ spacerRow
           , sectionHeaderRow "Implied Dates"
           ]
        ++ mkDatesCells "" (daImpliedDates dattr)
        ++ [ spacerRow
           , sectionHeaderRow "Descendant Dates"
           ]
        ++ mkDatesCells "Earliest " (daEarliestDates dattr)
        ++ [spacerRow]
        ++ mkDatesCells "Latest" (daLatestDates dattr)
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

-- TODO WIP refactor this one to AppComponent (keybinds to AppEventAction type).
-- Then, refactor everything *around* this, specifically:
-- - We no longer need to store time just for rendering. Can pull from the implicit param
-- - Control flow should use the return of handle event
-- - Actually set ?actx somewhere I guess?!

instance AppComponent MainTree () () where
  renderComponentWithOverlays
    s@MainTree
      { mtList
      , mtSubtree = Subtree {rootLabel, breadcrumbs}
      , mtFilters
      , mtShowDetails
      , mtOverlay
      } =
      (box, catMaybes [ovl, detailsOvl])
     where
      -- NOT `hAlignRightLayer` b/c that breaks background colors in light mode for some reason.
      headrow = renderRoot now rootLabel breadcrumbs <+> (padLeft Max (renderFilters mtFilters))
      box = headrow <=> L.renderList (renderRow now) True mtList
      detailsOvl = case (mtShowDetails, mtCurWithAttr s) of
        (True, Just illabel) -> Just ("Item Details", renderItemDetails now illabel)
        _ -> Nothing
      ovl = case mtOverlay of
        Just (Overlay ol _ _) -> Just $ (componentTitle ol, renderComponent ol)
        Nothing -> Nothing
      now = acZonedTime ?actx

  handleEvent ev =
    -- LATER when filters become more fancy and filter something wrt. the current time, this *may*
    -- need to process the Tick event and update its filter. (we probably don't wanna do this on
    -- \*every* event though to keep it usable, and maybe we don't even wanna process Tick in this
    -- way. If it ever matters, manual reload may be better.)
    wrappingActions $ do
      isTopLevel <- use (mtKeymapL . to kmzIsToplevel)
      listRName <- use (mtListL . L.listNameL)
      case ev of
        -- Code for keymap. NB this is a bit nasty, having some abstraction here would be good if we need it again.
        -- We also gotta be a bit careful not to overlap these in principle.
        (VtyEvent (EvKey KEsc [])) | not isTopLevel -> mtKeymapL %= kmzResetRoot >> return (Continue ())
        (VtyEvent (EvKey KBS [])) | not isTopLevel -> mtKeymapL %= kmzUp >> return (Continue ())
        -- Map Down to vi j and Up to vi k. This also handles basic mouse wheel scroll if mouse
        -- support is not activated (see Main.hs / brick mouse example)
        -- NB we don't receive these for mouse scroll if mouse support is activated. See below.
        -- NB the following are basically modifications to list. If we need more of these, we should
        -- make a function / wrapper.
        (VtyEvent (EvKey KDown [])) -> handleEvent (VtyEvent (EvKey (KChar 'j') []))
        (VtyEvent (EvKey KUp [])) -> handleEvent (VtyEvent (EvKey (KChar 'k') []))
        -- Mouse support
        (MouseDown rname' BLeft [] (Location {loc = (_, rown)}))
          | rname' == listRName -> do
              mtListL %= L.listMoveTo rown
              return (Continue ())
        -- SOMEDAY ideally, we could actually scroll the list while keeping the selection the same
        -- (except if it would go out of bounds), but Brick lists don't provide that feature.
        -- Implementing this may be related to implementing a "scrolloff" type feature later. I
        -- probably have to reach into the List implementation for this. I'm really not quite sure
        -- how it remembers its scroll position tbh. Something with viewports and Brick's 'visible'.
        -- I *think* we can just append the right visibility request (from Brick.Types.Internal).
        (MouseDown rname' BScrollDown [] _)
          | rname' == listRName -> do
              mtListL %= L.listMoveBy 3
              return (Continue ())
        (MouseDown rname' BScrollUp [] _)
          | rname' == listRName -> do
              mtListL %= L.listMoveBy (-3)
              return (Continue ())
        -- zoom mtListL $ L.listMoveByPages (0.3 :: Double)
        -- Keymap
        (VtyEvent e@(EvKey key mods)) -> do
          keymap <- use mtKeymapL
          case kmzLookup keymap key mods of
            NotFound -> handleFallback e
            LeafResult act nxt -> do
              res <- runAppEventAction act
              mtKeymapL .= nxt
              return res
            SubmapResult sm -> mtKeymapL .= sm >> return (Continue ())
        (VtyEvent e) -> handleFallback e
        _miscEvents -> return (Continue ())
   where
    wrappingActions actMain = do
      -- <<Global updates that should happen even if there's an active overlay would go here.>>
      case ev of
        (AppEvent (ModelUpdated _)) -> pullNewModel
        _ -> return ()

      -- Now pass the event to either the overlay or to our main function.
      mol <- use mtOverlayL
      case mol of
        Just (Overlay ol onContinue onConfirm) -> do
          -- SOMEDAY all of this wants some abstraction I think
          (ol', res) <- nestEventM ol (handleEvent ev)
          mtOverlayL .= Just (Overlay ol' onContinue onConfirm)
          case res of
            Continue x -> onContinue x
            Confirmed x -> mtOverlayL .= Nothing >> onConfirm x
            Canceled -> mtOverlayL .= Nothing >> return (Continue ())
        Nothing -> actMain
    handleFallback e = do
      zoom mtListL $ L.handleListEventVi (const $ return ()) e
      return (Continue ())

  componentKeyDesc s = case mtOverlay s of
    Nothing -> kmzDesc . mtKeymap $ s
    Just (Overlay ol _ _) -> componentKeyDesc ol

  -- "Root name - Realm" unless the Realm is the root itself, or higher.
  componentTitle MainTree {mtSubtree} = T.pack $ pathStr
   where
    -- pathStr = intercalate " < " $ name (rootAttr mtSubtree) : [name attr | (_, attr) <- (breadcrumbs mtSubtree)]
    pathStr = case mRealmBreadcrumb of
      Nothing -> rootName
      Just (_, (c, _)) -> rootName ++ " - " ++ name c
    mRealmBreadcrumb = case reverse (breadcrumbs mtSubtree) of
      _ : c : _ -> Just c
      _ -> Nothing
    rootName = name (fst . rootLabel $ mtSubtree)

mtCur :: MainTree -> Maybe EID
mtCur (MainTree {mtList}) = L.listSelectedElement mtList & fmap (\(_, (_, i, _)) -> i)

mtCurWithAttr :: MainTree -> Maybe LocalIdLabel
mtCurWithAttr (MainTree {mtList}) = L.listSelectedElement mtList & fmap (\(_, (_, i, attr)) -> (i, attr))

withCur ::
  (?actx :: AppContext) => (EID -> EventM n MainTree ()) -> EventM n MainTree ()
withCur go = do
  s <- get
  case mtCur s of
    Just cur -> go cur
    Nothing -> return ()

withCurWithAttr ::
  (LocalIdLabel -> EventM n MainTree ()) -> EventM n MainTree ()
withCurWithAttr go = do
  s <- get
  case mtCurWithAttr s of
    Just cura -> go cura
    Nothing -> return ()

withRoot ::
  (?actx :: AppContext) => (EID -> EventM n MainTree ()) -> EventM n MainTree ()
withRoot go = do
  s <- get
  let root = mtRoot s
  go root

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
  forest <- use $ mtSubtreeL . stForestL
  modifyModel (moveSubtreeRelFromForest cur go ins forest)

-- SOMEDAY ^^ Same applies. Also, these could all be unified.
moveCurRelativeDynamic ::
  (?actx :: AppContext) =>
  DynamicMoveWalker LocalIdLabel IdLabel ->
  EventM n MainTree ()
moveCurRelativeDynamic dgo = withCur $ \cur -> do
  forest <- use $ mtSubtreeL . stForestL
  modifyModel (moveSubtreeRelFromForestDynamic cur dgo forest)

-- | Modify the model and *synchronously* pull the new model. Important for adding nodes.
--
-- SOMEDAY Synchronicity is a design issue really. We also don't need it often (only for adding nodes rn).
--
-- TODO make an async function and remove the updating. Also improves perf.
modifyModel ::
  (?actx :: AppContext) =>
  ((?mue :: ModelUpdateEnv) => Model -> Model) ->
  EventM n MainTree ()
modifyModel f = do
  s@(MainTree {mtRoot, mtList}) <- get
  let filter_ = mtFilter s
  model' <- liftIO $ do
    -- needs to be re-written when we go more async. Assumes that the model update is performed *synchronously*!
    -- SOMEDAY should we just not pull here (and thus remove everything after this) and instead rely on the ModelUpdated event?
    modifyModelOnServer (acModelServer ?actx) f
    getModel (acModelServer ?actx)
  let subtree = filterRun filter_ mtRoot model'
  let list' = resetListPosition mtList $ forestToBrickList (getName mtList) (stForest subtree)
  put s {mtSubtree = subtree, mtList = list'}
  return ()

pullNewModel :: (?actx :: AppContext) => EventM n MainTree ()
pullNewModel = do
  s@(MainTree {mtRoot, mtList}) <- get
  let filter_ = mtFilter s
  model' <- liftIO $ getModel (acModelServer ?actx)
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

mtGoSubtreeFromCur :: GoWalker LocalIdLabel -> MainTree -> MainTree
mtGoSubtreeFromCur go mt = fromMaybe mt mres
 where
  mres = do
    -- Maybe monad
    cur <- mtCur mt
    par <- mt ^. mtSubtreeL . stForestL . to (forestGoFromToId cur go)
    return $ mt & mtListL %~ scrollListToEID par

-- | Toplevel app resource name, including all contained resources. This is a "cloning" routine.
setResourceName :: AppResourceName -> MainTree -> MainTree
setResourceName rname state =
  state
    { mtList =
        resetListPosition (mtList state) $
          forestToBrickList (MainListFor rname) (stForest . mtSubtree $ state)
    , mtResourceName = rname
    }
