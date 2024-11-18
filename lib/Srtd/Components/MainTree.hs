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
import Brick.Widgets.Table
import Control.Monad.IO.Class (liftIO)
import Data.CircularList qualified as CList
import Data.Function (on)
import Data.List (intercalate, intersperse)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
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
import Srtd.Component
import Srtd.Components.Attr (mostUrgentDateAttr, renderMostUrgentDate, renderMostUrgentDateMaybe, renderPastDate, renderStatus)
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

data MainTree = MainTree
  { mtRoot :: EID,
    mtFilters :: CList.CList Filter,
    mtSubtree :: Subtree,
    mtList :: MyList,
    -- | Top-level resource name for this component. We can assign anything nested below (or "above") it.
    mtResourceName :: AppResourceName,
    mtZonedTime :: ZonedTime,
    mtKeymap :: KeymapZipper (AppContext -> EventM AppResourceName MainTree ()),
    -- | Whether or not to show the details view. This is not implemented as a full overlay
    -- component for simplicity.
    mtShowDetails :: Bool
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
    [ kmLeaf (bind 'n') "New as next sibling" $ pushInsertNewItemRelToCur insAfter,
      kmLeaf (bind 'N') "New as prev sibling" $ pushInsertNewItemRelToCur insBefore,
      kmLeaf (bind 'S') "New as first child" $ pushInsertNewItemRelToCur insFirstChild,
      kmLeaf (bind 's') "New as last child" $ pushInsertNewItemRelToCur insLastChild,
      ( kmLeaf (bind 'e') "Edit name" $ \ctx -> do
          state <- get
          case mtCurWithAttr state of
            Just (cur, ((curAttr, _), _)) -> do
              let oldName = name curAttr
              let cb name' (AppContext {acModelServer = acModelServer', acZonedTime = acZonedTime'}) = do
                    let f = setLastModified (zonedTimeToUTC acZonedTime') . (nameL .~ name')
                    modifyModelOnServer acModelServer' (modifyAttrByEID cur f)
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
      (kmLeaf (bind 'h') "Go to parent" (const $ modify (mtGoSubtreeFromCur goParent))),
      (kmLeaf (bind 'J') "Go to next sibling" (const $ modify (mtGoSubtreeFromCur goNextSibling))),
      (kmLeaf (bind 'K') "Go to prev sibling" (const $ modify (mtGoSubtreeFromCur goPrevSibling))),
      (kmSub (bind 't') setStatusKeymap),
      (kmSub (bind 'o') openExternallyKeymap),
      (kmLeaf (bind '.') "Next filter" cycleNextFilter),
      (kmSub (bind 'd') editDateKeymap),
      ( kmLeaf (bind ' ') "Toggle Details Overlay" (const $ mtShowDetailsL %= not)
      ),
      (kmLeaf (bind 'q') "Quit" (const halt))
    ]

deleteKeymap :: Keymap (AppContext -> EventM n MainTree ())
deleteKeymap =
  kmMake
    "Delete"
    -- TOOD some undo would be nice, lol.
    [ (kmLeaf (bind 'D') "Subtree" $ withCur $ \cur -> modifyModel (deleteSubtree cur))
    -- (kmLeaf (bind 's') "Single" $ withCur $ \cur -> modifyModel (deleteSingle cur))
    ]

setStatusKeymap :: Keymap (AppContext -> EventM n MainTree ())
setStatusKeymap =
  kmMake
    "Set Status"
    [ kmLeaf (bind ' ') "None" (setStatus None),
      kmLeaf (bind 'n') "Next" (setStatus $ Next),
      kmLeaf (bind 'w') "Waiting" (setStatus $ Waiting),
      kmLeaf (bind 'p') "Project" (setStatus $ Project),
      kmLeaf (bind 'l') "Later" (setStatus $ Later),
      kmLeaf (bind 'i') "WIP" (setStatus $ WIP),
      kmLeaf (binding KEnter []) "Done" (setStatus $ Done),
      kmLeaf (bind 's') "Someday" (setStatus $ Someday),
      kmLeaf (bind 'o') "Open" (setStatus $ Open),
      kmLeaf (bind 't') "Touch" touchLastStatusModified
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
    mkDateEditShortcut (kb, label, l) = kmLeaf kb label $ withCurWithAttr $ \(cur, ((attr, _), _)) ctx ->
      let tz = zonedTimeZone $ acZonedTime ctx
          cb date' ctx' = do
            let f = setLastModified (zonedTimeToUTC $ acZonedTime ctx) . (l .~ date')
            modifyModelOnServer (acModelServer ctx') (modifyAttrByEID cur f)
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
        ( kmLeaf (bind 'j') "Down" $ moveCurRelativeDynamic dtoNextPreorder
        ),
        ( kmLeaf (bind 'k') "Up" $ moveCurRelativeDynamic dtoPrevPreorder
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
    [ ( kmLeaf (bind 'l') "First link in name" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) _ctx ->
          whenJust (findFirstURL name) $ \url -> liftIO (openURL url)
      ),
      ( kmLeaf (bind 'y') "Copy to clipboard" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) _ctx ->
          liftIO $ setClipboard name
      ),
      ( kmLeaf (bind 'x') "Copy first hex code" $ withCurWithAttr $ \(_eid, ((Attr {name}, _), _)) _ctx ->
          whenJust (findFirstHexCode name) $ \code -> liftIO (setClipboard code)
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
      [kmLeaf (bind 't') "Status" $ sortRootBy sorter (compare `on` (view $ _1 . statusL))]
    sortRootBy sorter ord = withRoot $ \root -> modifyModel (sorter ord root)

sortCurKeymap :: Keymap (AppContext -> EventM n MainTree ())
sortCurKeymap =
  kmMake
    "Sort selected by"
    $ (kmSub (bind 'D') $ kmMake "Deep" (mkItems sortDeepBelow))
      : mkItems sortShallowBelow
  where
    mkItems sorter =
      [kmLeaf (bind 't') "Status" $ sortCurBy sorter (compare `on` (view $ _1 . statusL))]
    sortCurBy sorter ord = withCur $ \cur -> modifyModel (sorter ord cur)

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

pushInsertNewItemRelToCur :: InsertWalker IdLabel -> AppContext -> EventM n MainTree ()
pushInsertNewItemRelToCur go ctx = do
  state <- get
  let (tgt', go') = case mtCur state of
        Just cur -> (cur, go)
        Nothing -> (mtRoot state, insLastChild)
  let cb name ctx' = do
        let attr = attrMinimal (zonedTimeToUTC . acZonedTime $ ctx') name
        uuid <- nextRandom
        modifyModelOnServer (acModelServer ctx') (insertNewNormalWithNewId uuid attr tgt' go')
        return $ EIDNormal uuid
  liftIO $ writeBChan (acAppChan ctx) $ PushOverlay (SomeBrickComponent . newNodeOverlay cb "" "New Item")

setStatus :: Status -> AppContext -> EventM n MainTree ()
setStatus status' = withCur $ \cur ->
  modifyModelWithCtx $ \ctx ->
    let f = setLastStatusModified (zonedTimeToUTC $ acZonedTime ctx) . (statusL .~ status')
     in modifyAttrByEID cur f

touchLastStatusModified :: AppContext -> EventM n MainTree ()
touchLastStatusModified = withCur $ \cur ->
  modifyModelWithCtx $ \ctx ->
    let f = setLastStatusModified (zonedTimeToUTC $ acZonedTime ctx)
     in modifyAttrByEID cur f

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
      mtKeymap = keymapToZipper rootKeymap,
      mtShowDetails = False
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
renderRow ztime sel (lvl, _, llabel@((Attr {name, status, dates, autoDates = AttrAutoDates {lastStatusModified}}, _), _)) =
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
    pathW = hBox $ intersperse (str " < ") . map mkBreadcrumbW $ rootAttr : map (fst . snd) breadcrumbs
    wrapBreadcrumbWidget attr w =
      let battr = mostUrgentDateAttr ztime False (dates attr)
       in hBox [str " ", withAttr battr (str "["), w, withAttr battr (str "]")]
    mkBreadcrumbW attr =
      hBox
        [ str (name attr),
          maybe emptyWidget (wrapBreadcrumbWidget attr) (renderMostUrgentDateMaybe ztime False (dates attr))
        ]

-- Currently we only render the currently selected filter.
renderFilters :: CList.CList Filter -> Widget n
renderFilters fs = maybe emptyWidget go (CList.focus fs)
  where
    go f = withDefAttr filterLabelAttr $ str (filterName f)

renderItemDetails :: ZonedTime -> LocalIdLabel -> Widget n
renderItemDetails ztime (eid, llabel) =
  padLeftRight 1 $
    withDefAttr (attrName "item_details") $
      vBox
        [ padBottom (Pad 1) topBox,
          -- TODO try a vertical line instead of padding. There should be some example in brick
          padBottom (Pad 1) $ hBox [padRight (Pad 5) leftBox, rightBox],
          botBox
        ]
  where
    (label@(attr, dattr), ldattr) = llabel
    topBox =
      -- We cannot make this a table b/c `strWrap` has greedy growth and tables don't support that.
      -- NB in principle, we also don't *need* a table here but sth less general would be fine.
      vBox
        [ -- hBox [str "EID    ", str (showEIDShort eid)],
          -- hBox [str "Title  ", strWrapWith nameWrapSettings (name attr)]
          hBox [strWrapWith nameWrapSettings (name attr)]
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
        [ sectionHeaderRow "Status",
          [str "Status", str (show $ status attr)],
          [str "Actionability", str (show $ llActionability llabel)],
          [str "Global Actionability", str (show $ glActionability label)],
          [str "Child Actionability", str (show $ daChildActionability dattr)],
          [str "Parent Actionability", str (show $ ldParentActionability ldattr)],
          spacerRow,
          sectionHeaderRow "Metadata"
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
          ++ [ spacerRow,
               sectionHeaderRow "Descendant Dates"
             ]
          ++ mkDatesCells "Earliest " (daEarliestDates dattr)
          ++ [spacerRow]
          ++ mkDatesCells "Latest" (daLatestDates dattr)
    sectionHeaderRow s = [withAttr sectionHeaderAttr (str s), emptyWidget]
    spacerRow = [str " ", emptyWidget]
    mkAutodatesCells prefix ad =
      [ [str (prefix ++ "Created"), renderUTCTime (created ad)],
        [str (prefix ++ "Modified"), renderUTCTime (lastModified ad)],
        [str (prefix ++ "Status Modified"), renderUTCTime (lastStatusModified ad)]
      ]
    mkDatesCells prefix ds =
      [ [str (prefix ++ "Deadline"), renderMDate (deadline ds)],
        [str (prefix ++ "Goalline"), renderMDate (goalline ds)],
        [str (prefix ++ "Scheduled"), renderMDate (scheduled ds)],
        [str (prefix ++ "Remind"), renderMDate (remind ds)]
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

instance BrickComponent MainTree where
  renderComponentWithOverlays s@MainTree {mtList, mtSubtree = Subtree {rootLabel, breadcrumbs}, mtFilters, mtZonedTime, mtShowDetails} =
    (box, ovls)
    where
      -- NOT `hAlignRightLayer` b/c that breaks background colors in light mode for some reason.
      headrow = renderRoot mtZonedTime rootLabel breadcrumbs <+> (padLeft Max (renderFilters mtFilters))
      box = headrow <=> L.renderList (renderRow mtZonedTime) True mtList
      ovls = case (mtShowDetails, mtCurWithAttr s) of
        (True, Just illabel) -> [("Item Details", renderItemDetails mtZonedTime illabel)]
        _ -> []

  handleEvent ctx ev =
    -- LATER when filters become more fancy and filter something wrt. the current time, this *may*
    -- need to process the Tick event and update its filter. (we probably don't wanna do this on
    -- \*every* event though to keep it usable, and maybe we don't even wanna process Tick in this
    -- way. If it ever matters, manual reload may be better.)
    updateZonedTime >> do
      isTopLevel <- use (mtKeymapL . to kmzIsToplevel)
      listRName <- use (mtListL . L.listNameL)
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
        -- Map Down to vi j and Up to vi k. This also handles basic mouse wheel scroll if mouse
        -- support is not activated (see Main.hs / brick mouse example)
        -- NB we don't receive these for mouse scroll if mouse support is activated. See below.
        -- NB the following are basically modifications to list. If we need more of these, we should
        -- make a function / wrapper.
        (VtyEvent (EvKey KDown [])) -> handleEvent ctx (VtyEvent (EvKey (KChar 'j') []))
        (VtyEvent (EvKey KUp [])) -> handleEvent ctx (VtyEvent (EvKey (KChar 'k') []))
        -- Mouse support
        (MouseDown rname' BLeft [] (Location {loc = (_, rown)}))
          | rname' == listRName ->
              mtListL %= L.listMoveTo rown
        -- SOMEDAY ideally, we could actually scroll the list while keeping the selection the same
        -- (except if it would go out of bounds), but Brick lists don't provide that feature.
        -- Implementing this may be related to implementing a "scrolloff" type feature later. I
        -- probably have to reach into the List implementation for this. I'm really not quite sure
        -- how it remembers its scroll position tbh. Something with viewports and Brick's 'visible'.
        -- I *think* we can just append the right visibility request (from Brick.Types.Internal).
        (MouseDown rname' BScrollDown [] _)
          | rname' == listRName ->
              mtListL %= L.listMoveBy 3
        (MouseDown rname' BScrollUp [] _)
          | rname' == listRName ->
              mtListL %= L.listMoveBy (-3)
        -- zoom mtListL $ L.listMoveByPages (0.3 :: Double)
        -- Keymap
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

withCur :: (EID -> AppContext -> EventM n MainTree ()) -> AppContext -> EventM n MainTree ()
withCur go ctx = do
  s <- get
  case mtCur s of
    Just cur -> go cur ctx
    Nothing -> return ()

withCurWithAttr :: (LocalIdLabel -> AppContext -> EventM n MainTree ()) -> AppContext -> EventM n MainTree ()
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
moveCurRelative :: GoWalker LocalIdLabel -> InsertWalker IdLabel -> AppContext -> EventM n MainTree ()
moveCurRelative go ins = withCur $ \cur ctx -> do
  forest <- use $ mtSubtreeL . stForestL
  modifyModel (moveSubtreeRelFromForest cur go ins forest) ctx

-- SOMEDAY ^^ Same applies. Also, these could all be unified.
moveCurRelativeDynamic :: DynamicMoveWalker LocalIdLabel IdLabel -> AppContext -> EventM n MainTree ()
moveCurRelativeDynamic dgo = withCur $ \cur ctx -> do
  forest <- use $ mtSubtreeL . stForestL
  modifyModel (moveSubtreeRelFromForestDynamic cur dgo forest) ctx

modifyModel :: (Model -> Model) -> AppContext -> EventM n MainTree ()
modifyModel f = modifyModelWithCtx (const f)

modifyModelWithCtx :: (AppContext -> Model -> Model) -> AppContext -> EventM n MainTree ()
modifyModelWithCtx f ctx@(AppContext {acModelServer}) = do
  s@(MainTree {mtRoot, mtList}) <- get
  let filter_ = mtFilter s
  model' <- liftIO $ do
    -- needs to be re-written when we go more async. Assumes that the model update is performed *synchronously*!
    -- SOMEDAY should we just not pull here (and thus remove everything after this) and instead rely on the ModelUpdated event?
    modifyModelOnServer acModelServer (f ctx)
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
    { mtList = resetListPosition (mtList state) $ forestToBrickList (MainListFor rname) (stForest . mtSubtree $ state),
      mtResourceName = rname
    }
