{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{-| A view component for a 'Subtree'.

To be imported qualified.
-}
module Srtd.Components.TreeView (
  -- * Types

  -- NB we do export 'TreeView's constructor & all fields as a matter of convenience but you
  -- shouldn't use them to update state. Only use the exported lenses and the setter functions from
  -- below.
  TreeView (..),
  SearchDirection (..),

  -- ** Lenses

  -- These are safe to use; unsafe lenses are not exposed.
  tvSearchRxL,
  tvDoFollowItemL,
  tvScrolloffL,

  -- * Construction
  makeFromModel,
  setResourceName,
  moveRootToEID,

  -- * Access
  tvCur,
  tvCurWithAttr,

  -- * Modification

  -- ** Modfying Position and Scrolling
  moveBy,
  moveToBeginning,
  moveToEnd,
  moveToIndex,
  moveToEID,
  moveGoWalkerFromCur,
  searchForRxAction,
  searchForRxSiblingAction,
  scrollBy,

  -- ** Updating Data
  reloadModel,
  replaceFilter,
) where

import Brick hiding (on)
import Brick.Widgets.List qualified as L
import Control.Applicative (asum)
import Control.Arrow (Arrow (second))
import Control.Monad (void)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.List (maximumBy, minimumBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Time (ZonedTime (..))
import Data.Vector qualified as Vec
import Graphics.Vty (Button (..), Event (..), Key (..))
import Graphics.Vty.Attributes qualified as VtyAttr
import Lens.Micro.Platform (use, (%=), (&), (.=), (.~), (<&>), (^.))
import Srtd.AppAttr qualified as AppAttr
import Srtd.Attr hiding (Canceled)
import Srtd.BrickAttrHelpers (combineAttrs)
import Srtd.BrickHelpers (pattern SomeMouse)
import Srtd.BrickListHelpers qualified as L
import Srtd.Component
import Srtd.Components.Attr (renderLastModified, renderMostUrgentDate, renderStatus)
import Srtd.Components.CompilingTextEntry (CompiledWithSource (..))
import Srtd.Data.IdTree (IdForest (..), forestGoFromToId, zForestFindId, zGetId)
import Srtd.Data.TreeZipper
import Srtd.Dates (DateOrTime (..), cropDate)
import Srtd.Keymap (KeyDesc (..))
import Srtd.Model (
  Filter,
  IdNotFoundError,
  Model,
  STForest,
  Subtree (..),
  runFilter,
  stParentEID,
 )
import Srtd.ModelServer (getModel)
import Srtd.MonadBrick
import Srtd.ProactiveBandana
import Srtd.Util (
  for,
  forestFlattenToList,
  regexSplitsWithMatchesOverlap,
  urlRegex,
 )
import Text.Regex.TDFA (RegexLike (..))
import Text.Regex.TDFA.Common (Regex)

-- * Type of list items

data ListIdLabel = ListIdLabel
  { lilLvl :: Int
  , lilEID :: EID
  , lilLocalLabel :: LocalLabel
  }

suffixLenses ''ListIdLabel

listIdLabel2LocalIdLabel :: ListIdLabel -> LocalIdLabel
listIdLabel2LocalIdLabel itm = (lilEID itm, lilLocalLabel itm)

instance HasAttr ListIdLabel where getAttrL = lilLocalLabelL . getAttrL

instance HasDerivedAttr ListIdLabel where getDerivedAttrL = lilLocalLabelL . getDerivedAttrL

instance HasLocalDerivedAttr ListIdLabel where
  getLocalDerivedAttrL = lilLocalLabelL . getLocalDerivedAttrL

instance HasEID ListIdLabel where getEIDL = lilEIDL

type TreeViewList = L.List AppResourceName ListIdLabel

-- * Other types (have to come early b/c TemplateHaskell)

-- | Cascading list of attrs that are applied on top of each other
--
-- SOMEDAY explore what else we can do with this
newtype CSAttr = CSAttr [AttrName] deriving (Semigroup, Monoid)

csAttrName :: AttrName -> CSAttr
csAttrName aname = CSAttr [aname]

-- | Use a combined `CSAttr` as the default attr.
--
-- Components are combined in order, with earlier ("leftmost") ones dominating later ones.
withDefCSAttr :: CSAttr -> Widget n -> Widget n
withDefCSAttr (CSAttr anames) = updateAttrMap $ \amap ->
  let
    emptyAttr :: VtyAttr.Attr
    emptyAttr = VtyAttr.Attr VtyAttr.KeepCurrent VtyAttr.KeepCurrent VtyAttr.KeepCurrent VtyAttr.KeepCurrent
    -- We set the default attr to the empty attr so that lookups into the attrmap below aren't
    -- combined with it, which would *always* give them a color and prevent combining style with
    -- color. It's kinda stupid that we don't just have a function to do these "raw" lookups into
    -- attrmap.
    --
    -- SOMEDAY This solves about 50% of the problem. Example:
    -- `url` indeed just sets style, so ordering doesn't matter when it's combined with `text_match`.
    -- But `selected.url` also sets color, so ordering suddenly matters when it's combined with `selected.text_match`.
    --
    -- Another instance where Brick's attr hierarchy is really weird and constraining at times.
    --
    -- SOMEDAY maybe these things should be handled using a different mechanism. There's a way to
    -- apply an `Attr -> Attr` function directly in brick.
    amap' = setDefaultAttr emptyAttr amap
    newDefaultAttr =
      foldr (flip combineAttrs) (getDefaultAttr amap) [attrMapLookup aname amap' | aname <- anames]
   in
    setDefaultAttr newDefaultAttr amap

-- * Main Type

-- TODO WIP this is one of the big places where proactive stuff will be actually useful.
-- Design where to use it. E.g., how deeply should we reach into the model stuff? (prob yes, also filter)

-- | An 'AppComponent' to view a 'Subtree' as a scrollable, movable list.
--
-- We only _display_ the tree and handle movement and scrolling. The parent has to implement any
-- kind of operations (e.g., modification)
--
-- Be careful updating fields and using lenses. Some operations require updates not reflected here.
--
-- SOMEDAY and maybe the constructor should be private b/c of this.
data TreeView = TreeView
  { tvSubtree :: Cell' Subtree (AppEventM TreeView ())
  -- ^ NOT safe to edit directly
  , tvFilter :: Cell (Filter, AppContext) (AppEventMOrNotFound TreeView ()) Filter
  -- ^ NOT safe to edit directly. The type is a bit funky b/c changes to the filter have to reload the model b/c it's not stored anywhere here, which may fail.
  -- SOMEDAY maybe just store the model. It's just a pointer!
  , tvList :: TreeViewList
  -- ^ Safe to edit directly to the same degree that 'L.List' is.
  , tvResourceName :: AppResourceName
  -- ^ NOT safe to edit directly
  , tvSearchRx :: Maybe (CompiledWithSource Regex)
  -- ^ safe to edit directly
  , tvDoFollowItem :: Bool
  -- ^ safe to edit directly
  , tvScrolloff :: Int
  -- ^ safe to edit directly
  -- TODO config options whether to show the two leading columns.
  }

suffixLenses ''TreeView

-- * API

makeFromModel ::
  (?actx :: AppContext) =>
  EID ->
  Filter ->
  Bool ->
  Int ->
  Model ->
  AppResourceName ->
  Either IdNotFoundError TreeView
makeFromModel root fi doFollowItem scrolloff model rname = do
  subtree <-
    translateAppFilterContext $
      runFilter fi root model
  let list = forestToBrickList (rname <> "brick list") $ stForest subtree
  return
    TreeView
      { tvSubtree = simpleCell subtree $ replaceSubtree'
      , tvFilter = simplePreMappingCell fi fst $ \(_fi', actx) ->
          -- Enforce late binding of ?actx in reloadModel.
          -- Without this contruction, ?actx would bind in 'makeFromModel', which is ok practically
          -- but not in principle, and could be an issue elsewhere when we read, e.g., time.
          -- SOMEDAY this is kinda unhinged, would be better to either use the correct type (needs a
          -- warpper though) or just not use implicit parameters. They are really tricky.
          let ?actx = actx
           in reloadModel
      , tvList = list
      , tvResourceName = rname
      , tvSearchRx = Nothing
      , tvDoFollowItem = doFollowItem
      , tvScrolloff = scrolloff
      }

replaceFilter :: (?actx :: AppContext) => Filter -> AppEventMOrNotFound TreeView ()
replaceFilter fi = runUpdateLens tvFilterL (fi, ?actx)

setResourceName :: AppResourceName -> TreeView -> TreeView
setResourceName rname =
  (tvResourceNameL .~ rname)
    . (tvListL . L.listNameL .~ (rname <> "brick list"))

-- | Move the tree to any EID, preserving settings. Returns an error if that EID doesn't exist
-- (presumably b/c it was deleted). Then nothing is updated.
moveRootToEID ::
  (?actx :: AppContext, MonadIO m, MonadState TreeView m) => EID -> ExceptT IdNotFoundError m ()
moveRootToEID eid = do
  -- NB this constructs a new TreeView, so we don't need to update any cells b/c they're gone
  -- anyways.
  -- SOMEDAY to be reconsidered depending on meaningful events we may emit.
  tv <- get
  model <- liftIO $ getModel (acModelServer ?actx)
  -- NB we can re-use the resource name b/c we're updating ourselves
  tv' <-
    liftEither $
      makeFromModel
        eid
        (cValue $ tvFilter tv)
        (tvDoFollowItem tv)
        (tvScrolloff tv)
        model
        (tvResourceName tv)
  put tv'

-- | Currently selected item
tvCur :: TreeView -> Maybe EID
tvCur = fmap fst . tvCurWithAttr

-- | Currently selected item with label
tvCurWithAttr :: TreeView -> Maybe LocalIdLabel
tvCurWithAttr TreeView {tvList} = L.listSelectedElement tvList & fmap (\(_, itm) -> listIdLabel2LocalIdLabel itm)

-- * Component Instance

instance AppComponent TreeView where
  type Return TreeView = ()

  -- TODO we want some events I think
  type Event TreeView = ()

  renderComponent s = Widget Greedy Greedy $ do
    c <- getContext
    -- NB this doesn't quite work as expected, but it avoids a situation where the selected row
    -- wouldn't be visible anymore, which is all we need for now.
    let canFitScrolloff = availHeight c >= tvScrolloff s
        mseli = if canFitScrolloff then (L.listSelected $ tvList s) else Nothing
    render $
      L.renderListWithIndex
        (renderRow now (fmap cwsCompiled . tvSearchRx $ s) (tvScrolloff s) mseli)
        True
        (tvList s)
   where
    now = acZonedTime ?actx

  -- TODO process the Tick event and update its filter b/c last-modified now depends on time (in a hacky way). - Or maybe explicitly don't and add a manual refresh??
  handleEvent ev = do
    listRName <- gets (L.listName . tvList)
    case ev of
      AppEvent (ModelUpdated _) -> notFoundToAER_ reloadModel
      AppEvent Tick -> return Continue
      (VtyEvent (EvKey KDown [])) -> aerVoid $ liftEventM $ moveBy 1
      (VtyEvent (EvKey KUp [])) -> aerVoid $ liftEventM $ moveBy (-1)
      (MouseDown rname' BLeft [] (Location {loc = (_, rown)}))
        | rname' == listRName -> aerVoid $ liftEventM $ moveToIndex rown
      (MouseDown rname' BScrollDown [] _)
        | rname' == listRName -> aerVoid $ scrollBy 3
      (MouseDown rname' BScrollUp [] _)
        | rname' == listRName -> aerVoid $ scrollBy (-3)
      (VtyEvent e) -> aerVoid $ liftEventM $ zoom tvListL $ L.handleListEventVi L.handleListEvent e
      SomeMouse -> return Continue

  -- TODO Dummy for now
  componentKeyDesc s = KeyDesc {kdName = componentTitle s, kdIsToplevel = True, kdPairs = []}

  -- Dummy for now.
  componentTitle _ = "Tree View"

-- * Rendering

renderRow :: ZonedTime -> Maybe Regex -> Int -> Maybe Int -> Int -> Bool -> ListIdLabel -> Widget n
renderRow
  ztime
  mrx
  scrolloff
  mseli
  i
  sel
  -- SOMEDAY these can be made way simpler using universal accessors (`g*`)
  ( ListIdLabel
      lvl
      _
      llabel@( ( Attr {name, status, dates, autoDates = AttrAutoDates {}}
                 , DerivedAttr {daLatestAutodates}
                 )
               , _
               )
    ) =
    visi $
      withSelAttr sel $
        hBox $
          -- previous version. We prob don't wanna bring this back b/c it's not flexible enough (e.g., we can't fill), and it's not very complicated anyways.
          -- alignColumns [AlignLeft, AlignLeft] [2, 80] [renderMaybeStatus sel status, renderName lvl name]
          -- Ideally we'd have a table-list hybrid but oh well. NB this is a bit hard b/c of widths and partial drawing.
          -- NB the `nameW` is a bit flakey. We need to apply padding in this order, o/w some things are not wide enough.
          -- I think it's so we don't have two greedy widgets or something.
          -- TODO attr management really broken here, and I'm not sure how I'd fix.
          [ withDefAttr (maybePrefixSelAttr sel $ attrName "dates_column") . hBox $
              [ lastStatusModifiedW
              , str " "
              , dateW
              ]
          , indentW
          , collapsedW
          , statusW
          , str " "
          , padRight Max nameW
          ]
   where
    -- Scrolloff
    visi = case mseli of
      Nothing -> id -- either nothing selected OR window too small
      Just seli ->
        if seli - scrolloff <= i && i <= seli + scrolloff then visible else id

    -- The first level doesn't take indent b/c deadlines are enough rn.
    indentW = str (concat (replicate lvl "    "))
    collapsedW =
      -- NB the symbol means that something is hidden; this is subtly different form ldIsCollapsed.
      if (ldHiddenChildren . getLocalDerivedAttr $ llabel) > 0
        -- if (ldIsCollapsed . getLocalDerivedAttr $ llabel)
        -- SOMEDAY not sure if this is the prettiest character, here are some alternatives:
        then withDefAttr AppAttr.collapsed_marker (str "▸")
        -- then withDefAttr collapsed_marker (str "›")
        -- then withDefAttr collapsed_marker (str "►")
        -- then withDefAttr collapsed_marker (str "▷")
        else str " "
    -- EXPERIMENTAL: Show also implied dates "upwards" from children.
    -- SOMEDAY It might be confusing if the implied date comes from children or parent, maybe use different style or marker
    -- was: theDates = daImpliedDates
    theDates = gEarliestImpliedOrChildDates (zonedTimeZone ztime) llabel
    dateW = renderMostUrgentDate ztime sel dates theDates
    -- EXPERIMENTAL: Show *latest* lastStatusModified including children, not just of the item itself.
    lastStatusModifiedW =
      renderLastModified ztime sel $
        cropDate (zonedTimeZone ztime) (DateAndTime (lastStatusModified daLatestAutodates))
    statusW = renderStatus sel status (gLocalActionability llabel)
    regexHighlights =
      catMaybes
        -- SOMEDAY Order matters here, unfortunately:
        -- The text match has to take priority b/c it changes color and the URL only changes style.
        -- This is not gonna work if a theme flips these around.
        -- See `withDefCSAttr` for a known bug.
        [ mrx <&> (,(attrName "text_match"))
        , Just (urlRegex, (attrName "url"))
        ]
    regexHighlightsWithSel =
      for regexHighlights $
        second $
          csAttrName . if sel then (attrName "selected" <>) else id
    nameW =
      let chunksMatches = regexSplitsWithMatchesOverlap regexHighlightsWithSel (T.pack name)
          chunks = for chunksMatches $ \(csa, s) -> withDefCSAttr csa (txt s)
       in hBox chunks

withSelAttr :: Bool -> Widget n -> Widget n
withSelAttr = withDefAttrIf AppAttr.selected_item_row

withDefAttrIf :: AttrName -> Bool -> Widget n -> Widget n
withDefAttrIf a True = withDefAttr a
withDefAttrIf _ False = id

-- TODO this function shouldn't exist. Instead proper inheritance.
maybePrefixSelAttr :: Bool -> AttrName -> AttrName
maybePrefixSelAttr True a = AppAttr.selected_item_row <> a
maybePrefixSelAttr False a = a

-- * Event Handling

moveToIndex :: (MonadState TreeView m) => Int -> m ()
moveToIndex i = tvListL %= L.listMoveTo i

moveToBeginning :: (MonadState TreeView m) => m ()
moveToBeginning = tvListL %= L.listMoveToBeginning

moveToEnd :: (MonadState TreeView m) => m ()
moveToEnd = tvListL %= L.listMoveToEnd

moveBy :: (MonadState TreeView m) => Int -> m ()
moveBy n = tvListL %= L.listMoveBy n

moveGoWalkerFromCur :: (MonadState TreeView m) => GoWalker LocalIdLabel -> m ()
moveGoWalkerFromCur go = void . runMaybeT $ do
  cur <- MaybeT $ gets tvCur
  par <- MaybeT $ forestGoFromToId cur go . stForest <$> gets (cValue . tvSubtree)
  lift $ moveToEID par

-- | Move to a specified EID. Does nothing if the EID is not found.
moveToEID :: (MonadState TreeView m) => EID -> m ()
moveToEID eid = tvListL %= moveListToEID eid

moveListToEID :: EID -> TreeViewList -> TreeViewList
moveListToEID eid = L.listFindBy $ \itm -> lilEID itm == eid

scrollBy :: (AppMonadBrick TreeView m) => Int -> m ()
scrollBy n = do
  scrolloff <- gets tvScrolloff
  liftEventM $ zoom tvListL $ listScrollMoveBy scrolloff n

-- | Scroll a list by the specified amount, moving the selection when we reach the top/bottom.
--
-- TODO Move to BrickHelpers
listScrollMoveBy ::
  (Ord n) =>
  -- | Scrolloff
  Int ->
  -- | Amount to scroll by
  Int ->
  EventM n (L.List n e) ()
listScrollMoveBy scrolloffIn n = void . runMaybeT $ do
  li <- get
  let name = getName li
  vp <- MaybeT $ lookupViewport name
  seli <- hoistMaybe (L.listSelected li)
  lift $ do
    let
      viewTop = vp ^. vpTop
      viewHeight = vp ^. vpSize & snd
      viewBottom = viewTop + viewHeight - 1
      listHeight = Vec.length . L.listElements $ li
      listBottom = listHeight - 1
      -- Decision taken in render routine, see there
      -- SOMEDAY just like in the render routine, I feel this doesn't work quite precisely.
      scrolloff = if (viewHeight >= scrolloffIn) then scrolloffIn else 0
      -- Actual scroll amount, taking into account that we stop at the top/bottom of the list.
      n' =
        minimumBy
          (comparing abs)
          [ max 0 (viewTop + n) - viewTop
          , min listBottom (viewBottom + n) - viewBottom
          ]
      -- Amount we need to move the selection in case we'd cross it with the top/bottom of the
      -- view by scrolling.
      -- If we don't move the selection ourselves, scrolling doesn't either, and visibility
      -- constraints make it so nothing scrolls actually.
      moveAmount =
        maximumBy
          (comparing abs)
          [ max 0 (viewTop + n' + scrolloff - seli)
          , min 0 (viewBottom + n' - scrolloff - seli)
          ]
    put $ L.listMoveBy moveAmount li
    vScrollBy (viewportScroll name) n'

-- | (Try to) reload the model. This is almost never needed externally b/c we receive a ModelUpdated
-- event anyways.
--
-- Only call this externally if for some reason you need to update the TreeView early, synchronously
-- with something else.
reloadModel :: (?actx :: AppContext) => AppEventMOrNotFound TreeView ()
reloadModel = do
  s <- get
  let filter_ = cValue $ tvFilter s
  let root_ = root . cValue . tvSubtree $ s
  model' <- liftIO $ getModel (acModelServer ?actx)
  subtree <- liftEither $ translateAppFilterContext $ runFilter filter_ root_ model'
  lift $ runUpdateLens tvSubtreeL subtree

replaceSubtree' :: Subtree -> AppEventM TreeView ()
replaceSubtree' subtree = do
  old <- get
  let list' = forestToBrickList (getName . tvList $ old) (stForest subtree)
  tvListL .= list'
  liftEventM $ resetListPosition (tvDoFollowItem old) old

-- | Choose between follow or no-follow using a flag
resetListPosition :: (MonadState TreeView m) => Bool -> TreeView -> m ()
resetListPosition True = resetListPositionFollow
resetListPosition False = resetListPositionIndex

-- | Tries preserve the list position by index  only.
-- TODO needed?? This is the default behavior I think
resetListPositionIndex :: (MonadState TreeView m) => TreeView -> m ()
resetListPositionIndex old = case L.listSelectedElement (tvList old) of
  Nothing -> return ()
  Just (ix_, _) -> moveToIndex ix_

-- | `resetListPosition old new` tries to set the position of `new` to the current element of `old`, prioritizing the EID or, if that fails, the siblings or, then parents or, then the current position.
-- EXPERIMENTAL. This may not always be desired actually.
resetListPositionFollow :: (MonadState TreeView m) => TreeView -> m ()
resetListPositionFollow old =
  gets tvList >>= \new -> case L.listSelectedElement (tvList old) of
    Nothing -> return () -- `old` is empty
    Just (ix_, tgtItm) ->
      let
        selEID = gEID tgtItm
        selz = zForestFindId selEID (stForest . cValue . tvSubtree $ old)
        tgtEIDs =
          catMaybes $
            (Just selEID)
              : (fmap zGetId . goNextSibling =<< selz)
              : (fmap zGetId . goPrevSibling =<< selz)
              : map (Just . gEID) (gLocalBreadcrumbs tgtItm)
       in
        -- Try to find the previously selected element or a parent.
        asum (map (\eid -> tryGoToEID eid new) tgtEIDs)
          -- If we can't find
          & fromMaybe (moveToIndex ix_)
 where
  tryGoToEID eid new =
    Vec.findIndex (\itm -> lilEID itm == eid) (L.listElements new) <&> \ix' ->
      moveToIndex ix'

forestToBrickList :: AppResourceName -> STForest -> TreeViewList
forestToBrickList rname forest = L.list rname (Vec.fromList contents) 1
 where
  contents =
    map (\(i, attr) -> ListIdLabel (gLocalLevel attr) i attr) . forestFlattenToList . idForest $ forest

-- ** Search

data SearchDirection = Forward | Backward

searchForRxAction :: (MonadState TreeView m) => SearchDirection -> Bool -> m ()
searchForRxAction dir curOk = do
  mrx <- use tvSearchRxL
  case mrx of
    Nothing -> return ()
    Just rx -> tvListL %= searchForRx dir curOk (cwsCompiled rx)

-- SOMEDAY if this is slow, we might instead go via the tree. Note that this has wrap-around, though.
searchForRxSiblingAction :: (MonadState TreeView m) => SearchDirection -> m ()
searchForRxSiblingAction dir = do
  mrx <- use tvSearchRxL
  mCurAttr <- gets tvCurWithAttr
  st <- gets (cValue . tvSubtree)
  case (mrx, mCurAttr) of
    (Just rx, Just (_i, curllabel)) ->
      let curpar = stParentEID st curllabel
       in tvListL %= searchForRxNextSibling dir (cwsCompiled rx) curpar st
    _ -> return ()
 where
  searchForRxNextSibling Forward rx curpar st = L.listFindBy (p rx curpar st)
  searchForRxNextSibling Backward rx curpar st = L.listFindBackwardsBy (p rx curpar st)
  p rx curpar st itm = stParentEID st itm == curpar && nameMatchesRx rx itm

-- | Usage: `searchForRx direction doNothingIfCurrentMatches`
-- TODO should be part of TreeView's API
searchForRx :: SearchDirection -> Bool -> Regex -> TreeViewList -> TreeViewList
searchForRx _ True rx l | matchesRxCurrent rx l = l
searchForRx Forward _ rx l = L.listFindBy (nameMatchesRx rx) l
searchForRx Backward _ rx l = L.listFindBackwardsBy (nameMatchesRx rx) l

nameMatchesRx :: (RegexLike regex String, HasAttr a) => regex -> a -> Bool
nameMatchesRx rx itm = matchTest rx (gName itm)

matchesRxCurrent :: Regex -> TreeViewList -> Bool
matchesRxCurrent rx l = case L.listSelectedElement l of
  Just (_ix, itm) | matchTest rx (gName itm) -> True
  _ -> False
