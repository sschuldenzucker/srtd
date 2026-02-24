{-# LANGUAGE TypeFamilies #-}

-- | A component that lets users enter a query, filters for it, and lets them select a node
module Srtd.Components.QuickFilter (
  -- * Types
  QuickFilter (..),

  -- ** Variants

  -- These variants affect the behavior, including return types, of the component and need to be
  -- given as an argument on construction, and parameterize the query
  QueryEntry (..),
  NodeSelection (..),
  NodeSelectionOrQueryEntry (..),

  -- * Construction
  quickFilterFromTreeView,
) where

import Brick
import Brick.Keybindings
import Brick.Widgets.Border
import Control.Monad (forM_)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift, liftIO)
import Data.Text (Text)
import Data.Void (Void)
import Graphics.Vty.Input (Key (..), Modifier (MMeta))
import Lens.Micro.Platform
import Srtd.Attr (EID)
import Srtd.BrickHelpers (pattern SomeMouseUp, pattern SomeVtyOtherEvent, pattern VtyKeyEvent)
import Srtd.Component
import Srtd.Components.CompilingTextEntry (
  CompiledWithSource (..),
  CompilingTextEntry,
  MaybeEmpty (..),
  compilingRegexEntry,
 )
import Srtd.Components.CompilingTextEntry qualified as CTE
import Srtd.Components.TreeView (TreeView (..))
import Srtd.Components.TreeView qualified as TV
import Srtd.Keymap
import Srtd.Log
import Srtd.Model
import Srtd.ProactiveBandana
import Srtd.Query (SingleItemQuery (..))
import Srtd.Util (replaceExceptT, safeConst)
import Text.Regex.TDFA.Common (Regex)

-- * Types

-- SOMEDAY it is not clear to me if it's really more efficient to abstract the interaction variant
-- away or if we should just tolerate some code duplication.
-- Alternatively, we could encapsulate just the annoying behavior we want to (e.g., syncing the tree
-- view with the query entry) on the implementation side and duplicate the component itself.

-- | Variant that lets the user enter a query. They cannot scroll in the mini view below the query
-- entry. Only the query is returned, and it must be valid and return at least one result.
data QueryEntry = QueryEntry

-- | Variant that lets he user enter a query and select a matching node. The most recent valid query
-- is also returned and can be optionally used for highlighting, for example.
data NodeSelection = NodeSelection

-- | Mix between 'QueryEntry' and 'NodeSelection'. Regular confirm selects a node and alt-confirm
-- enters the query. In the latter case, the currently selected node is also returned and can
-- optionally be used to center the view or so.
data NodeSelectionOrQueryEntry = NodeSelectionOrQueryEntry

-- | Class of all variants
class VariantBehavior v where
  type ConfirmType v

  onTryConfirm :: ComponentEventM' (QuickFilter v)

  extraKeys :: [(Binding, KeymapItem (ComponentEventM' (QuickFilter v)))]
  extraKeys = []

-- SOMEDAY instead of Regex use SingleItemQuery (or Query)
data QuickFilter v = QuickFilter
  { sTextEntry :: CompilingTextEntry Regex
  , sTreeView :: TreeView
  , sValue ::
      Cell
        (MaybeEmpty (CompiledWithSource Regex), AppContext)
        (ComponentEventMOrNotFound (QuickFilter v) ())
        (MaybeEmpty (CompiledWithSource Regex))
  , sBaseFilter :: Filter
  , sKMZ :: KeymapZipper (ComponentEventM' (QuickFilter v))
  , sResourceName :: AppResourceName
  }

suffixLenses ''QuickFilter

-- * Construction

quickFilterFromTreeView ::
  forall v.
  (VariantBehavior v) => v -> TV.TreeView -> Text -> Text -> AppResourceName -> QuickFilter v
quickFilterFromTreeView _v tv s name rname =
  QuickFilter
    { sTextEntry = textEntry
    , sTreeView = TV.setResourceName (rname <> "treeview") tv & TV.tvSearchRxL .~ Nothing
    , sValue =
        let base = uniqueCell (const $ return ()) Empty $ \mev actx ->
              let ?actx = actx
               in maybePushQueryIntoTreeView mev
         in -- SOMEDAY screams for a pattern. Does this recur for multiple arguments? I think no.
            mapCellHandlerInput fst (\(_, actx) hf -> hf actx) base
    , sBaseFilter = cValue . tvFilter $ tv
    , sKMZ = keymapToZipper $ mkKeymap name
    , sResourceName = rname
    }
 where
  -- NB this relies on the CompilingTextEntry _not_ auto-applying the initial text.
  -- Otherwise this would needs a refresh already.
  textEntry = compilingRegexEntry s (rname <> "editor")

-- * Behavior

mkKeymap :: (VariantBehavior v) => Text -> Keymap (ComponentEventM' (QuickFilter v))
mkKeymap name =
  kmMake name $
    [ kmLeaf (binding KEsc []) "Cancel" $ return Canceled
    , kmLeaf (binding KEnter []) "Confirm" $ onTryConfirm
    ]
      ++ extraKeys

callIntoTreeView :: ComponentEventM TV.TreeView a -> ComponentEventM (QuickFilter v) a
callIntoTreeView = callIntoComponentEventM sTreeViewL $ safeConst (return ())

callIntoTextEntry ::
  ComponentEventM (CompilingTextEntry Regex) a -> ComponentEventMOrNotFound (QuickFilter v) a
callIntoTextEntry act = do
  -- SOMEDAY make a helper function for this.
  (ret, events) <- lift $ zoomComponentEventM sTextEntryL act
  forM_ events $ \case
    CTE.ValueChanged mev -> do
      actx <- ask
      runUpdateLens sValueL (mev, actx)
  return ret

instance (VariantBehavior v) => AppComponent (QuickFilter v) where
  type Return (QuickFilter v) = ConfirmType v

  -- TODO I think I want some events.
  type Event (QuickFilter v) = Void

  renderComponent s =
    vBox
      [ renderComponent (sTextEntry s)
      , hBorder
      , renderComponent (sTreeView s)
      ]

  handleEvent ev = case ev of
    AppEvent (ModelUpdated _) -> routeToBoth
    AppEvent Tick -> routeToBoth
    VtyKeyEvent KDown [] -> routeToTreeView
    VtyKeyEvent KUp [] -> routeToTreeView
    VtyKeyEvent key mods -> kmzDispatch sKMZL key mods routeToEdit
    MouseDown rname _k _mods _loc -> do
      myRName <- gets sResourceName
      if
        | (myRName <> "treeview") `isPrefixOf` rname -> routeToTreeView
        | (myRName <> "editor") `isPrefixOf` rname -> routeToEdit
        | otherwise -> do
            liftIO $
              glogL WARNING $
                "QuickFilter received click on " ++ show rname ++ ", which we don't recognize. Ignoring."
            return Continue
    -- TODO not clear to me wat do here
    SomeVtyOtherEvent -> routeToBoth
    SomeMouseUp -> return Continue
   where
    routeToTreeView = fmap translateEventReturn $ callIntoTreeView $ handleEvent ev
    routeToEdit = notFoundToAER $ fmap translateEventReturn $ callIntoTextEntry $ handleEvent ev
    routeToBoth = do
      _returnIsAlwaysContinue <- routeToEdit
      routeToTreeView

    -- specific to our two child components
    translateEventReturn ::
      AppEventReturn b -> AppEventReturn (Return (QuickFilter v))
    translateEventReturn = \case
      Continue -> Continue
      Confirmed _ -> Continue -- doesn't happen
      Canceled -> Canceled -- load error

  componentTitle = kmName . cur . kmzResetRoot . sKMZ

  componentKeyDesc s = (kmzDesc . sKMZ $ s) & kdPairsL %~ (++ extraPairs)
   where
    -- HACK. Some keys we map through dispatch, but they're not in our keymap.
    -- SOMEDAY that's ugly.
    extraPairs =
      [ ("Tab", "Complete")
      , ("C-l", "Clear")
      , ("C-d", "Clear")
      ]

-- | Push a given filter into our TreeView, unless it's Invalid.
--
-- NB this can fail only b/c TreeView reloads the model when we change the filter, which wouldn't
-- really be necessary and is actually a performance issue.
--
-- SOMEDAY this should be fixed.
maybePushQueryIntoTreeView ::
  (?actx :: AppContext) =>
  MaybeEmpty (CompiledWithSource Regex) -> ComponentEventMOrNotFound (QuickFilter v) ()
maybePushQueryIntoTreeView mev = do
  baseFilter <- gets sBaseFilter
  case mev of
    Invalid -> return ()
    Empty -> setFilterAndSearchRx baseFilter Nothing
    Valid v ->
      let (CompiledWithSource rx _) = v
          q = QueryRegexParts [rx]
          fullFilter = chainFilters (singleItemQueryFlatFilter q) baseFilter
       in setFilterAndSearchRx fullFilter (Just v)
 where
  setFilterAndSearchRx fi mv = do
    replaceExceptT callIntoTreeView $ do
      TV.replaceFilter fi
    sTreeViewL . TV.tvSearchRxL .= mv

-- * Variant instances

instance VariantBehavior QueryEntry where
  type ConfirmType QueryEntry = CompiledWithSource Regex

  onTryConfirm = do
    -- NB we could also use cValue . sValue (i.e., our own cell) but doesn't matter.
    mv <- gets (CTE.valueMaybe . sTextEntry)
    mcur <- gets (TV.tvCur . sTreeView)
    case (mv, mcur) of
      (Just v, Just _cur) -> return $ Confirmed v
      _ -> return Continue

instance VariantBehavior NodeSelection where
  type ConfirmType NodeSelection = (Maybe (CompiledWithSource Regex), EID)

  onTryConfirm = do
    mv <- gets (CTE.valueMaybe . sTextEntry)
    mcur <- gets (TV.tvCur . sTreeView)
    -- Filters with no results cannot be confirmed.
    case mcur of
      Nothing -> return Continue
      Just cur -> return $ Confirmed (mv, cur)

data NodeOrQueryConfirmed q
  = NodeSelected (Maybe (CompiledWithSource q)) EID
  | QueryConfirmed (CompiledWithSource q) EID

instance VariantBehavior NodeSelectionOrQueryEntry where
  type ConfirmType NodeSelectionOrQueryEntry = NodeOrQueryConfirmed Regex

  onTryConfirm = do
    mv <- gets (CTE.valueMaybe . sTextEntry)
    mcur <- gets (TV.tvCur . sTreeView)
    -- We don't let the user confirm anything if there are no results. (UX likely wants this)
    case mcur of
      Nothing -> return Continue
      Just cur -> return $ Confirmed $ NodeSelected mv cur

  -- TODO I think we should save the most recently valid filter here.
  extraKeys =
    [ kmLeaf (binding KEnter [MMeta]) "Confirm (filter)" $ do
        mv <- gets (CTE.valueMaybe . sTextEntry)
        mcur <- gets (TV.tvCur . sTreeView)
        case (mv, mcur) of
          -- Filters with no results cannot be confirmed.
          (Just v, Just cur) -> return $ Confirmed $ QueryConfirmed v cur
          _ -> return Continue
    ]
