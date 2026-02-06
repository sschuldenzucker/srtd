-- TODO DataKinds not needed I think
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
-- TODO TypeApplications not needed I think
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- This is needed to create the AppComponent instance below b/c we're mixing fundeps with associated types.
-- It's fine in this particular case. See below.
{-# LANGUAGE UndecidableInstances #-}

-- | A component that lets users enter a query, filters for it, and lets them select a node
module Srtd.Components.QuickFilter where

import Brick
import Brick.Keybindings
import Control.Monad (liftM2)
import Control.Monad.Trans (lift)
import Data.Functor (void)
import Data.Text (Text)
import Graphics.Vty.Input (Event (..), Key (..), Modifier (MMeta))
import Lens.Micro.Platform
import Srtd.Attr (EID)
import Srtd.Component
import Srtd.Components.CompilingTextEntry (
  CompiledWithSource (..),
  CompilingTextEntry,
  compilingRegexEntry,
 )
import Srtd.Components.CompilingTextEntry qualified as CTE
import Srtd.Components.TreeView (TreeView (..))
import Srtd.Components.TreeView qualified as TV
import Srtd.Keymap
import Srtd.Model
import Srtd.Query (SingleItemQuery (..))
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
  type ContinueType v
  type ConfirmType v

  onTryConfirm :: MyAppEventAction v

  extraKeys :: [(Binding, KeymapItem (MyAppEventAction v))]
  extraKeys = []

  -- | Action to return the default "Continue" value.
  defaultContinue :: (?actx :: AppContext) => EventM AppResourceName (QuickFilter v) (ContinueType v)

myAERContinue ::
  (?actx :: AppContext, VariantBehavior v) =>
  EventM AppResourceName (QuickFilter v) (AppEventReturn (ContinueType v) b)
myAERContinue = Continue <$> defaultContinue

myAERVoid ::
  (?actx :: AppContext, VariantBehavior v) =>
  EventM AppResourceName (QuickFilter v) a ->
  EventM AppResourceName (QuickFilter v) (AppEventReturn (ContinueType v) b)
myAERVoid = (>> myAERContinue)

-- SOMEDAY instead of Regex use SingleItemQuery (or Query)
data QuickFilter v = QuickFilter
  { sTextEntry :: CompilingTextEntry Regex
  , sTreeView :: TreeView
  , sOldValue :: Maybe (CompiledWithSource Regex)
  , sBaseFilter :: Filter
  , sKMZ :: KeymapZipper (MyAppEventAction v)
  }

type MyAppEventAction v =
  AppEventAction
    (QuickFilter v)
    (ContinueType v)
    (ConfirmType v)

suffixLenses ''QuickFilter

-- * Construction

quickFilterFromTreeView ::
  forall v.
  (VariantBehavior v) => v -> TV.TreeView -> Text -> Text -> AppResourceName -> QuickFilter v
quickFilterFromTreeView _v tv s name rname =
  QuickFilter
    { sTextEntry = textEntry
    , sTreeView = TV.setResourceName (MainListFor rname) tv
    , sOldValue = Nothing
    , sBaseFilter = tvFilter tv
    , sKMZ = keymapToZipper $ mkKeymap name
    }
 where
  -- NB this relies on the CompilingTextEntry _not_ auto-applying the initial text.
  -- Otherwise this would needs a refresh already.
  textEntry = compilingRegexEntry s (EditorFor rname)

-- * Behavior

mkKeymap :: (VariantBehavior v) => Text -> Keymap (MyAppEventAction v)
mkKeymap name =
  kmMake name $
    [ kmLeafA (binding KEsc []) "Cancel" $ return Canceled
    , kmLeaf (binding KEnter []) "Confirm" $ onTryConfirm
    ]
      ++ extraKeys

-- This form, with UndecidableInstances, is required to mix associated types (used here) and
-- functional dependencies (used in AppComponent). It's a GHC limitation. It's fine logically.
-- See https://stackoverflow.com/questions/45360959/illegal-type-synonym-family-application-in-instance-with-functional-dependency
-- The injectivity constraint discussed there doesn't apply here.
--
-- SOMEDAY we can fix this by making a and b associated types of AppComponent, just like it's done
-- here. That'd be more ergonomic as well I think. We can recover the multi-param form using a type
-- synonym (which is really a constraint synonym):
-- `type Cla2 s a = (Cla s, a ~ A s)`
instance (VariantBehavior v, a ~ ContinueType v, b ~ ConfirmType v) => AppComponent (QuickFilter v) a b where
  renderComponent s =
    vBox
      [ renderComponent (sTextEntry s)
      , renderComponent (sTreeView s)
      ]

  -- TODO can be simplified by making multiple toplevel definitions, one for VtyEvent (EvKey ...) and one as a fallback.
  -- TODO What's going on here suggests to me that the data carried by Continue is too much. Remove it?
  -- -> I think yes. We're not using it all that much and this level of abstraction probably isn't needed?
  -- OR make it actually useful by returning what has changed.
  -- Check search entry, though, we're using it there! - Could do with inspection of state though I think.
  -- Remove all dependencies on continue types first, then eliminate it. Go step wise.
  -- What about Confirmed though? That one is probably important tbh b/c types are transformed.
  -- TODO Review this dispatching code. It's pretty convoluted & brittle, which seems to point to a design issue. Not sure it's real; depends a bit on how often we need something like this.
  handleEvent ev = do
    kmz <- gets sKMZ
    case ev of
      (VtyEvent (EvKey key mods)) ->
        case kmzLookup kmz key mods of
          NotFound -> handleFallback ev
          LeafResult act nxt -> do
            res <- runAppEventAction act
            sKMZL .= nxt
            return res
          SubmapResult sm -> sKMZL .= sm >> myAERContinue
      _ -> handleFallback ev -- not needed I think
   where
    handleFallback ev@(AppEvent _aev) = do
      -- Route app events to both sub-components.
      void $ zoom sTextEntryL $ handleEvent ev
      void $ zoom sTreeViewL $ handleEvent ev
      myAERContinue
    -- This weird case selection is required for routing b/c I have no way of knowing if an event
    -- got actually supported/handled. That is quite sad.
    handleFallback ev@(VtyEvent (EvKey k [])) = case k of
      -- TODO what to do with the keymap? E.g. the user can clear using C-l, complete with tab, etc. How to make this visible?
      KDown -> routeToTreeView
      KUp -> routeToTreeView
      -- By default, everything else should go to the editor.
      _ -> routeToEdit
     where
      routeToTreeView = myAERVoid $ zoom sTreeViewL $ handleEvent ev
      routeToEdit = do
        void $ zoom sTextEntryL $ handleEvent ev
        notFoundToAER $ do
          maybeSyncFilterToTreeView
          lift $ myAERContinue
    handleFallback _ = myAERContinue

  componentTitle = kmName . cur . kmzTop . sKMZ

  -- TODO include key desc from sTextEntry, which we also support.
  -- This needs some more infra, maybe a change to how components work.
  componentKeyDesc = kmzDesc . sKMZ

-- | Sync entered filter to tree view on change
maybeSyncFilterToTreeView ::
  (?actx :: AppContext) => EventMOrNotFound AppResourceName (QuickFilter v) ()
maybeSyncFilterToTreeView = do
  moldVal <- gets sOldValue
  mnewVal <- gets (CTE.sValue . sTextEntry)
  -- TODO update the search rx.
  -- 1. initially it should be Nothing
  -- 2. as we update here, it should also be updated.
  if
    -- TODO handle empty text. This yields mnewVal == Nothing so does nothing but should reset the filter.
    | (Just newVal) <- mnewVal
    , moldVal /= mnewVal -> do
        -- TODO This reloads the _whole_ subtree including all local derived attrs, on each key press.
        -- That wouldn't be needed if we give TreeView a way to filter only what's already there.
        baseFilter <- gets sBaseFilter
        let
          (CompiledWithSource rx _) = newVal
          q = QueryRegexParts [rx]
          fullFilter = chainFilters (singleItemQueryFlatFilter q) baseFilter
        zoom sTreeViewL $ TV.replaceFilter fullFilter
        sOldValueL .= mnewVal
    | otherwise -> return ()

-- * Variant instances

instance VariantBehavior QueryEntry where
  type ContinueType QueryEntry = Maybe (CompiledWithSource Regex)
  type ConfirmType QueryEntry = CompiledWithSource Regex

  defaultContinue = gets (CTE.sValue . sTextEntry)

  onTryConfirm = AppEventAction $ do
    mv <- gets (CTE.sValue . sTextEntry)
    mcur <- gets (TV.tvCur . sTreeView)
    case (mv, mcur) of
      (Just v, Just _cur) -> return $ Confirmed v
      _ -> return $ Continue mv

instance VariantBehavior NodeSelection where
  type ContinueType NodeSelection = (Maybe (CompiledWithSource Regex), Maybe EID)
  type ConfirmType NodeSelection = (Maybe (CompiledWithSource Regex), EID)

  defaultContinue = liftM2 (,) (gets $ CTE.sValue . sTextEntry) (gets $ TV.tvCur . sTreeView)

  onTryConfirm = AppEventAction $ do
    mv <- gets (CTE.sValue . sTextEntry)
    mcur <- gets (TV.tvCur . sTreeView)
    -- Filters with no results cannot be confirmed.
    case mcur of
      Nothing -> return $ Continue (mv, Nothing)
      Just cur -> return $ Confirmed (mv, cur)

data NodeOrQueryConfirmed q
  = NodeSelected (Maybe (CompiledWithSource q)) EID
  | QueryConfirmed (CompiledWithSource q) EID

instance VariantBehavior NodeSelectionOrQueryEntry where
  type ContinueType NodeSelectionOrQueryEntry = (Maybe (CompiledWithSource Regex), Maybe EID)
  type ConfirmType NodeSelectionOrQueryEntry = NodeOrQueryConfirmed Regex

  defaultContinue = liftM2 (,) (gets $ CTE.sValue . sTextEntry) (gets $ TV.tvCur . sTreeView)

  onTryConfirm = AppEventAction $ do
    mv <- gets (CTE.sValue . sTextEntry)
    mcur <- gets (TV.tvCur . sTreeView)
    -- We don't let the user confirm anything if there are no results. (UX likely wants this)
    case mcur of
      Nothing -> return $ Continue (mv, mcur)
      Just cur -> return $ Confirmed $ NodeSelected mv cur

  -- TODO I think we should save the most recently valid filter here.
  extraKeys =
    [ kmLeafA (binding KEnter [MMeta]) "Confirm (filter)" $ do
        mv <- gets (CTE.sValue . sTextEntry)
        mcur <- gets (TV.tvCur . sTreeView)
        case (mv, mcur) of
          -- Filters with no results cannot be confirmed.
          (Just v, Just cur) -> return $ Confirmed $ QueryConfirmed v cur
          _ -> return $ Continue (mv, mcur)
    ]
