{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | A component that lets users enter a query, filters for it, and lets them select a node
module Srtd.Components.QuickFilter where

import Brick
import Brick.Keybindings
import Data.Text (Text)
import Graphics.Vty.Input (Key (..))
import Srtd.Attr (EID)
import Srtd.Component
import Srtd.Components.CompilingTextEntry (
  CompiledWithSource,
  CompilingTextEntry,
  compilingRegexEntry,
 )
import Srtd.Components.CompilingTextEntry qualified as CTE
import Srtd.Components.TreeView (TreeView (..))
import Srtd.Components.TreeView qualified as TV
import Srtd.Keymap
import Srtd.Model
import Text.Regex.TDFA.Common (Regex)

-- SOMEDAY it is not clear to me if it's really more efficient to abstract the interaction variant
-- away or if we should just tolerate some code duplication.
-- Alternatively, we could encapsulate just the annoying behavior we want to (e.g., syncing the tree
-- view with the query entry) on the implementation side and duplicate the component itself.

-- | A variant is like a type-driven feature set. This is necessary b/c it affects return types (and
-- also behavior).
data Variant
  = -- | Enter a search query for later use that _must_ be valid. The user cannot select anything.
    QueryEntry
  | -- | Filter and select a node. The user _must_ select something. The (most recent valid) query
    -- is also returned. Invalid queries are allowed and ignored.
    NodeSelection

-- SOMEDAY additional option, for refile etc.
-- Not clear this needs an additional option or if the first step is just to select the anchor. But UX wise I think it should happen here.
-- Also, add an option for "... and jump here". And "... and jump here in a new tab." Especially for refile.
-- Now I'm wondering if I want a different interaction pattern.
-- Or honestly keep this one minimal and keep it alive in the background.
-- -> DON'T WORRY ABOUT IT QUITE YET.

-- | -- | Like 'NodeSelection' but the user selects a node + a direction. This is for insertion/moving.
--    NodeDirectionSelection
type family ContinueType (v :: Variant) where
  -- The currently entered query, if valid
  ContinueType 'QueryEntry = Maybe (CompiledWithSource Regex)
  -- The currently entered query, if valid, and the currently selected EID, if non-empty
  ContinueType 'NodeSelection = (Maybe (CompiledWithSource Regex), Maybe EID)

type family ConfirmType (v :: Variant) where
  -- The entered query. Invalid queries cannot be confirmed.
  ConfirmType 'QueryEntry = CompiledWithSource Regex
  -- The entered query OR the most recently entered valid one, and the selected node.
  -- The user cannot confirm if the query yields no results.
  ConfirmType 'NodeSelection = (Maybe (CompiledWithSource Regex), EID)

-- SOMEDAY instead of Regex use SingleItemQuery (or Query)
data QuickFilter (v :: Variant) = QuickFilter
  { sTextEntry :: CompilingTextEntry Regex
  , sTreeView :: TreeView
  , sBaseFilter :: Filter
  , sKMZ :: KeymapZipper (MyAppEventAction v)
  }

type MyAppEventAction v =
  AppEventAction
    (QuickFilter v)
    (ContinueType v)
    (ConfirmType v)

suffixLenses ''QuickFilter

quickFilterFromTreeView ::
  forall v. (VariantBehavior v) => TV.TreeView -> Text -> Text -> AppResourceName -> QuickFilter v
quickFilterFromTreeView tv s name rname =
  QuickFilter
    { sTextEntry = textEntry
    , sTreeView = tv
    , sBaseFilter = tvFilter tv
    , sKMZ = keymapToZipper $ mkKeymap name
    }
 where
  -- NB this relies on the CompilingTextEntry _not_ auto-applying the initial text.
  -- Otherwise this would needs a refresh already.
  textEntry = compilingRegexEntry s rname

quickQueryEntryFromTreeView ::
  TV.TreeView -> Text -> Text -> AppResourceName -> QuickFilter QueryEntry
quickQueryEntryFromTreeView = quickFilterFromTreeView

quickNodeSelectionFromTreeView ::
  TV.TreeView -> Text -> Text -> AppResourceName -> QuickFilter NodeSelection
quickNodeSelectionFromTreeView = quickFilterFromTreeView

-- TODO WIP so this certainly works. Review if it's more efficient than just making two separate components.
-- Keep in mind we'll want a third one for refile, though.
-- Maybe this becomes easier when I roll everything into VariantBehavior and maybe add a proxy somewhere.
--
-- TODO Can I just make two singleton types and just some multiparam type classes with fundeps and be done with it? (or maybe type families, they look ergonomic here)

-- SOMEDAY roll the two type families into this class. MAY make things unergonomic but try it out.
class VariantBehavior (v :: Variant) where
  onTryConfirm :: MyAppEventAction v

instance VariantBehavior QueryEntry where
  onTryConfirm = AppEventAction $ do
    mv <- gets (CTE.sValue . sTextEntry)
    case mv of
      Nothing -> return $ Continue Nothing
      Just v -> return $ Confirmed v

instance VariantBehavior NodeSelection where
  -- TODO I wanna return the most recently entered one.
  -- That needs additional, variant-dependent state. Implementable via another type family.
  onTryConfirm = AppEventAction $ do
    mv <- gets (CTE.sValue . sTextEntry)
    mcur <- gets (TV.tvCur . sTreeView)
    case mcur of
      Nothing -> return $ Continue (mv, Nothing)
      Just cur -> return $ Confirmed (mv, cur)

-- TODO WIP
-- Maybe start writing the Component instance as well.
-- TODO I also want a variant where I _can_ also submit the filter using a different key binding. Maybe alt-enter should do it. Are these just feature flags?

mkKeymap :: (VariantBehavior v) => Text -> Keymap (MyAppEventAction v)
mkKeymap name =
  kmMake name $
    [ kmLeafA (binding KEsc []) "Cancel" $ return Canceled
    , kmLeaf (binding KEnter []) "Confirm" $ onTryConfirm
    ]
