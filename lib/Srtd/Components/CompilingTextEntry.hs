{-| A component that lets the user enter some text that is compiled into some kind of other object,
usually a query.

Generalizes regex / single item query / query entry.

SOMEDAY this could be just a Cell if we can reliably own our Editor. But then it's really basically an AppComponent.
-}
module Srtd.Components.CompilingTextEntry (
  -- * Types
  CompiledWithSource (..),
  ConfirmType (..),
  CompilingTextEntry (..),
  CompilingTextEntryEvent (..),

  -- * Construction
  compilingTextEntry,
  compilingSingleItemQueryEntry,
  compilingQueryEntry,
  compilingRegexEntry,

  -- * Access
  valueMaybeEmpty,
  valueMaybe,

  -- * Helpers
  MaybeEmpty (..),
  maybeToMaybeEmpty,
  maybeEmptyToMaybe,
  uniqueMaybeEmptyCell,
) where

import Brick
import Brick.Keybindings
import Control.Monad (forM_, when)
import Data.Function qualified as Function
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Zipper qualified as TZ
import Graphics.Vty.Input.Events
import Lens.Micro.Platform
import Srtd.BrickHelpers (pattern SomeNonVtyKeyBrickEvent, pattern VtyKeyEvent)
import Srtd.Component
import Srtd.Components.EditorProactive
import Srtd.Keymap
import Srtd.ProactiveBandana
import Srtd.Query
import Srtd.Util (captureWriterT, eitherToMaybe, tell1)
import Text.Regex.TDFA (
  CompOption (..),
  ExecOption (..),
  RegexOptions (..),
 )
import Text.Regex.TDFA.Text as TDFA

data ConfirmType
  = -- | Signals Return
    RegularConfirm
  | -- | Signals Meta-return
    AltConfirm

data CompiledWithSource c = CompiledWithSource
  { cwsCompiled :: c
  , cwsSource :: Text
  }

-- The Eq instances compares the _source_ not the compiled one. This creates a correct Eq instance
-- but note that two different sources can give rise to the same (e.g.) regex.
-- Where equality is used for reloading, this also means that there will be false positives when only unimportant details changed (e.g., spacing in a Query). This is not detected.
instance Eq (CompiledWithSource c) where
  (==) = (==) `Function.on` cwsSource

compileWithSource :: (Text -> Maybe c) -> Text -> Maybe (CompiledWithSource c)
compileWithSource f t = CompiledWithSource <$> (f t) <*> pure t

-- | A tertiary value that can be valid, empty, or invalid. We sometimes use this for regexs. (the
-- empty regex is invalid but should still receive some special treatment)
--
-- SOMEDAY we could make this the default return value type of CompilingTextEntry
data MaybeEmpty a = Valid a | Empty | Invalid
  deriving (Eq, Ord, Show)

data CompilingTextEntry c = CompilingTextEntry
  { sEditor :: EditorProactive
  , sValue :: Cell Text (AppEventM (CompilingTextEntry c) ()) (MaybeEmpty (CompiledWithSource c))
  , sInitialText :: Text
  }

suffixLenses ''CompilingTextEntry

-- | Map a Maybe to MaybeEmpty. The result will never be Empty.
maybeToMaybeEmpty :: Maybe a -> MaybeEmpty a
maybeToMaybeEmpty = \case
  Just x -> Valid x
  Nothing -> Invalid

maybeEmptyToMaybe :: MaybeEmpty a -> Maybe a
maybeEmptyToMaybe = \case
  Valid x -> Just x
  Empty -> Nothing
  Invalid -> Nothing

valueMaybeEmpty :: CompilingTextEntry c -> MaybeEmpty (CompiledWithSource c)
valueMaybeEmpty = cValue . sValue

valueMaybe :: CompilingTextEntry c -> Maybe (CompiledWithSource c)
valueMaybe = maybeEmptyToMaybe . valueMaybeEmpty

-- | Like 'uniqueMaybeCell' but for 'MaybeEmpty'
--
-- SOMEDAY may not be needed, it's pretty simple
uniqueMaybeEmptyCell ::
  (Eq v) => h -> MaybeEmpty v -> (v -> h) -> h -> Cell' (MaybeEmpty v) h
uniqueMaybeEmptyCell dflt mex0 onNewlyValid onNewlyEmpty = cell mex0 $ \mex' -> do
  mex <- get
  if
    | (Valid x') <- mex', mex' /= mex -> return $ onNewlyValid x'
    | Empty <- mex', mex' /= mex -> return $ onNewlyEmpty
    | otherwise -> return $ dflt

-- | General form with a specified compiler
compilingTextEntry :: (Text -> Maybe c) -> Text -> AppResourceName -> CompilingTextEntry c
compilingTextEntry f s rname =
  -- SOMEDAY actual completion and highlight the previous pattern
  CompilingTextEntry
    { sEditor = editorProactiveText "" (EditorFor rname)
    , -- NB this is *not* a unique cell b/c EditorProactive already has unique semantics for its text,
      -- and we wouldn't gain anything here.
      sValue = simpleMappingCell Empty compile' $ \mev' -> do
        -- A bit hacky b/c our component interface doesn't let us pass parameters, so we store this
        -- in state
        callIntoEditor $ setPostRender (postRenderFor $ maybeEmptyToMaybe mev')
        tell1 (ValueChanged mev')
    , sInitialText = s
    }
 where
  -- Special case so that the empty string is _always_ Nothing.
  -- This wouldn't be all that necessary b/c empty regexs are invalid but let's be sure about it.
  compile' "" = Empty
  compile' t = maybeToMaybeEmpty . compileWithSource f $ t

compilingSingleItemQueryEntry :: Text -> AppResourceName -> CompilingTextEntry SingleItemQuery
compilingSingleItemQueryEntry = compilingTextEntry parseAndCompileSingleItemQuery

compilingQueryEntry :: Text -> AppResourceName -> CompilingTextEntry Query
compilingQueryEntry = compilingTextEntry parseAndCompileQuery

compilingRegexEntry :: Text -> AppResourceName -> CompilingTextEntry Regex
compilingRegexEntry = compilingTextEntry (eitherToMaybe . TDFA.compile myCompOpt myExecOpt)
 where
  myCompOpt = defaultCompOpt {caseSensitive = False}
  myExecOpt = defaultExecOpt {captureGroups = False}

keymap :: Keymap (AppEventAction (CompilingTextEntry c))
keymap =
  kmMake
    "Search"
    [ kmLeafA (binding KEsc []) "Cancel" $ return Canceled
    , -- SOMEDAY more descriptive names for this: it's confirm-and-go and confirm-and-go-to-sibling.
      kmLeafA (binding KEnter []) "Confirm" $ do
        mv <- gets valueMaybe
        case mv of
          -- NB the user can't confirm an invalid regex.
          Nothing -> return $ Continue
          Just v -> return $ Confirmed (v, RegularConfirm)
    , kmLeafA (binding KEnter [MMeta]) "Confirm (alt)" $ do
        mv <- gets valueMaybe
        case mv of
          -- NB the user can't confirm an invalid regex.
          Nothing -> return $ Continue
          Just v -> return $ Confirmed (v, AltConfirm)
    , ( kmLeafA (ctrl 'd') "Clear" $ do
          callIntoEditor $ applyEdit TZ.clearZipper
          return Continue
      )
    , ( kmLeafA (ctrl 'l') "Clear" $ do
          callIntoEditor $ applyEdit TZ.clearZipper
          return Continue
      )
    , kmLeafA (bind '\t') "Complete" $ do
        s <- gets (getEditorText . sEditor)
        -- SOMEDAY proper completion, also have a visual hint.
        init_ <- gets sInitialText
        when (s `T.isPrefixOf` init_) $ do
          callIntoEditor $ applyEdit (const $ TZ.textZipper [init_] (Just 1))
        return Continue
    ]

callIntoEditor :: AppEventM EditorProactive a -> AppEventM (CompilingTextEntry c) a
callIntoEditor act = do
  (ret, events) <- captureWriterT $ zoom sEditorL act
  forM_ events $ \case
    TextChanged t -> runUpdateLens sValueL t
  return ret

postRenderFor :: Maybe a -> Widget n -> Widget n
postRenderFor mv =
  withAttr $ attrName "search_entry" <> (if isNothing mv then attrName "error" else mempty)

-- SOMEDAY should this be MaybeEmpty?
data CompilingTextEntryEvent c = ValueChanged (MaybeEmpty (CompiledWithSource c))

instance
  AppComponent
    (CompilingTextEntry c)
  where
  type Return (CompilingTextEntry c) = ((CompiledWithSource c), ConfirmType)

  type Event (CompilingTextEntry c) = CompilingTextEntryEvent c

  renderComponent = renderComponent . sEditor

  handleEvent ev =
    case ev of
      VtyKeyEvent key mods -> kmDispatch keymap key mods (handleFallback ev)
      SomeNonVtyKeyBrickEvent -> handleFallback ev
      AppEvent (ModelUpdated _) -> handleFallback ev
      AppEvent Tick -> handleFallback ev
   where
    handleFallback ev' = do
      _resIsAlwaysContinue <- callIntoEditor $ handleEvent ev'
      return Continue

  componentKeyDesc _self = kmDesc keymap

  componentTitle _self = "Search"
