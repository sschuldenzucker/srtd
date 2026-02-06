{-| A component that lets the user enter some text that is compiled into some kind of other object,
usually a query.

Generalizes regex / single item query / query entry.
-}
module Srtd.Components.CompilingTextEntry (
  -- * Types
  CompiledWithSource (..),
  ConfirmType (..),
  CompilingTextEntry (..),

  -- * Construction
  compilingTextEntry,
  compilingSingleItemQueryEntry,
  compilingQueryEntry,
  compilingRegexEntry,

  -- * Helpers
  MaybeEmpty (..),
  maybeToMaybeEmpty,
  maybeEmptyValue,
) where

import Brick
import Brick.Keybindings
import Brick.Widgets.Edit
import Control.Monad (when)
import Data.Function qualified as Function
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Zipper qualified as TZ
import Graphics.Vty.Input.Events
import Lens.Micro.Platform
import Srtd.Component
import Srtd.Keymap
import Srtd.Query
import Srtd.Util (eitherToMaybe)
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

data CompilingTextEntry c = CompilingTextEntry
  { sEditor :: Editor Text AppResourceName
  , -- NB there is some redundancy here (text being stored in two places) but it's easier to return stuff this way.
    sValue :: Maybe (CompiledWithSource c)
  , sInitialText :: Text
  , sCompile :: Text -> Maybe c
  }

suffixLenses ''CompilingTextEntry

-- | A tertiary value that can be valid, empty, or invalid. We sometimes use this for regexs. (the
-- empty regex is invalid but should still receive some special treatment)
--
-- SOMEDAY we could make this the default return value type of CompilingTextEntry
data MaybeEmpty a = Valid a | Empty | Invalid
  deriving (Eq, Ord, Show)

-- | Map a Maybe to MaybeEmpty. The result will never be Empty.
maybeToMaybeEmpty :: Maybe a -> MaybeEmpty a
maybeToMaybeEmpty = \case
  Just x -> Valid x
  Nothing -> Invalid

maybeEmptyValue :: CompilingTextEntry a -> MaybeEmpty (CompiledWithSource a)
maybeEmptyValue cte
  | (T.intercalate "\n" . getEditContents . sEditor) cte == "" = Empty
  | otherwise = maybeToMaybeEmpty . sValue $ cte

type MyAppEventAction c =
  AppEventAction
    (CompilingTextEntry c)
    (Maybe (CompiledWithSource c))
    (CompiledWithSource c, ConfirmType)

-- | General form with a specified compiler
compilingTextEntry :: (Text -> Maybe c) -> Text -> AppResourceName -> CompilingTextEntry c
compilingTextEntry f s rname =
  -- SOMEDAY actual completion and highlight the previous pattern
  CompilingTextEntry
    { sEditor = editorText (EditorFor rname) (Just 1) ""
    , sValue = Nothing
    , sInitialText = s
    , sCompile = f
    }

compilingSingleItemQueryEntry :: Text -> AppResourceName -> CompilingTextEntry SingleItemQuery
compilingSingleItemQueryEntry = compilingTextEntry parseAndCompileSingleItemQuery

compilingQueryEntry :: Text -> AppResourceName -> CompilingTextEntry Query
compilingQueryEntry = compilingTextEntry parseAndCompileQuery

compilingRegexEntry :: Text -> AppResourceName -> CompilingTextEntry Regex
compilingRegexEntry = compilingTextEntry (eitherToMaybe . TDFA.compile myCompOpt myExecOpt)
 where
  myCompOpt = defaultCompOpt {caseSensitive = False}
  myExecOpt = defaultExecOpt {captureGroups = False}

keymap :: Keymap (MyAppEventAction c)
keymap =
  kmMake
    "Search"
    [ kmLeafA (binding KEsc []) "Cancel" $ return Canceled
    , -- SOMEDAY more descriptive names for this: it's confirm-and-go and confirm-and-go-to-sibling.
      kmLeafA (binding KEnter []) "Confirm" $ do
        mv <- use sValueL
        case mv of
          -- NB the user can't confirm an invalid regex.
          Nothing -> return $ Continue Nothing
          Just v -> return $ Confirmed (v, RegularConfirm)
    , kmLeafA (binding KEnter [MMeta]) "Confirm (alt)" $ do
        mv <- use sValueL
        case mv of
          -- NB the user can't confirm an invalid regex.
          Nothing -> return $ Continue Nothing
          Just v -> return $ Confirmed (v, AltConfirm)
    , ( kmLeafA (ctrl 'd') "Clear" $ do
          setText ""
          sEditorL %= applyEdit TZ.clearZipper
          Continue <$> use sValueL
      )
    , ( kmLeafA (ctrl 'l') "Clear" $ do
          setText ""
          sEditorL %= applyEdit TZ.clearZipper
          Continue <$> use sValueL
      )
    , kmLeafA (bind '\t') "Complete" $ do
        s0 <- gets (getEditContents . sEditor)
        let s = case s0 of
              [s_] -> s_
              _ -> error "Single-line editor must have a single line."
        init_ <- gets sInitialText
        -- SOMEDAY proper completion, also have a visual hint.
        when (s `T.isPrefixOf` init_) $ do
          sEditorL %= applyEdit (const $ TZ.textZipper [init_] (Just 1))
          setText init_
        Continue <$> use sValueL
    ]

keymapZipper :: KeymapZipper (MyAppEventAction c)
keymapZipper = keymapToZipper keymap

setText :: Text -> EventM AppResourceName (CompilingTextEntry c) ()
-- Can't search for an empty string.
setText "" = sValueL .= Nothing
setText s = do
  f <- gets sCompile
  let erx = compileWithSource f s
  sValueL .= erx

instance
  AppComponent
    (CompilingTextEntry c)
    (Maybe (CompiledWithSource c))
    ((CompiledWithSource c), ConfirmType)
  where
  renderComponent self = editUI
   where
    editUI = renderEditor (withAttr myAttr . txt . T.intercalate "\n") True (sEditor self)
    myAttr = attrName "search_entry" <> (if isNothing (sValue self) then attrName "error" else mempty)

  handleEvent ev =
    case ev of
      (VtyEvent (EvKey key mods)) -> do
        case kmzLookup keymapZipper key mods of
          NotFound -> handleFallback ev
          LeafResult act _nxt -> runAppEventAction act
          SubmapResult _sm -> error "wtf submap?"
      _ -> handleFallback ev
   where
    handleFallback ev' = do
      zoom sEditorL $ handleEditorEvent ev'
      text <- (T.intercalate "\n" . getEditContents) <$> use sEditorL
      setText text
      Continue <$> use sValueL

  componentKeyDesc _self = kmzDesc keymapZipper

  componentTitle _self = "Search"
