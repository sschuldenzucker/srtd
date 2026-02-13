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
) where

import Brick
import Brick.Keybindings
import Control.Monad (forM_, when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Zipper qualified as TZ
import Graphics.Vty.Input.Events
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

compileWithSource :: (Text -> Maybe c) -> Text -> Maybe (CompiledWithSource c)
compileWithSource f t = CompiledWithSource <$> (f t) <*> pure t

data CompilingTextEntry c = CompilingTextEntry
  { sEditor :: EditorProactive
  , sValue :: Cell Text (AppEventM (CompilingTextEntry c) ()) (Maybe (CompiledWithSource c))
  , sInitialText :: Text
  }

suffixLenses ''CompilingTextEntry

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
    { sEditor = editorProactiveText "" (EditorFor rname)
    , sValue = simpleMappingCell Nothing compile' $ \mv' -> do
        -- A bit hacky b/c our component interface doesn't let us pass parameters, so we store this
        -- in state
        callIntoEditor $ setPostRender (postRenderFor mv')
        tell1 (ValueChanged mv')
    , sInitialText = s
    }
 where
  -- Special case so that the empty string is _always_ Nothing.
  -- This wouldn't be all that necessary b/c empty regexs are invalid but let's be sure about it.
  compile' "" = Nothing
  compile' t = compileWithSource f $ t

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
        mv <- gets (cValue . sValue)
        case mv of
          -- NB the user can't confirm an invalid regex.
          Nothing -> return $ Continue
          Just v -> return $ Confirmed (v, RegularConfirm)
    , kmLeafA (binding KEnter [MMeta]) "Confirm (alt)" $ do
        mv <- gets (cValue . sValue)
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

keymapZipper :: KeymapZipper (MyAppEventAction c)
keymapZipper = keymapToZipper keymap

callIntoEditor :: AppEventM EditorProactive a -> AppEventM (CompilingTextEntry c) a
callIntoEditor act = do
  (ret, events) <- captureWriterT $ zoom sEditorL act
  forM_ events $ \case
    TextChanged t -> runUpdateLens sValueL t
  return ret

postRenderFor :: Maybe a -> Widget n -> Widget n
postRenderFor mv =
  withAttr $ attrName "search_entry" <> (if isNothing mv then attrName "error" else mempty)

data CompilingTextEntryEvent c = ValueChanged (Maybe (CompiledWithSource c))

instance
  AppComponent
    (CompilingTextEntry c)
  where
  type Return (CompilingTextEntry c) = ((CompiledWithSource c), ConfirmType)

  type Event (CompilingTextEntry c) = CompilingTextEntryEvent c

  renderComponent = renderComponent . sEditor

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
      _resIsAlwaysContinue <- callIntoEditor $ handleEvent ev'
      return Continue

  componentKeyDesc _self = kmzDesc keymapZipper

  componentTitle _self = "Search"
