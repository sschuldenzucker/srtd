-- | A component that lets the user enter a regex. We validate and stream as we go.
module Srtd.Components.RegexSearchEntryOverlay (
  regexSearchEntryOverlay,
  RegexWithSource (..),
  compileWithSource,
  ConfirmType (..),
) where

import Brick
import Brick.Keybindings (binding, ctrl)
import Brick.Widgets.Edit
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Zipper qualified as TZ
import Graphics.Vty (Event (..), Key (..), Modifier (MMeta))
import Lens.Micro.Platform
import Srtd.Component
import Srtd.Keymap
import Srtd.Util (eitherToMaybe)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

data ConfirmType
  = -- | Signals Return
    RegularConfirm
  | -- | Signals Meta-return
    AltConfirm

data RegexWithSource = RegexWithSource
  { rxsRegex :: Regex
  , rxsSource :: T.Text
  }

compileWithSource :: CompOption -> ExecOption -> Text -> Either String RegexWithSource
compileWithSource compOpt execOpt s =
  compile compOpt execOpt s <&> \rx ->
    RegexWithSource rx s

data MyState = MyState
  { sEditor :: Editor Text AppResourceName
  , -- NB there is some redundancy here (text being stored in two places) but it's easier to return stuff this way.
    sValue :: Maybe RegexWithSource
  }

suffixLenses ''MyState

type MyAppEventAction =
  AppEventAction MyState (Maybe RegexWithSource) (RegexWithSource, ConfirmType)

regexSearchEntryOverlay :: Text -> AppResourceName -> MyState
regexSearchEntryOverlay s rname =
  MyState
    { sEditor = editor (EditorFor rname) (Just 1) s & applyEdit TZ.gotoEOF
    , sValue = eitherToMaybe $ compileWithSource myCompOpt myExecOpt s
    }

keymap :: Keymap MyAppEventAction
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
    ]

keymapZipper :: KeymapZipper MyAppEventAction
keymapZipper = keymapToZipper keymap

myCompOpt :: CompOption
myCompOpt = defaultCompOpt {caseSensitive = False}

myExecOpt :: ExecOption
myExecOpt = defaultExecOpt {captureGroups = False}

setText :: Text -> EventM AppResourceName MyState ()
-- Can't search for an empty string.
setText "" = sValueL .= Nothing
setText s = do
  let erx = compileWithSource myCompOpt myExecOpt s
  sValueL .= eitherToMaybe erx

instance AppComponent MyState (Maybe RegexWithSource) (RegexWithSource, ConfirmType) where
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
