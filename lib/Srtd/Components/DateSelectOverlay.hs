{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Srtd.Components.DateSelectOverlay where

import Brick
import Brick.BChan (writeBChan)
import Brick.Keybindings (bind, binding, ctrl)
import Brick.Widgets.Edit
import Control.Monad (when)
import Control.Monad.State (liftIO)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (TimeZone, ZonedTime (zonedTimeZone))
import Graphics.Vty (Event (..), Key (..))
import Lens.Micro.Platform
import Srtd.Attr (EID)
import Srtd.Component
import Srtd.Dates
import Srtd.Keymap
import Srtd.Todo

-- | `Nothing` means that the user explicitly opted to delete the value. Note that the callback can
-- still ignore this.
--
-- SOMEDAY Ideally, the caller would configure this component to tell it if it's allowed to submit
-- Nothing, and then also get the right type.
type Callback = Maybe DateOrTime -> AppContext -> IO EID

data DateSelectOverlay = DateSelectOverlay
  { dsEditor :: Editor Text AppResourceName,
    dsValue :: Maybe DateOrTime,
    dsCallback :: Callback,
    dsZonedTime :: ZonedTime,
    dsOrigValue :: Maybe DateOrTime,
    dsTitle :: Text
  }

suffixLenses ''DateSelectOverlay

dsTimeZone :: DateSelectOverlay -> TimeZone
dsTimeZone = zonedTimeZone . dsZonedTime

-- SOMEDAY we don't pull new times when this overlay is open for longer.
-- That should be fine really but if we ever wanna be super-clean we could make a periodic "tick"
-- event (emitted by another thread) and listen to that.
dateSelectOverlay :: Callback -> Maybe DateOrTime -> ZonedTime -> Text -> AppResourceName -> DateSelectOverlay
dateSelectOverlay cb origValue ztime title rname =
  DateSelectOverlay
    { dsEditor = theEditor,
      dsValue = Nothing,
      dsCallback = cb,
      dsZonedTime = ztime,
      dsOrigValue = origValue,
      dsTitle = title
    }
  where
    theEditor = editor (EditorFor rname) (Just 1) ""

-- SOMEDAY a nice calendar display of the selected month and key bindings to directly select dates

keymap :: Keymap (AppContext -> EventM n DateSelectOverlay ())
keymap =
  kmMake
    "Select Date"
    [ kmLeaf (binding KEsc []) "Cancel" $ \ctx ->
        liftIO $ writeBChan (acAppChan ctx) (PopOverlay $ ORNone),
      kmLeaf (binding KEnter []) "Confirm" $ \ctx -> do
        mv <- use dsValueL
        when (isJust mv) $ submitAndClose ctx,
      kmLeaf (ctrl 'd') "Delete" submitAndClose
    ]
  where
    submitAndClose ctx = do
      mv <- use dsValueL
      cb <- use dsCallbackL
      eid <- liftIO $ cb mv ctx
      liftIO $ writeBChan (acAppChan ctx) (PopOverlay $ OREID eid)

-- Trivial rn.
keymapZipper :: KeymapZipper (AppContext -> EventM n DateSelectOverlay ())
keymapZipper = keymapToZipper keymap

instance BrickComponent DateSelectOverlay where
  -- TODO take 'has focus' into account. (currently always yes; this is ok *here for now* but not generally) (prob warrants a param)
  -- TODO make prettier, e.g., colors, spacing, padding, etc.
  -- TODO should we just show the original value (if any), to make the UI less weird?
  renderComponent self = editUI <=> dateUI <=> origDateUI
    where
      editUI = renderEditor (txt . T.intercalate "\n") True (dsEditor self)
      dateUI = renderDate (dsValue self)
      origDateUI = renderDate (dsOrigValue self)
      renderDate date = maybe emptyWidget (str . prettyAbsolute (dsTimeZone self)) $ date

  -- NB we don't have sub-keymaps here atm, so don't need to handle as much as MainTree, for instance.
  handleEvent ctx ev = case ev of
    (VtyEvent (EvKey key mods)) -> do
      case kmzLookup keymapZipper key mods of
        NotFound -> handleFallback ev
        LeafResult act _nxt -> act ctx
        SubmapResult _sm -> error "wtf submap?"
    _ -> handleFallback ev
    where
      handleFallback ev' = do
        zoom dsEditorL $ handleEditorEvent ev'
        text <- (T.intercalate "\n" . getEditContents) <$> use dsEditorL
        ztime <- use dsZonedTimeL
        dsValueL .= parseInterpretHumanDateOrTime text ztime

  componentKeyDesc self = kmzDesc keymapZipper & kdNameL .~ (dsTitle self)

  componentTitle = dsTitle
