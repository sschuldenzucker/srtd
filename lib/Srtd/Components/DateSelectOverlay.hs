module Srtd.Components.DateSelectOverlay where

import Brick
import Brick.Keybindings (binding, ctrl)
import Brick.Widgets.Edit
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (ZonedTime (zonedTimeZone))
import Graphics.Vty (Event (..), Key (..))
import Lens.Micro.Platform
import Srtd.Component
import Srtd.Dates
import Srtd.Keymap

data DateSelectOverlay = DateSelectOverlay
  { dsEditor :: Editor Text AppResourceName
  , dsValue :: Maybe DateOrTime
  -- ^ `Nothing` means invalid and `Just` means valid. Deletion is handled directly and not
  -- represented in the state.
  , dsOrigValue :: Maybe DateOrTime
  , dsTitle :: Text
  }

suffixLenses ''DateSelectOverlay

type MyAppEventAction = AppEventAction DateSelectOverlay () (Maybe DateOrTime)

dateSelectOverlay ::
  Maybe DateOrTime -> Text -> AppResourceName -> DateSelectOverlay
dateSelectOverlay origValue title rname =
  DateSelectOverlay
    { dsEditor = theEditor
    , dsValue = Nothing
    , dsOrigValue = origValue
    , dsTitle = title
    }
 where
  theEditor = editor (EditorFor rname) (Just 1) ""

keymap :: Keymap MyAppEventAction
keymap =
  kmMake
    "Select Date"
    [ kmLeafA (binding KEsc []) "Cancel" $ return Canceled
    , kmLeafA (binding KEnter []) "Confirm" $ do
        mv <- use dsValueL
        case mv of
          -- We do *nothing* if we the current date is not valid! The user can't confirm then (I
          -- think this is the expected interaction).
          Nothing -> return $ Continue ()
          Just v -> return $ Confirmed (Just v)
    , kmLeafA (ctrl 'd') "Delete" (return $ Confirmed Nothing)
    ]

-- Trivial rn.
keymapZipper :: KeymapZipper MyAppEventAction
keymapZipper = keymapToZipper keymap

-- | This returns `Maybe DateOrTime` on confirm, which is the new value that should be set for the
-- attribute, i.e., a `Nothing` values means to set the attribute to empty and a `Just` value means
-- to set this value.
--
-- In contrast, cancellation is handled through a 'Canceled' return value. The user cannot enter an
-- invalid date.
instance AppComponent DateSelectOverlay () (Maybe DateOrTime) where
  -- TODO take 'has focus' into account. (currently always yes; this is ok *here for now* but not generally) (prob warrants a param)
  -- TODO make prettier, e.g., colors, spacing, padding, etc.
  renderComponent self = editUI <=> dateUI <=> origDateUI
   where
    editUI = renderEditor (txt . T.intercalate "\n") True (dsEditor self)
    dateUI = renderDate (dsValue self)
    origDateUI = renderDate (dsOrigValue self)
    renderDate date = maybe emptyWidget (str . prettyAbsolute tz) $ date
    tz = zonedTimeZone . acZonedTime $ ?actx

  -- NB we don't have sub-keymaps here atm, so don't need to handle as much as MainTree, for instance.
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
      zoom dsEditorL $ handleEditorEvent ev'
      text <- (T.intercalate "\n" . getEditContents) <$> use dsEditorL
      dsValueL .= parseInterpretHumanDateOrTime text (acZonedTime ?actx)
      return $ Continue ()

  componentKeyDesc self = kmzDesc keymapZipper & kdNameL .~ (dsTitle self)

  componentTitle = dsTitle
