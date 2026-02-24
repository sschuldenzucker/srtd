module Srtd.Components.DateSelectOverlay (
  -- * Types
  DateSelectOverlay (..),
  DateSelectOverlayEvent (..),

  -- * Construction
  dateSelectOverlay,
) where

import Brick
import Brick.Keybindings (binding, ctrl)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import Data.Time (ZonedTime (zonedTimeZone))
import Graphics.Vty (Key (..))
import Lens.Micro.Platform
import Srtd.BrickHelpers (pattern SomeNonVtyKeyBrickEvent, pattern VtyKeyEvent)
import Srtd.Component
import Srtd.Components.EditorProactive
import Srtd.Dates
import Srtd.Keymap
import Srtd.ProactiveBandana
import Srtd.Util (tell1)

data DateSelectOverlay = DateSelectOverlay
  { dsEditor :: EditorProactive
  , dsValue :: Cell (Text, ZonedTime) (ComponentEventM DateSelectOverlay ()) (Maybe DateOrTime)
  -- ^ `Nothing` means invalid and `Just` means valid. Deletion is handled directly and not
  -- represented in the state.
  -- NB this updates *only when* the user changes the text, not when the reference time changes.
  -- This is intentional.
  , dsOrigValue :: Maybe DateOrTime
  , dsTitle :: Text
  }

suffixLenses ''DateSelectOverlay

dateSelectOverlay ::
  Maybe DateOrTime -> Text -> AppResourceName -> DateSelectOverlay
dateSelectOverlay origValue title rname =
  DateSelectOverlay
    { dsEditor = editorProactiveText "" (rname <> "editor")
    , dsValue = simpleMappingCell Nothing compile' $ \mv -> tell1 (ValueChanged mv)
    , dsOrigValue = origValue
    , dsTitle = title
    }
 where
  compile' = uncurry parseInterpretHumanDateOrTime

keymap :: Keymap (ComponentEventM' DateSelectOverlay)
keymap =
  kmMake
    "Select Date"
    [ kmLeaf (binding KEsc []) "Cancel" $ return Canceled
    , kmLeaf (binding KEnter []) "Confirm" $ do
        mv <- gets (cValue . dsValue)
        case mv of
          -- We do *nothing* if we the current date is not valid! The user can't confirm then (I
          -- think this is the expected interaction).
          Nothing -> return $ Continue
          Just v -> return $ Confirmed (Just v)
    , kmLeaf (ctrl 'd') "Delete" (return $ Confirmed Nothing)
    ]

callIntoEditor ::
  ComponentEventM EditorProactive a -> ComponentEventM DateSelectOverlay a
callIntoEditor = callIntoComponentEventM dsEditorL $ \case
  TextChanged t -> do
    zt <- asks acZonedTime
    runUpdateLens dsValueL (t, zt)

data DateSelectOverlayEvent = ValueChanged (Maybe DateOrTime)

-- | This returns `Maybe DateOrTime` on confirm, which is the new value that should be set for the
-- attribute, i.e., a `Nothing` values means to set the attribute to empty and a `Just` value means
-- to set this value.
--
-- In contrast, cancellation is handled through a 'Canceled' return value. The user cannot enter an
-- invalid date.
instance AppComponent DateSelectOverlay where
  type Return DateSelectOverlay = Maybe DateOrTime
  type Event DateSelectOverlay = DateSelectOverlayEvent

  -- TODO take 'has focus' into account. (currently always yes; this is ok *here for now* but not generally) (prob warrants a param)
  -- TODO make prettier, e.g., colors, spacing, padding, etc.
  renderComponent self = editUI <=> dateUI <=> origDateUI
   where
    editUI = renderComponent (dsEditor self)
    dateUI = renderDate (cValue $ dsValue self)
    origDateUI = renderDate (dsOrigValue self)
    renderDate date = maybe emptyWidget (str . prettyAbsolute tz) $ date
    tz = zonedTimeZone . acZonedTime $ ?actx

  handleEvent ev =
    case ev of
      VtyKeyEvent key mods -> kmDispatch keymap key mods (handleFallback ev)
      SomeNonVtyKeyBrickEvent -> handleFallback ev
      AppEvent (ModelUpdated _) -> handleFallback ev
      AppEvent Tick -> handleFallback ev
   where
    handleFallback ev' = do
      _returnIsAlwaysContinue <- callIntoEditor (handleEvent ev')
      return Continue

  componentKeyDesc self = kmDesc keymap & kdNameL .~ (dsTitle self)

  componentTitle = dsTitle
