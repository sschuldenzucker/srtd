-- | Tiny app-level indicator for model-level counts.
module Srtd.Components.ClipboardIndicator (
  ClipboardIndicator,
  make,
) where

import Brick
import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)
import Data.List (intersperse)
import Data.Text qualified as T
import Data.Void (Void)
import Lens.Micro.Platform
import Srtd.AppAttr qualified as AppAttr
import Srtd.Component
import Srtd.Keymap (KeyDesc (..))
import Srtd.Model
import Srtd.ModelServer

-- | Cached model-state indicator.
data ClipboardIndicator = ClipboardIndicator
  { ciInboxSize :: Int
  , ciClipboardSize :: Int
  }

suffixLenses ''ClipboardIndicator

-- | Create a 'ClipboardIndicator' from the current model.
make :: Model -> ClipboardIndicator
make model =
  ClipboardIndicator
    { ciInboxSize = inboxSize model
    , ciClipboardSize = clipboardSize model
    }

instance AppComponent ClipboardIndicator where
  type Return ClipboardIndicator = Void
  type Event ClipboardIndicator = Void

  renderComponent ci
    | null renderedParts = str " "
    | otherwise = hBox . intersperse (str " ") $ renderedParts
   where
    renderedParts =
      [renderCount AppAttr.inbox_indicator (ciInboxSize ci) | ciInboxSize ci > 0]
        ++ [renderCount AppAttr.clipboard_indicator (ciClipboardSize ci) | ciClipboardSize ci > 0]
    renderCount attr n = withAttr attr . padLeftRight 1 . str $ show n

  handleEvent ev = case ev of
    AppEvent (ModelUpdated _) -> do
      mserver <- asks acModelServer
      model <- liftIO $ getModel mserver
      ciInboxSizeL .= inboxSize model
      ciClipboardSizeL .= clipboardSize model
      return Continue
    _ -> return Continue

  componentKeyDesc _ = KeyDesc "Clipboard Indicator" False []

  componentTitle _ = T.pack "Clipboard Indicator"
