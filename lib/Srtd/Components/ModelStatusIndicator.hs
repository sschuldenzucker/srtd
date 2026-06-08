-- | Tiny app-level indicator for model-level counts.
module Srtd.Components.ModelStatusIndicator (
  ModelStatusIndicator,
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

-- | Cached model-status indicator.
data ModelStatusIndicator = ModelStatusIndicator
  { msiInboxSize :: Int
  , msiClipboardSize :: Int
  }

suffixLenses ''ModelStatusIndicator

-- | Create a 'ModelStatusIndicator' from the current model.
make :: Model -> ModelStatusIndicator
make model =
  ModelStatusIndicator
    { msiInboxSize = inboxSize model
    , msiClipboardSize = clipboardSize model
    }

instance AppComponent ModelStatusIndicator where
  type Return ModelStatusIndicator = Void
  type Event ModelStatusIndicator = Void

  renderComponent msi
    | null renderedParts = str " "
    | otherwise = hBox . intersperse (str " ") $ renderedParts
   where
    renderedParts =
      [renderCount AppAttr.inbox_indicator (msiInboxSize msi) | msiInboxSize msi > 0]
        ++ [renderCount AppAttr.clipboard_indicator (msiClipboardSize msi) | msiClipboardSize msi > 0]
    renderCount attr n = withAttr attr . padLeftRight 1 . str $ show n

  handleEvent ev = case ev of
    AppEvent (ModelUpdated _) -> do
      mserver <- asks acModelServer
      model <- liftIO $ getModel mserver
      msiInboxSizeL .= inboxSize model
      msiClipboardSizeL .= clipboardSize model
      return Continue
    _ -> return Continue

  componentKeyDesc _ = KeyDesc "Model Status Indicator" False []

  componentTitle _ = T.pack "Model Status Indicator"
