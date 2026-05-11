-- | Tiny app-level indicator for the number of entries in the persistent clipboard.
module Srtd.Components.ClipboardIndicator (
  ClipboardIndicator,
  make,
) where

import Brick
import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)
import Data.Text qualified as T
import Data.Void (Void)
import Lens.Micro.Platform
import Srtd.AppAttr qualified as AppAttr
import Srtd.Component
import Srtd.Keymap (KeyDesc (..))
import Srtd.Model
import Srtd.ModelServer

-- | Cached clipboard-size indicator.
data ClipboardIndicator = ClipboardIndicator
  { ciClipboardSize :: Int
  }

suffixLenses ''ClipboardIndicator

-- | Create a 'ClipboardIndicator' from the current model.
make :: Model -> ClipboardIndicator
make model = ClipboardIndicator {ciClipboardSize = clipboardSize model}

instance AppComponent ClipboardIndicator where
  type Return ClipboardIndicator = Void
  type Event ClipboardIndicator = Void

  renderComponent ci
    | ciClipboardSize ci == 0 = str " "
    | otherwise = withAttr AppAttr.clipboard_indicator . str . show $ ciClipboardSize ci

  handleEvent ev = case ev of
    AppEvent (ModelUpdated _) -> do
      mserver <- asks acModelServer
      model <- liftIO $ getModel mserver
      ciClipboardSizeL .= clipboardSize model
      return Continue
    _ -> return Continue

  componentKeyDesc _ = KeyDesc "Clipboard Indicator" False []

  componentTitle _ = T.pack "Clipboard Indicator"
