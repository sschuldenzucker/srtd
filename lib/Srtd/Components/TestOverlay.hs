module Srtd.Components.TestOverlay (
  -- * Types
  TestOverlay (..),

  -- * Construction
  newTestOverlay,
) where

import Brick
import Brick.Keybindings (bind)
import Control.Monad.State (liftIO)
import Data.Void (Void)
import Graphics.Vty (Event (..), Key (..))
import Srtd.Component
import Srtd.Keymap
import Srtd.Log

data TestOverlay = TestOverlay {toKMZ :: KeymapZipper (AppEventAction TestOverlay ())}

suffixLenses ''TestOverlay

-- The following is more of a demonstration. Of course overkill here.

keymap :: Keymap (AppEventAction TestOverlay ())
keymap =
  kmMake
    "Test Overlay"
    [ kmLeafA (bind 'T') "Close" $ return (Confirmed ())
    , kmSub (bind 'a') stickySubmap
    ]

stickySubmap :: Keymap (AppEventAction TestOverlay ())
stickySubmap =
  sticky $
    kmMake
      "Sticky Submap"
      [ kmLeafA_ (bind 'a') "Noop" $ do
          liftIO $ glogL INFO "TestOverlay Noop triggered"
      ]

newTestOverlay :: TestOverlay
newTestOverlay = TestOverlay (keymapToZipper keymap)

instance AppComponent TestOverlay where
  type Return TestOverlay = ()
  type Event TestOverlay = Void

  renderComponent _ = str "Test Component. Press T to close."

  handleEvent (VtyEvent (EvKey KEsc [])) = do
    modify (\(TestOverlay kmz) -> TestOverlay (kmzResetRoot kmz))
    return $ Canceled
  handleEvent (VtyEvent (EvKey key mods)) = kmzDispatch toKMZL key mods (return Continue)
  handleEvent _ = return Continue

  componentKeyDesc (TestOverlay kmz) = kmzDesc kmz

  componentTitle _ = "Test Overlay"
