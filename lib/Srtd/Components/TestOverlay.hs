module Srtd.Components.TestOverlay where

import Brick
import Brick.Keybindings (bind)
import Control.Monad.State (liftIO)
import Graphics.Vty (Event (..), Key (..))
import Srtd.Component
import Srtd.Keymap
import Srtd.Log

data TestOverlay = TestOverlay (KeymapZipper (AppEventAction TestOverlay () ()))

-- The following is more of a demonstration. Of course overkill here.

keymap :: Keymap (AppEventAction TestOverlay () ())
keymap =
  kmMake
    "Test Overlay"
    [ kmLeafA (bind 'T') "Close" $ return (Confirmed ())
    , kmSub (bind 'a') stickySubmap
    ]

stickySubmap :: Keymap (AppEventAction TestOverlay () ())
stickySubmap =
  sticky $
    kmMake
      "Sticky Submap"
      [ kmLeafA_ (bind 'a') "Noop" $ do
          liftIO $ glogL INFO "TestOverlay Noop triggered"
      ]

newTestOverlay :: TestOverlay
newTestOverlay = TestOverlay (keymapToZipper keymap)

instance AppComponent TestOverlay () () where
  renderComponent _ = str "Test Component. Press T to close."

  handleEvent (VtyEvent (EvKey KEsc [])) = do
    modify (\(TestOverlay kmz) -> TestOverlay (kmzResetRoot kmz))
    return $ Canceled
  handleEvent (VtyEvent (EvKey key mods)) = do
    (TestOverlay kmz) <- get
    case kmzLookup kmz key mods of
      NotFound -> return $ Continue ()
      LeafResult act nxt -> do
        res <- runAppEventAction act
        put (TestOverlay nxt)
        return res
      SubmapResult nxt -> put (TestOverlay nxt) >> return (Continue ())
  handleEvent _ = return $ Continue ()

  componentKeyDesc (TestOverlay kmz) = kmzDesc kmz

  componentTitle _ = "Test Overlay"
