{-# LANGUAGE OverloadedStrings #-}

module Srtd.Components.TestOverlay where

import Brick
import Brick.BChan (writeBChan)
import Brick.Keybindings (bind)
import Control.Monad.State (liftIO)
import Graphics.Vty (Event (..), Key (..))
import Srtd.Component
import Srtd.Keymap
import Srtd.Log

data TestOverlay = TestOverlay (KeymapZipper (AppContext -> EventM AppResourceName TestOverlay ()))

-- The following is more of a demonstration. Of course overkill here.

keymap :: Keymap (AppContext -> EventM n TestOverlay ())
keymap =
  kmMake
    "Test Overlay"
    [ kmLeaf (bind 'T') "Close" $ \(AppContext {acAppChan}) -> do
        liftIO $ writeBChan acAppChan (PopOverlay ORNone)
    , kmSub (bind 'a') stickySubmap
    ]

stickySubmap :: Keymap (AppContext -> EventM n TestOverlay ())
stickySubmap =
  sticky $
    kmMake
      "Sticky Submap"
      [ kmLeaf (bind 'a') "Noop" (const $ liftIO $ glogL INFO "TestOverlay Noop triggered")
      ]

newTestOverlay :: TestOverlay
newTestOverlay = TestOverlay (keymapToZipper keymap)

instance BrickComponent TestOverlay where
  renderComponent _ = str "Test Component. Press T to close."

  handleEvent _ (VtyEvent (EvKey KEsc [])) = modify (\(TestOverlay kmz) -> TestOverlay (kmzResetRoot kmz))
  handleEvent ctx (VtyEvent (EvKey key mods)) = do
    (TestOverlay kmz) <- get
    case kmzLookup kmz key mods of
      NotFound -> return ()
      LeafResult act nxt -> act ctx >> put (TestOverlay nxt)
      SubmapResult nxt -> put (TestOverlay nxt)
  handleEvent _ _ = return ()

  componentKeyDesc (TestOverlay kmz) = kmzDesc kmz

  componentTitle _ = "Test Overlay"
