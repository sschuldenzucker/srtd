{-# LANGUAGE OverloadedStrings #-}

module Components.TestOverlay where

import Brick
import Brick.BChan (writeBChan)
import Brick.Keybindings (bind)
import Component
import Control.Monad.State (liftIO)
import Graphics.Vty (Event (..), Key (..))
import Keymap

data TestOverlay = TestOverlay

-- The following is more of a demonstration. Of course overkill here.

keymap :: KeymapZipper (AppContext -> EventM n TestOverlay ())
keymap =
  kmzMake
    [ kmLeaf (bind 'T') "close" $ \(AppContext {acAppChan}) -> do
        liftIO $ writeBChan acAppChan (PopOverlay ORNone)
    ]

instance BrickComponent TestOverlay where
  renderComponent TestOverlay = str "Test Component. Press T to close."

  handleEvent ctx (VtyEvent (EvKey key mods)) = case stepKeymap keymap key mods of
    NotFound -> return ()
    LeafResult act -> act ctx
    SubmapResult _ -> error "wtf"
  handleEvent _ _ = return ()

  -- handleEvent (AppContext {acAppChan}) (VtyEvent (EvKey (KChar 'T') [])) = do
  --   liftIO $ writeBChan acAppChan (PopOverlay ORNone)
  -- handleEvent _ _ = return ()

  componentKeyDesc _ = kmzDesc keymap
