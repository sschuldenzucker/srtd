module Components.TestOverlay where

import Brick
import Brick.BChan (writeBChan)
import Component
import Control.Monad.State (liftIO)
import Graphics.Vty (Event (..), Key (..))

data TestOverlay = TestOverlay

instance BrickComponent TestOverlay where
  renderComponent TestOverlay = str "Test Component. Press T to close."
  handleEvent (AppContext {acAppChan}) (VtyEvent (EvKey (KChar 'T') [])) = do
    liftIO $ writeBChan acAppChan (PopOverlay ORNone)
  handleEvent _ _ = return ()
