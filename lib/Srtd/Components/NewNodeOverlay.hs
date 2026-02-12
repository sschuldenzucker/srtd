{-| Component for editing item names for new/existing items.

TODO Rename. It's more of a generic name editing component.
-}
module Srtd.Components.NewNodeOverlay where

import Brick
import Brick.Widgets.Edit
import Data.List (intercalate)
import Data.Text (Text)
import Graphics.Vty (Event (..), Key (..))
import Lens.Micro.Platform
import Srtd.Component
import Srtd.Keymap (KeyDesc (..))

data NewNodeOverlay = NewNodeOverlay
  { _nnEditor :: Editor String AppResourceName
  , _nnTitle :: Text
  }

makeLenses ''NewNodeOverlay

newNodeOverlay :: String -> Text -> AppResourceName -> NewNodeOverlay
newNodeOverlay initName title rname = NewNodeOverlay (editor editorRName (Just 1) initName) title
 where
  editorRName = EditorFor rname

instance AppComponent NewNodeOverlay String where
  -- TODO take 'has focus' into account. (currently always yes; this is ok *here for now* but not generally) (prob warrants a param)
  renderComponent self = renderEditor (str . intercalate "\n") True (self ^. nnEditor)

  handleEvent ev = case ev of
    (VtyEvent (EvKey KEsc [])) -> return Canceled
    (VtyEvent (EvKey KEnter [])) -> do
      NewNodeOverlay {_nnEditor} <- get
      -- `intercalate "\n"`, *not* `unlines` b/c that adds a trailing newline.
      let res = intercalate "\n" $ getEditContents _nnEditor
      return $ Confirmed res
    _ -> do
      zoom nnEditor $ handleEditorEvent ev
      aerContinue

  componentKeyDesc self = KeyDesc (_nnTitle self) True [("esc", "cancel"), ("enter", "confirm")]

  componentTitle = _nnTitle
