{-| Component for editing item names for new/existing items.

TODO Rename. It's more of a generic name editing component.
-}
module Srtd.Components.NewNodeOverlay (
  -- * Types
  NewNodeOverlay (..),

  -- * Construction
  newNodeOverlay,
) where

import Brick
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Graphics.Vty (Key (..))
import Lens.Micro.Platform
import Srtd.BrickHelpers (pattern SomeNonVtyKeyBrickEvent, pattern VtyKeyEvent)
import Srtd.Component
import Srtd.Components.EditorProactive
import Srtd.Keymap (KeyDesc (..))

data NewNodeOverlay = NewNodeOverlay
  { _nnEditor :: EditorProactive
  , _nnTitle :: Text
  }

makeLenses ''NewNodeOverlay

newNodeOverlay :: String -> Text -> AppResourceName -> NewNodeOverlay
newNodeOverlay initName title rname =
  NewNodeOverlay (editorProactiveText (T.pack initName) editorRName) title
 where
  editorRName = rname <> "editor"

callIntoEditor :: ComponentEventM EditorProactive a -> ComponentEventM NewNodeOverlay a
-- events are ignored b/c we don't need them for now.
callIntoEditor = callIntoComponentEventM nnEditor $ const (return ())

instance AppComponent NewNodeOverlay where
  type Return NewNodeOverlay = String
  type Event NewNodeOverlay = Void

  -- TODO take 'has focus' into account. (currently always yes; this is ok *here for now* but not generally) (prob warrants a param)
  renderComponent self = renderComponent (self ^. nnEditor)

  handleEvent ev = case ev of
    VtyKeyEvent KEsc [] -> return Canceled
    VtyKeyEvent KEnter [] -> do
      NewNodeOverlay {_nnEditor} <- get
      let res = getEditorText _nnEditor
      return $ Confirmed (T.unpack res)
    VtyKeyEvent _key _mods -> handleFallback
    SomeNonVtyKeyBrickEvent -> handleFallback
    AppEvent (ModelUpdated _) -> handleFallback
    AppEvent Tick -> handleFallback
   where
    handleFallback =
      do
        _resIsAlwaysContinue <- callIntoEditor $ handleEvent ev
        return Continue

  componentKeyDesc self = KeyDesc (_nnTitle self) True [("esc", "cancel"), ("enter", "confirm")]

  componentTitle = _nnTitle
