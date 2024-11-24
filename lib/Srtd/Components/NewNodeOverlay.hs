{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Component for editing item names for new/existing items.
--
-- TODO Rename. It's more of a generic name editing component.
module Srtd.Components.NewNodeOverlay where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.Edit
import Control.Monad.State (liftIO)
import Data.List (intercalate)
import Data.Text (Text)
import Graphics.Vty (Event (..), Key (..))
import Lens.Micro.Platform
import Srtd.Attr (EID)
import Srtd.Component
import Srtd.Keymap (KeyDesc (..))

-- SOMEDAY instead of this half-assed continuation passing style, maybe we can find something that's:
-- - Either actual CPS
-- - Or this could just return the String and then the caller has to deal with it
-- - Or this just does its modification and then the caller has to deal with it.

-- | Callback passed to this. Must return the EID that is passed down to the parent on confirm (Enter).
--
-- Alternatively, this could be "pure" and *only* pass down the entered name to the parent, but it's not doing that rn.
type Callback = String -> AppContext -> IO EID

data NewNodeOverlay = NewNodeOverlay
  { _nnEditor :: Editor String AppResourceName
  , -- TODO looks way too general tbh.
    _nnCallback :: Callback
  , _nnTitle :: Text
  }

makeLenses ''NewNodeOverlay

newNodeOverlay :: Callback -> String -> Text -> AppResourceName -> NewNodeOverlay
newNodeOverlay cb initName title rname = NewNodeOverlay (editor editorRName (Just 1) initName) cb title
  where
    editorRName = EditorFor rname

instance BrickComponent NewNodeOverlay where
  -- TODO take 'has focus' into account. (currently always yes; this is ok *here for now* but not generally) (prob warrants a param)
  renderComponent self = renderEditor (str . intercalate "\n") True (self ^. nnEditor)

  handleEvent ctx ev = case ev of
    (VtyEvent (EvKey KEsc [])) ->
      liftIO $ writeBChan acAppChan (PopOverlay $ ORNone)
    (VtyEvent (EvKey KEnter [])) -> do
      NewNodeOverlay {_nnEditor, _nnCallback} <- get
      -- `intercalate "\n"`, *not* `unlines` b/c that adds a trailing newline.
      eid <- liftIO $ _nnCallback (intercalate "\n" $ getEditContents _nnEditor) ctx
      liftIO $ writeBChan acAppChan (PopOverlay $ OREID eid)
    _ -> zoom nnEditor $ handleEditorEvent ev
    where
      AppContext {acAppChan} = ctx

  componentKeyDesc self = KeyDesc (_nnTitle self) True [("esc", "cancel"), ("enter", "confirm")]

  componentTitle = _nnTitle
