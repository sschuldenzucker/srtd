{-# LANGUAGE TemplateHaskell #-}

module Components.NewNodeOverlay where

import Attr (EID)
import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.Edit
import Component
import Control.Monad.State (liftIO)
import Graphics.Vty (Event (..), Key (..))
import Lens.Micro.Platform

-- SOMEDAY instead of this half-assed continuation passing style, maybe we can find something that's:
-- - Either actual CPS
-- - Or this could just return the String and then the caller has to deal with it
-- - Or this just does its modification and then the caller has to deal with it.

type Callback = String -> AppContext -> IO EID

data NewNodeOverlay = NewNodeOverlay
  { _nnEditor :: Editor String AppResourceName,
    -- TODO looks way too general tbh.
    _nnCallback :: Callback
  }

makeLenses ''NewNodeOverlay

newNodeOverlay :: Callback -> AppResourceName -> NewNodeOverlay
newNodeOverlay cb name = NewNodeOverlay (editor name (Just 1) "") cb

instance BrickComponent NewNodeOverlay where
  -- TODO take 'has focus' into account. (currently always yes; this is ok *here for now* but not generally) (prob warrants a param)
  renderComponent self = renderEditor (str . unlines) True (self ^. nnEditor)

  handleEvent ctx ev = case ev of
    (VtyEvent (EvKey KEsc [])) ->
      liftIO $ writeBChan acAppChan (PopOverlay $ ORNone)
    (VtyEvent (EvKey KEnter [])) -> do
      NewNodeOverlay {_nnEditor, _nnCallback} <- get
      eid <- liftIO $ _nnCallback (unlines $ getEditContents _nnEditor) ctx
      liftIO $ writeBChan acAppChan (PopOverlay $ OREID eid)
    _ -> zoom nnEditor $ handleEditorEvent ev
    where
      AppContext {acAppChan} = ctx
