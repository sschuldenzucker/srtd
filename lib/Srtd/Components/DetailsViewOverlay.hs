{-# LANGUAGE OverloadedStrings #-}

module Srtd.Components.DetailsViewOverlay (DetailsViewOverlay, newDetailsViewOverlay) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Keybindings (bind)
import Brick.Widgets.Table
import Control.Monad.State (liftIO)
import Graphics.Vty (Event (..), Key (..))
import Srtd.Attr
import Srtd.Component
import Srtd.Keymap
import Srtd.Log
import Srtd.Todo (todo)

data DetailsViewOverlay = DetailsViewOverlay LocalIdLabel

newDetailsViewOverlay :: LocalIdLabel -> DetailsViewOverlay
newDetailsViewOverlay = DetailsViewOverlay

keymap :: Keymap (AppContext -> EventM n DetailsViewOverlay ())
keymap =
  kmMake
    "Item Details Overlay"
    [ kmLeaf (bind ' ') "Close" $ \(AppContext {acAppChan}) -> do
        liftIO $ writeBChan acAppChan (PopOverlay ORNone)
    ]

kmz :: KeymapZipper (AppContext -> EventM n DetailsViewOverlay ())
kmz = keymapToZipper keymap

instance BrickComponent DetailsViewOverlay where
  renderComponent (DetailsViewOverlay (eid, llabel)) =
    vBox
      [ padBottom (Pad 1) topBox,
        -- TODO try a vertical line instead of padding. There should be some example in brick
        hBox [padRight (Pad 5) leftBox, rightBox]
      ]
    where
      (label@(attr, dattr), ldattr) = llabel
      topBox =
        tbl
          [ [str "Title", str (name attr)],
            [str "EID", str (showEIDShort eid)]
          ]
      leftBox =
        tbl
          [ [str "Status", str (show $ status attr)],
            [str "Actionability", str (show $ llActionability llabel)],
            [str "Global Actionability", str (show $ glActionability label)],
            [str "Child Actionability", str (show $ daChildActionability dattr)],
            [str "Parent Actionability", str (show $ ldParentActionability ldattr)]
          ]
      rightBox =
        tbl
          -- TODO dates. needs the current time zone to render correctly
          [[str "Deadline", str ("todo")]]
      tbl =
        renderTable
          . surroundingBorder False
          . columnBorders False
          . rowBorders False
          . setDefaultColAlignment AlignLeft
          . table
          . map padFirstCell
      padFirstCell [] = []
      padFirstCell (h : t) = padRight (Pad 2) h : t

  handleEvent ctx (VtyEvent (EvKey key mods)) = do
    case kmzLookup kmz key mods of
      NotFound -> return ()
      -- We don't use sticky keys or submaps so 'nxt' is ignored.
      LeafResult act _nxt -> act ctx
      SubmapResult _nxt -> error "wtf"
  handleEvent _ _ = return ()

  componentKeyDesc _ = kmzDesc kmz

  componentTitle _ = "Item Details"
