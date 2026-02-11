module Srtd.Components.EditorBanana where

import Brick.Widgets.Edit
import Data.Text (Text)
import Reactive.Banana
import Srtd.BananaComponent
import Srtd.Component (AppResourceName)

data EditorBanana = EditorBanana
  { ebComponent :: AppComponentBanana Text Text
  , ebText :: Behavior Text
  }

instance HasAppComponentBanana (EditorBanana) Text Text where
  getAppComponentBanana = ebComponent

mkEditorBanana :: Text -> AppResourceName -> BananaMaker EditorBanana
mkEditorBanana t rname _actb eBrickEvent = do
  let initEditor = editorText rname (Just 1) t
  -- TODO WIP handleEditorEvent is the _stateful_ version! Uses EventM with an editor state!
  -- That is actually a big problem. Needs some message passing thing I think.
  -- Maybe we cannot make the whole component reactive actually. Event handling, then keymap, maybe has to be traditional imperative. :/
  -- But that would suck for composition I think. Maybe?
  bEditor <- accumB initEditor $ handleEditorEvent <$> eBrickEvent
  _todo
