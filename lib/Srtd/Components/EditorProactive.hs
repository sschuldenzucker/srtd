{-| A thin wrapper around Brick's Editor component

Not intended to be used on its own. Always Continue's.
-}
module Srtd.Components.EditorProactive (
  -- * Types
  EditorProactive (..),
  EditorProactiveEvent (..),

  -- * Construction
  editorProactiveText,

  -- * Access
  getEditorText,

  -- * Modification
  applyEdit,
  setPostRender,
) where

import Brick
import Brick.Keybindings
import Brick.Widgets.Edit hiding (applyEdit)
import Brick.Widgets.Edit qualified as E
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Zipper (TextZipper)
-- import Data.Text.Zipper qualified as TZ
import Data.Text.Zipper.Generic.Words qualified as TZ
import Lens.Micro.Platform
import Srtd.BrickHelpers (pattern SomeNonVtyKeyBrickEvent, pattern VtyKeyEvent)
import Srtd.Component
import Srtd.Keymap
import Srtd.MonadBrick
import Srtd.ProactiveBandana
import Srtd.Util (tell1)

data EditorProactive = EditorProactive
  { epEditor :: Editor Text AppResourceName
  , epText :: Cell' Text (AppEventM EditorProactive ())
  , epPostRender :: Widget AppResourceName -> Widget AppResourceName
  -- ^ For applying attrs to rendering only. Could also just be an attr state but whatever
  }

suffixLenses ''EditorProactive

editorProactiveText :: Text -> AppResourceName -> EditorProactive
editorProactiveText initText rname =
  EditorProactive
    { epEditor = editor rname (Just 1) initText
    , epText = uniqueCellM initText $ \t' -> tell1 (TextChanged t')
    , epPostRender = id
    }

{- editorProactiveString :: String -> AppResourceName -> EditorProactive
editorProactiveString initString rname =
  EditorProactive
    { epEditor = editor rname (Just 1) initString
    } -}

setPostRender ::
  (Widget AppResourceName -> Widget AppResourceName) -> AppEventM EditorProactive ()
setPostRender f = epPostRenderL .= f

data EditorProactiveEvent t = TextChanged t

-- SOMEDAY does this already exist? It kinda should
class RenderStringLike s where
  renderStringLike :: s -> Widget n
  renderStringLikes :: [s] -> Widget n

instance RenderStringLike String where
  renderStringLike = str
  renderStringLikes = str . intercalate "\n"

instance RenderStringLike Text where
  renderStringLike = txt
  renderStringLikes = txt . T.intercalate "\n"

-- TODO generic version that also supports String. Had some issues there.
-- ... or don't and get rid of RenderStringLike

getEditorText :: EditorProactive -> Text
getEditorText = cValue . epText

applyEdit :: (TextZipper Text -> TextZipper Text) -> AppEventM EditorProactive ()
applyEdit f = do
  liftEventM $ epEditorL %= E.applyEdit f
  updateTextCellFromEditor

updateTextCellFromEditor :: AppEventM EditorProactive ()
updateTextCellFromEditor = do
  t <- liftEventM $ (T.intercalate "\n" . getEditContents) <$> gets epEditor
  runUpdateLens epTextL t

keymap :: Keymap (AppEventAction EditorProactive)
keymap =
  kmMake
    "Editor"
    [kmLeafA_ (ctrl 'w') "Delete word" $ applyEdit TZ.deletePrevWord]

instance AppComponent (EditorProactive) where
  type Return EditorProactive = ()
  type Event EditorProactive = EditorProactiveEvent Text

  -- TODO take 'has focus' into account. (currently always yes; this is ok *here for now* but not generally) (prob warrants a param)
  renderComponent s = renderEditor renderStringLikes True (epEditor s)

  handleEvent ev = case ev of
    VtyKeyEvent key mods -> kmDispatch keymap key mods handleFallback
    SomeNonVtyKeyBrickEvent -> handleFallback
    AppEvent _ -> return Continue
   where
    handleFallback = do
      t' <- liftEventM $ do
        zoom epEditorL $ handleEditorEvent ev
        (T.intercalate "\n" . getEditContents) <$> gets epEditor
      runUpdateLens epTextL t'
      return Continue

  componentKeyDesc _s = KeyDesc "Editor" True []
  componentTitle _s = "Editor"
