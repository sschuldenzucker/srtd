{-# LANGUAGE ExistentialQuantification #-}

-- | A model defining a unified component interface. I have no idea why Brick doesn't include this.
--
-- The only place where we really need this is for overlays. We don't localize resource names or messages.
module Component where

import Attr (EID)
import Brick
import Brick.BChan (BChan)
import Data.Text (Text)
import Lens.Micro.Platform (lens)
import ModelServer (ModelServer, MsgModelUpdated)

-- SOMEDAY This is really messy. Would be way way better if it could be typed to what the caller is even doing.
-- I.e.: what they're calling; what they're expecting as a return value.
data OverlayReturnValue = ORNone | OREID EID deriving (Show)

data AppMsg = PopOverlay OverlayReturnValue | PushOverlay (AppResourceName -> SomeBrickComponent) | ModelUpdated MsgModelUpdated

-- SOMEDAY we could make Show a precondition for BrickComponent or for SomeBrickComponent for better vis
instance Show AppMsg where
  show (PopOverlay ret) = "PopOverlay(" ++ show ret ++ ")"
  show (PushOverlay _) = "PushOverlay _"
  show (ModelUpdated msg) = "ModelUpdated(" ++ show msg ++ ")"

-- Not super clean but I don't think I'll need a lot here.
data AppResourceName = MainList | Overlay Int | EditorFor AppResourceName deriving (Eq, Ord, Show)

data AppContext = AppContext
  { acModelServer :: ModelServer,
    acAppChan :: BChan AppMsg
  }

-- | A class of components. All Brick components (hopefully,,,) satisfy this.
--
-- TODO rename to AppComponent b/c it's really specific to *this* app by now.
-- TODO maybe generalize over types if only to avoid circularities grrr
class BrickComponent s where
  -- | Render this component to a widget. Like `appRender` for apps, or the many render functions.
  renderComponent :: s -> Widget AppResourceName

  -- | Handle an event. Like the `handleEvent` functions.
  handleEvent :: AppContext -> BrickEvent AppResourceName AppMsg -> EventM AppResourceName s ()

  -- | Give description of currently bound keys. You probably wanna use the Keymap module to generate these.
  --
  -- (is toplevel, descriptions)
  componentKeyDesc :: s -> (Bool, [(Text, Text)])

data SomeBrickComponent = forall s. (BrickComponent s) => SomeBrickComponent s

instance BrickComponent SomeBrickComponent where
  renderComponent (SomeBrickComponent s) = renderComponent s

  -- This is a somewhat unhinged construction to deal with the existential quantification while
  -- using the `zoom` machinery. Wondering if there's a library function that just does what I want
  -- more directly (transform the state, keep all other layers intact)
  handleEvent ctx ev = do
    (SomeBrickComponent s) <- get
    -- Alternatively, we could make the actual transform and use unsafeCoerce. But this seems to work fine.
    let theLens = lens (const s) (const SomeBrickComponent)
    zoom theLens $ handleEvent ctx ev

  componentKeyDesc (SomeBrickComponent s) = componentKeyDesc s
