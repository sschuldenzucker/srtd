{-# LANGUAGE ExistentialQuantification #-}

-- | A model defining a unified component interface. I have no idea why Brick doesn't include this.
--
-- The only place where we really need this is for overlays. We don't localize resource names or messages.
module Component where

import AppMsg
import Brick
import Lens.Micro.Platform (lens)

-- | A class of components. All Brick components (hopefully,,,) satisfy this.
--
-- TODO rename to AppComponent b/c it's really specific to *this* app by now.
class BrickComponent s where
  -- | Render this component to a widget. Like `appRender` for apps, or the many render functions.
  renderComponent :: s -> Widget AppResourceName

  -- | Handle an event. Like the `handleEvent` functions.
  handleEvent :: AppContext -> BrickEvent AppResourceName AppMsg -> EventM AppResourceName s AppMsg

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
