{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

{-| A model defining a unified component interface. I have no idea why Brick doesn't include this.

The only place where we really need this is for overlays. We don't localize resource names or messages.
-}
module Srtd.Component where

import Brick
import Brick.BChan (BChan)
import Brick.Keybindings (Binding)
import Data.Text (Text)
import Data.Time (ZonedTime)
import Lens.Micro.Platform
import Srtd.Attr (EID)
import Srtd.Keymap (KeyDesc, KeymapItem, kmLeaf)
import Srtd.ModelServer (ModelServer, MsgModelUpdated)

-- SOMEDAY This is really messy. Would be way way better if it could be typed to what the caller is even doing.
-- I.e.: what they're calling; what they're expecting as a return value.
data OverlayReturnValue = ORNone | OREID EID deriving (Show)

-- TODO this we can refactor most of away.
data AppMsg
  = PopOverlay OverlayReturnValue
  | PushOverlay (AppResourceName -> SomeAppComponent)
  | PushTab (AppResourceName -> SomeAppComponent)
  | -- SOMEDAY PopOverlay and PopTab should be reconciled into a generic "pop active" msg so views can safely "pop themselves".
    PopTab
  | NextTab
  | PrevTab
  | ModelUpdated MsgModelUpdated
  | -- | A signal sent once per minute. Only needs to be handled for components where the internal
    -- state depends on the clock. (which typically means they cache data that depends on entered
    -- dates). Even then, blanket handlers (running on *every* event) are often fine.
    Tick

-- SOMEDAY we could make Show a precondition for BrickComponent or for SomeBrickComponent for better vis
-- This is a custom instance b/c some constructors' data doesn't have show and there are no sane defaults. :/
instance Show AppMsg where
  show (PopOverlay ret) = "PopOverlay(" ++ show ret ++ ")"
  show (PushOverlay _) = "PushOverlay _"
  show (PushTab _) = "PushTab _"
  show PopTab = "PopTab"
  show NextTab = "NextTab"
  show PrevTab = "PrevTab"
  show (ModelUpdated msg) = "ModelUpdated(" ++ show msg ++ ")"
  show Tick = "Tick"

-- Not super clean but I don't think I'll need a lot here. These nest to be unique across different tabs / overlays.
-- SOMEDAY maybe our resource names should just be lists of strings, aka. Brick attr names.
data AppResourceName
  = MainListFor AppResourceName
  | Overlay Int
  | Tab Int
  | TabTitleFor AppResourceName
  | EditorFor AppResourceName
  deriving (Eq, Ord, Show)

-- SOMEDAY consider passing this as an implicit parameter. Especially if we include "normal" app config in here.
-- (not sure if this would *actually* be cleaner: We then have to carry the type annotation around.)
data AppContext = AppContext
  { acModelServer :: ModelServer
  , acAppChan :: BChan AppMsg
  , acZonedTime :: ZonedTime
  -- ^ Current time and time zone at the time of processing this event. ONLY for user-facing
  -- interactions, NOT for internal process coordination!
  }

data AppEventReturn a b = Continue a | Confirmed b | Canceled

forgetAppEventReturnData :: AppEventReturn a b -> AppEventReturn () ()
forgetAppEventReturnData = \case
  Continue _ -> Continue ()
  Confirmed _ -> Confirmed ()
  Canceled -> Canceled

-- Somehow TemplateHaskell breaks everything here. Perhaps b/c of the circularity.
-- suffixLenses ''AppContext

acZonedTimeL :: Lens' AppContext ZonedTime
acZonedTimeL = lens acZonedTime (\ctx ztime -> ctx {acZonedTime = ztime})

-- TODO add type AppHandler s = AppContext -> EventM AppResourceName s ()
-- or AppHandler n s a or something.

-- | A class of components. All Brick components (hopefully,,,) satisfy this.
--
-- Parameters:
--
-- - `s` The state type of the component
-- - `a` The type of intermediate results that is returned when the component remains visible.
-- - `b` The type of final results when the component is closed.
--
-- Either `renderComponent` or `renderComponentWithOverlays` has to be implemented.
--
-- SOMEDAY is the intermediate result even needed? We could just inspect the component! And also for
-- the final result actually. (question is prob: does this have to be polymorphic, and I think the
-- answer is no.) So we could remove a and b. Not *such* a huge simplification but still.
class AppComponent s a b | s -> a, s -> b where
  -- | Render this component to a widget. Like `appRender` for apps, or the many render functions.
  renderComponent :: (?actx :: AppContext) => s -> Widget AppResourceName
  renderComponent = fst . renderComponentWithOverlays

  -- | Variant of `renderComponent` that lets us provide a list over overlays that should also be rendered.
  -- Overlays behave like Brick's 'appDraw', i.e. the first overlay is rendered at the top. Of form
  -- `(Title, Widget)`.
  renderComponentWithOverlays ::
    (?actx :: AppContext) => s -> (Widget AppResourceName, [(Text, Widget AppResourceName)])
  renderComponentWithOverlays = (,[]) . renderComponent

  -- | Handle an event. Like the `handleEvent` functions.
  --
  -- The return value gives the caller an intermediate or final result and tells them what to do
  -- with the component.
  handleEvent ::
    (?actx :: AppContext) =>
    BrickEvent AppResourceName AppMsg ->
    EventM AppResourceName s (AppEventReturn a b)

  -- | Give description of currently bound keys. You probably wanna use the Keymap module to generate these.
  componentKeyDesc :: s -> KeyDesc

  -- | Title of the component. Used in tabs and (should be used in) overlay titles.
  componentTitle :: s -> Text

-- | Wrapper that encapsulates any AppComponent and forgets results.
data SomeAppComponent = forall s a b. (AppComponent s a b) => SomeAppComponent s

instance AppComponent SomeAppComponent () () where
  renderComponent (SomeAppComponent s) = renderComponent s
  renderComponentWithOverlays (SomeAppComponent s) = renderComponentWithOverlays s

  -- This is a somewhat unhinged construction to deal with the existential quantification while
  -- using the `zoom` machinery. Wondering if there's a library function that just does what I want
  -- more directly (transform the state, keep all other layers intact)
  handleEvent ev = do
    (SomeAppComponent s) <- get
    -- Alternatively, we could make the actual transform and use unsafeCoerce. But this seems to work fine.
    let theLens = lens (const s) (const SomeAppComponent)
    res <- zoom theLens $ handleEvent ev
    return $ forgetAppEventReturnData res

  componentKeyDesc (SomeAppComponent s) = componentKeyDesc s

  componentTitle (SomeAppComponent s) = componentTitle s

-- * Keymap wrapper tools

-- | Helper type for keymaps. This must be a newtype (not type alias) so that we can avoid
-- ImpredicativeTypes, which can lead to ghc hangup (I've seen this with this particular code
-- before!)
--
-- This isn't used in this module but you can use it with a keymap.
newtype AppEventAction s a b = AppEventAction
  { runAppEventAction ::
      (?actx :: AppContext) =>
      EventM AppResourceName s (AppEventReturn a b)
  }

-- | Like 'kmLeaf' but wrap the given action in 'AppEventAction'
kmLeafA ::
  Binding ->
  Text ->
  -- NB For some reason, *this* use of the constraint inside the type doesn't need ImpredicativeTypes.
  ((?actx :: AppContext) => EventM AppResourceName s (AppEventReturn a b)) ->
  (Binding, KeymapItem (AppEventAction s a b))
kmLeafA b n x = kmLeaf b n (AppEventAction x)

-- | Like 'kmLeafA' but also return 'Continue ()'. Useful to simplify code for components that don't
-- return intermediate results.
kmLeafA_ ::
  Binding ->
  Text ->
  ((?actx :: AppContext) => EventM AppResourceName s a) ->
  (Binding, KeymapItem (AppEventAction s () b))
kmLeafA_ b n x = kmLeafA b n (x >> return (Continue ()))
