{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

{-| A model defining a unified component interface. I have no idea why Brick doesn't include this.

A Component is a piece of data that can be rendered to brick and supports a vaguely "call" like
interface where a parent can hold and call it and the component can tell it when it's done and
return a value (and/or pass a stream of intermediate values if desired). It also provides context
info (e.g., a list of supported keybindings) to the parent for rendering.
-}
module Srtd.Component where

import Brick
import Brick.BChan (BChan)
import Brick.Keybindings (Binding)
import Data.Text (Text)
import Data.Time (ZonedTime)
import Lens.Micro.Platform
import Srtd.Keymap (KeyDesc, KeymapItem, kmLeaf)
import Srtd.ModelServer (ModelServer, MsgModelUpdated)

-- | Global messages sent through Srtd at the app (top) level together with any brick events.
-- Individual components can define their own message type.
data AppMsg
  = -- SOMEDAY the first couple ones are for MainTree to communicate with Main. May not be needed, could just be the message returned by MainTree.
    -- (but let's keep them for now in case I have some funky interaction pattern later)
    PushTab (AppResourceName -> SomeAppComponent)
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
  show (PushTab _) = "PushTab _"
  show NextTab = "NextTab"
  show PrevTab = "PrevTab"
  show (ModelUpdated msg) = "ModelUpdated(" ++ show msg ++ ")"
  show Tick = "Tick"

-- | Global type for Brick "resource names", which are used to detect clicks (and also visibility/scroll, but I'm not using that right now). This can be anything but resource names need to be globally unique.
--
-- NB tabs, overlays, etc. are _not_ numbered consecutively but the Int is to ensure uniqueness only.
--
-- Not super clean but I don't think I'll need a lot here. These nest to be unique across different tabs / overlays.
-- SOMEDAY maybe our resource names should just be lists of strings, aka. Brick attr names.
data AppResourceName
  = MainListFor AppResourceName
  | OverlayFor Int AppResourceName
  | Tab Int
  | TabTitleFor AppResourceName
  | EditorFor AppResourceName
  deriving (Eq, Ord, Show)

-- | App conext passed down from the app (top) level to components that need it.
data AppContext = AppContext
  { acModelServer :: ModelServer
  , acAppChan :: BChan AppMsg
  , acZonedTime :: ZonedTime
  -- ^ Current time and time zone at the time of processing this event. ONLY for user-facing
  -- interactions, NOT for internal process coordination!
  }

-- | Return data returned by 'handleEvent' (see below) to tell the parent component if the child
-- component should be kept around.
data AppEventReturn a b
  = -- | Event processed successfully and the component should be kept open, returns an intermediate
    -- result of type `a`. (choose `a = ()`) for components that don't return any intermediate result,
    -- which are most of them.
    Continue a
  | -- | Event processed successfully and the user has confirmed whatever action encoded.
    -- The component should be closed.
    Confirmed b
  | -- | Either the user has pressed the 'cancel' key (often ESC) _or_ there was some kind of error,
    -- and the component should be closed without a result produced or action taken.
    --
    -- NB we currently don't differentiate between "user cancel" and "error". SOMEDAY maybe
    -- we should do that, though currently the parent can't really take useful actions based on the
    -- distinction.
    Canceled

forgetAppEventReturnData :: AppEventReturn a b -> AppEventReturn () ()
forgetAppEventReturnData = \case
  Continue _ -> Continue ()
  Confirmed _ -> Confirmed ()
  Canceled -> Canceled

-- Somehow TemplateHaskell breaks everything here. Perhaps b/c of the circularity.
-- suffixLenses ''AppContext

acZonedTimeL :: Lens' AppContext ZonedTime
acZonedTimeL = lens acZonedTime (\ctx ztime -> ctx {acZonedTime = ztime})

-- | A class of components. All Brick components (hopefully,,,) satisfy this.
--
-- Parameters:
--
-- - `s` The state type of the component
-- - `a` The type of intermediate messages that is passed to the parent/caller when the component
--   remains visible.
-- - `b` The type of final messages that is passed to the parent/caller when the component is
--   closed.
--
-- Either `renderComponent` or `renderComponentWithOverlays` has to be implemented.
class AppComponent s a b | s -> a, s -> b where
  -- | Render this component to a widget. Like `appRender` for apps, or the many render functions.
  renderComponent :: (?actx :: AppContext) => s -> Widget AppResourceName
  renderComponent = fst . renderComponentWithOverlays

  -- | Variant of `renderComponent` that lets us provide a list of overlays that should also be rendered.
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
  ((?actx :: AppContext) => EventM AppResourceName s ()) ->
  (Binding, KeymapItem (AppEventAction s () b))
-- NB this is one of the few cases where we can't make this point-free b/c the definition of
-- 'aerVoid' doesn't include the `?actx` constraint.
kmLeafA_ b n x = kmLeafA b n (aerVoid x)

-- | Return `Continue ()`.
aerContinue :: (Monad m) => m (AppEventReturn () b)
aerContinue = return $ Continue ()

-- | Variant of 'void' for the (common) case where there's no intermediate result.
aerVoid :: (Monad m) => m a -> m (AppEventReturn () b)
aerVoid act = act >> aerContinue
