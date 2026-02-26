{-# LANGUAGE ExistentialQuantification #-}
-- For the MonadWriter instance of ComponentEventM. It's fine!!
{-# LANGUAGE UndecidableInstances #-}

{-| A model defining a unified component interface. I have no idea why Brick doesn't include this.

A Component is a piece of data that can be rendered to brick and supports a vaguely "call" like
interface where a parent can hold and call it and the component can tell it when it's done and
return a value (and/or pass a stream of intermediate events if desired). It also provides context
info (e.g., a list of supported keybindings) to the parent for rendering.

This is very similar to [brick-panes](https://github.com/kquick/brick-panes), though more
specialized to what I really need:

- Types are not generic but refer to the actual resource name type etc. that I use.
- I integrate the 'AppEventReturn' type, which gives feedback to the parent component in a paricular way.
- I integrate overlays and key binding help in the way I use it.
-}
module Srtd.Component where

import Brick
import Brick.BChan (BChan)
import Brick.Keybindings (Binding)
import Control.Monad.Except
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT))
import Control.Monad.State (MonadState)
import Control.Monad.Writer.Strict
import Data.List qualified as L
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (ZonedTime)
import Data.Void (Void)
import Graphics.Vty.Input (Key (KBS, KEsc), Modifier)
import Lens.Micro.Platform
import Srtd.Keymap (
  KeyDesc,
  Keymap,
  KeymapItem,
  KeymapResult (..),
  KeymapZipper,
  kmLeaf,
  kmLookup,
  kmzIsToplevel,
  kmzLookup,
  kmzResetRoot,
  kmzUp,
 )
import Srtd.Log (Priority (..), glogL)
import Srtd.Model (FilterContext (..), IdNotFoundError)
import Srtd.ModelServer (ModelServer, MsgModelUpdated)
import Srtd.MonadBrick (MonadBrick (..))
import Srtd.Util (ALens' (..), captureWriterT)

-- | Messages that reach the root of the app. They do not recur into individual components.
--
-- SOMEDAY these messages could also be returned as child events from components up the tree. But
-- I'm not doing that right now.
data AppRootMsg
  = PushTab (AppResourceName -> SomeAppComponent)
  | NextTab
  | PrevTab
  | SwapTabNext
  | SwapTabPrev
  | AppComponentMsg AppComponentMsg

-- | Messages that reach individual components and cascade hierarchically.
data AppComponentMsg
  = ModelUpdated MsgModelUpdated
  | -- | A signal sent once per minute. Only needs to be handled for components where the internal
    -- state depends on the clock. (which typically means they cache data that depends on entered
    -- dates). Even then, blanket handlers (running on *every* event) are often fine.
    Tick
  deriving (Show)

-- SOMEDAY we could make Show a precondition for BrickComponent or for SomeBrickComponent for better vis
-- This is a custom instance b/c some constructors' data doesn't have show and there are no sane defaults. :/
instance Show AppRootMsg where
  show (PushTab _) = "PushTab _"
  show NextTab = "NextTab"
  show PrevTab = "PrevTab"
  show SwapTabNext = "SwapTabNext"
  show SwapTabPrev = "SwapTabPrev"
  show (AppComponentMsg msg) = "AppComponentMsg (" ++ show msg ++ ")"

-- | Global type for Brick "resource names", which are used to detect clicks (and also
-- visibility/scroll, but I'm not using that right now). This (1) needs to be globally unique for brick
-- widgets and (2) needs to follow the AppComponent hierarchy to tell them where to route.
--
-- NB tabs, overlays, etc. are _not_ numbered consecutively but the Int is to ensure uniqueness only.
--
-- The list is in hierarchical order with toplevel hierarchy *first*.
--
-- SOMEDAY this might be a bit slow for applications that need a lot of throughput, like scrolling. Not clear to me.
newtype AppResourceName = AppResourceName {unAppResourceName :: [PrimitiveAppResourceName]}
  deriving (Eq, Ord, Show, Semigroup)

data PrimitiveAppResourceName
  = NamedAppResource Text Int
  | TabTitleFor AppResourceName
  deriving (Eq, Ord, Show)

instance IsString AppResourceName where
  fromString s = AppResourceName [NamedAppResource (T.pack s) 0]

isPrefixOf :: AppResourceName -> AppResourceName -> Bool
(AppResourceName s) `isPrefixOf` (AppResourceName t) = s `L.isPrefixOf` t

-- | App conext passed down from the app (top) level to components that need it.
data AppContext = AppContext
  { acModelServer :: ModelServer
  , acAppChan :: BChan AppRootMsg
  , acZonedTime :: ZonedTime
  -- ^ Current time and time zone at the time of processing this event. ONLY for user-facing
  -- interactions, NOT for internal process coordination!
  }

translateAppFilterContext :: AppContext -> ((?fctx :: FilterContext) => a) -> a
translateAppFilterContext actx x =
  let ?fctx = FilterContext {fcZonedTime = acZonedTime $ actx}
   in x

-- | Return data returned by 'handleEvent' (see below) to tell the parent component if the child
-- component should be kept around.
--
-- NB this is obviously a functor and could be made a monad, but we don't provide these instances
-- for now b/c they don't seem super useful.
data AppEventReturn b
  = -- | Event processed successfully and the component should be kept open.
    Continue
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

forgetAppEventReturnData :: AppEventReturn b -> AppEventReturn ()
forgetAppEventReturnData = \case
  Continue -> Continue
  Confirmed _ -> Confirmed ()
  Canceled -> Canceled

-- Somehow TemplateHaskell breaks everything here. Perhaps b/c of the circularity.
-- suffixLenses ''AppContext

acZonedTimeL :: Lens' AppContext ZonedTime
acZonedTimeL = lens acZonedTime (\ctx ztime -> ctx {acZonedTime = ztime})

-- | Shorthand for MonadBrick constraints in the app
type AppMonadBrick s m = MonadBrick AppResourceName s m

-- | Monad for event handlers
--
-- This is Brick's 'EventM' + ability to write events + ability to read AppContext
newtype ComponentEventM s a = ComponentEventM
  {runComponentEventM :: ReaderT AppContext (WriterT [Event s] (EventM AppResourceName s)) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState s
    , MonadReader AppContext
    , MonadBrick AppResourceName s
    )

-- | Type of component event handlers
type ComponentEventM' s = ComponentEventM s (AppEventReturn (Return s))

-- Hack around limitations: type synonyms not allowed in instance heads (to ensure consistency). Fine here.
-- Requires UndecidableInstances.
-- Needs a manual instance.
instance (e ~ [Event s]) => MonadWriter e (ComponentEventM s) where
  tell x = ComponentEventM $ tell x
  listen = ComponentEventM . listen . runComponentEventM
  pass = ComponentEventM . pass . runComponentEventM

-- | Run a 'ComponentEventM' down to its inner 'EventM' action
runComponentEventM' :: AppContext -> ComponentEventM s a -> EventM AppResourceName s (a, [Event s])
runComponentEventM' actx = runWriterT . flip runReaderT actx . runComponentEventM

-- NOTE: There is *no* instance for Zoom on ComponentEventM. This is b/c ComponentEventM
-- ties together the event type and the state and this monad is generally not zoomable. Use
-- callIntoComponentEventM or, if needed, nestComponentEventM, or walk into the stack itself (you
-- probably won't need to do that though)

-- | Run a 'ComponentEventM' with explicitly provided state, returning explicit events. Like
-- `nestEventM` but for ComponentEventM.
nestComponentEventM ::
  s -> ComponentEventM s a -> ComponentEventM t (s, (a, [Event s]))
nestComponentEventM s act = do
  actx <- ask
  let act' = runWriterT $ runReaderT (runComponentEventM act) actx
  liftEventM $ nestEventM s $ act'

-- | Run a 'ComponentEventM' action in a child component, and return produced events.
--
-- Generally 'callIntoComponentEventM' is preferred for simple use cases, but you can use this to
-- process events in another monad, for instance.
--
-- This is not like 'zoom' b/c the return type is different
zoomComponentEventM :: Lens' t s -> ComponentEventM s a -> ComponentEventM t (a, [Event s])
zoomComponentEventM l act = do
  -- SOMEDAY should we use zoom here with an unwrapper?
  s <- use l
  (s', ret) <- nestComponentEventM s act
  l .= s'
  return ret

-- | Given a lens and an event handler, run an action on a child component and handle events.
--
-- Use this instead of `zoom` when calling into another component.
callIntoComponentEventM ::
  Lens' t s -> (Event s -> ComponentEventM t ()) -> ComponentEventM s a -> ComponentEventM t a
callIntoComponentEventM l h act = do
  (ret, events) <- zoomComponentEventM l act
  mapM_ h events
  return ret

-- | A class of components. All Brick components (hopefully,,,) satisfy this.
--
-- Parameters:
--
-- - `s` The state type of the component
-- - `b` The type of final messages that is passed to the parent/caller when the component is
--   closed.
--
-- Either `renderComponent` or `renderComponentWithOverlays` has to be implemented.
class AppComponent s where
  type Return s

  -- TODO should this be data?
  type Event s

  -- | Render this component to a widget. Like `appRender` for apps, or the many render functions.
  --
  -- NB Different from 'handleEvent', this uses an implicit param b/c the app context (and really
  -- just the current time) is sometimes but very rarely used for rendering. Implicit params are
  -- convenient here.
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
  -- The return value gives the caller final result (if Confirmed) and tells them what to do with
  -- the component.
  handleEvent ::
    BrickEvent AppResourceName AppComponentMsg ->
    ComponentEventM' s

  -- | Give description of currently bound keys. You probably wanna use the Keymap module to generate these.
  componentKeyDesc :: s -> KeyDesc

  -- | Title of the component. Used in tabs and (should be used in) overlay titles.
  componentTitle :: s -> Text

-- | Wrapper that encapsulates any AppComponent and forgets results and events. (!)
data SomeAppComponent = forall s. (AppComponent s) => SomeAppComponent s

instance AppComponent SomeAppComponent where
  type Return SomeAppComponent = ()
  type Event SomeAppComponent = Void

  renderComponent (SomeAppComponent s) = renderComponent s
  renderComponentWithOverlays (SomeAppComponent s) = renderComponentWithOverlays s

  -- This is a somewhat unhinged construction to deal with the existential quantification while
  -- using the `zoom` machinery. Wondering if there's a library function that just does what I want
  -- more directly (transform the state, keep all other layers intact)
  handleEvent ev = do
    (SomeAppComponent s) <- get
    -- Alternatively, we could make the actual transform and use unsafeCoerce. But this seems to work fine.
    -- For some reason, I need ALens' if I wanna put this into a variable, else it complains about
    -- an ambiguous functor thingy f0. When I just replace the `lens` expression below, I don't need
    -- ALens'. Whatever...
    let theLens = ALens' $ lens (const s) (const SomeAppComponent)
    -- This ignores events! And also the Confirmed value.
    -- (res, _events) <- zoom theLens $ captureWriterT $ handleEvent ev
    res <- callIntoComponentEventM (runALens' theLens) (const $ return ()) $ handleEvent ev
    return $ forgetAppEventReturnData res

  componentKeyDesc (SomeAppComponent s) = componentKeyDesc s

  componentTitle (SomeAppComponent s) = componentTitle s

-- * Keymap wrapper tools

-- TODO should this be our monad of choice??

-- | Like 'kmLeaf' but also return 'Continue'.
kmLeaf_ :: (Monad m) => Binding -> Text -> m () -> (Binding, KeymapItem (m (AppEventReturn b)))
kmLeaf_ b n x = kmLeaf b n (aerVoid x)

-- | Execute action and return Continue
aerVoid :: (Monad m) => m () -> m (AppEventReturn b)
aerVoid act = act >> return Continue

-- | Common dispatch routine for keymaps. Given a lens where we store a KeymapZipper, look up,
-- execute, and update the keymap, or execute a fallback.
kmzDispatch ::
  -- | Lens where the KeymapZipper is stored
  Lens' s (KeymapZipper (ComponentEventM' s)) ->
  -- | Pressed key from VtyEvent
  Key ->
  -- | Pressed modifiers from VtyEvent
  [Modifier] ->
  -- | Fallback action if no key matches
  ComponentEventM' s ->
  ComponentEventM' s
kmzDispatch l key mods fallback = do
  kmz <- use l
  case kmzLookup kmz key mods of
    NotFound ->
      if
        | key == KEsc && mods == [] && not (kmzIsToplevel kmz) -> aerVoid $ l %= kmzResetRoot
        | key == KBS && mods == [] && not (kmzIsToplevel kmz) -> aerVoid $ l %= kmzUp
        | otherwise -> fallback
    LeafResult act nxt -> do
      res <- act
      l .= nxt
      return res
    SubmapResult nxt -> do
      l .= nxt
      return Continue

-- | 'kmzDispatch' when we don't have submaps and don't track state.
--
-- Errors and ignores if a submap is found.
kmDispatch ::
  -- | Input keymap
  Keymap (ComponentEventM' s) ->
  -- | Pressed key from VtyEvent
  Key ->
  -- | Pressed modifiers from VtyEvent
  [Modifier] ->
  -- | Fallback action if no key matches
  ComponentEventM' s ->
  ComponentEventM' s
kmDispatch km key mods fallback = case kmLookup km key mods of
  NotFound -> fallback
  -- We ignore nxt, which tells us whether the keymap is sticky: without a zipper, it will always be.
  LeafResult act _nxt -> act
  SubmapResult _nxt -> do
    liftIO $
      glogL ERROR $
        "kmDispatch found submap result at " ++ show (key, mods) ++ " but cannot handle it. Ignoring."
    return Continue

-- * Error Handling

type ComponentEventMOrNotFound s a =
  ExceptT IdNotFoundError (ComponentEventM s) a

-- | Like 'callIntoComponentEventM' but with actions and handlers that may fail.
--
-- Specific to IdNotFoundError over other error types for no reason.
--
-- NB you can also apply this to an input action that _cannot_ fail (of type `ComponentEventM`) by
-- simply wrapping it in `lift`. The important bit here is that the _handler_ may fail.
callIntoComponentEventMOrNotFound ::
  Lens' t s ->
  (Event s -> ComponentEventMOrNotFound t ()) ->
  ComponentEventMOrNotFound s a ->
  ComponentEventMOrNotFound t a
callIntoComponentEventMOrNotFound l h act = do
  (ret, events) <- lift $ zoomComponentEventM l (runExceptT act)
  mapM_ h events
  liftEither ret

-- | Convert exception handling.
notFoundToAER_ :: (Monad m) => ExceptT IdNotFoundError m () -> m (AppEventReturn b)
notFoundToAER_ = notFoundToAER . aerVoid

-- | Merge exception handling.
--
-- An exception is treated equivalent to returning 'Canceled'.
notFoundToAER :: (Monad m) => ExceptT IdNotFoundError m (AppEventReturn b) -> m (AppEventReturn b)
notFoundToAER act = do
  eres <- runExceptT act
  case eres of
    Left _err -> return Canceled
    Right res -> return res
