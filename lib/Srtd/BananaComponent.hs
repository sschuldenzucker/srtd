{-# LANGUAGE FunctionalDependencies #-}

module Srtd.BananaComponent where

import Brick
import Brick.BChan (BChan)
import Data.IORef
import Data.Text (Text)
import Data.Time (ZonedTime)
import Lens.Micro.Platform
import Reactive.Banana
import Reactive.Banana.Frameworks
import Srtd.Component
import Srtd.Keymap
import Srtd.ModelServer (ModelServer)

data BananaComponentRuntime a b = BananaComponentRuntime
  { bcrtObserveWidget :: IO (Widget AppResourceName)
  , bcrtObserveKeyDesc :: IO KeyDesc
  , bcrtObserveTitle :: IO Text
  , bcrtTriggerEvent :: BrickEvent AppResourceName AppMsg -> IO ()
  , bcrtObserveTriggerEventReturn :: IO (AppEventReturn a b)
  , bcrtUpdateZonedTime :: ZonedTime -> IO ()
  , bcrtNetwork :: EventNetwork
  }

data BananaComponent a b = BananaComponent
  { bcRuntime :: BananaComponentRuntime a b
  , bcCachedWidget :: Widget AppResourceName
  , bcCachedKeyDesc :: KeyDesc
  , bcCachedTitle :: Text
  }

suffixLenses ''BananaComponentRuntime
suffixLenses ''BananaComponent

-- TODO LATER shutdown via pause, needs to be a supported method in AppComponent. I think.

instance AppComponent (BananaComponent a b) a b where
  -- TODO LATER add overlays (easy)
  renderComponent = bcCachedWidget

  componentKeyDesc = bcCachedKeyDesc

  componentTitle = bcCachedTitle

  handleEvent ev = do
    s <- get
    let rt = bcRuntime s
    res <- liftIO $ do
      -- order matters!
      bcrtUpdateZonedTime rt $ acZonedTime ?actx
      bcrtTriggerEvent rt ev
      bcrtObserveTriggerEventReturn rt

    (bcCachedWidgetL .=) =<< liftIO (bcrtObserveWidget rt)
    (bcCachedKeyDescL .=) =<< liftIO (bcrtObserveKeyDesc rt)
    (bcCachedTitleL .=) =<< liftIO (bcrtObserveTitle rt)

    return res

-- TODO WIP include app context

data AppContextBanana = AppContextBanana
  { acbModelServer :: ModelServer
  , acbAppChan :: BChan AppMsg
  , acbZonedTime :: Behavior ZonedTime
  }

mkAppContextBanana :: AppContext -> AddHandler ZonedTime -> MomentIO (AppContextBanana)
mkAppContextBanana actx addZonedTimeHandler = do
  bZonedTime <- fromChanges (acZonedTime actx) addZonedTimeHandler
  return
    AppContextBanana
      { acbModelServer = acModelServer actx
      , acbAppChan = acAppChan actx
      , acbZonedTime = bZonedTime
      }

type AppEvent = BrickEvent AppResourceName AppMsg

-- TODO use this more
data AppComponentBanana a b = AppComponentBanana
  { acbWidget :: Behavior (Widget AppResourceName)
  , acbKeyDesc :: Behavior KeyDesc
  , acbTitle :: Behavior Text
  , -- TODO the old discussion if Continue should even exist.
    -- This is nasty here b/c it's a weird initialization point.
    -- It must be a behavior to avoid deadlocks.
    acbAppEventReturn :: Behavior (AppEventReturn a b)
  }

type BananaMaker a = AppContextBanana -> Event AppEvent -> MomentIO a

type BananaComponentMaker a b = BananaMaker (AppComponentBanana a b)

-- TODO use this
class HasAppComponentBanana s a b | s -> a b where
  getAppComponentBanana :: s -> AppComponentBanana a b

mkBananaComponentRuntime ::
  (?actx :: AppContext) => BananaComponentMaker a b -> IO (BananaComponentRuntime a b)
mkBananaComponentRuntime go = do
  (addBrickEventHandler, triggerBrickEvent) <- newAddHandler
  (addZonedTimeHandler, triggerZonedTime) <- newAddHandler

  -- NB we write these with the first observed value below, so this is never read.
  rWidget <- newIORef (error "not initialized")
  rKeyDesc <- newIORef (error "not initialized")
  rTitle <- newIORef (error "not initialized")
  rRes <- newIORef (error "not initialized")

  network <- compile $ do
    eBrickEvent <- fromAddHandler addBrickEventHandler
    actb <- mkAppContextBanana ?actx addZonedTimeHandler

    (AppComponentBanana bWidget bKeyDesc bTitle bRes) <- go actb eBrickEvent

    -- previous version: only update when brick events arrive. May be brittle.
    -- reactimate $ writeIORef rWidget . Just <$> bWidget <@ eBrickEvent
    syncBehaviorIntoIORef bWidget rWidget
    syncBehaviorIntoIORef bKeyDesc rKeyDesc
    syncBehaviorIntoIORef bTitle rTitle
    syncBehaviorIntoIORef bRes rRes

  actuate network

  return $
    BananaComponentRuntime
      { bcrtObserveWidget = readIORef rWidget
      , bcrtObserveKeyDesc = readIORef rKeyDesc
      , bcrtObserveTitle = readIORef rTitle
      , bcrtTriggerEvent = triggerBrickEvent
      , bcrtObserveTriggerEventReturn = readIORef rRes
      , bcrtUpdateZonedTime = triggerZonedTime
      , bcrtNetwork = network
      }

mkBananaComponent :: (?actx :: AppContext) => BananaComponentMaker a b -> IO (BananaComponent a b)
mkBananaComponent go = do
  rt <- mkBananaComponentRuntime go
  widget <- bcrtObserveWidget rt
  keyDesc <- bcrtObserveKeyDesc rt
  title <- bcrtObserveTitle rt
  return $ BananaComponent rt widget keyDesc title

-- Should be run during compile.
syncBehaviorIntoIORef :: Behavior a -> IORef a -> MomentIO ()
syncBehaviorIntoIORef b r = do
  -- TODO correct semantics for valueB?
  valueB b >>= liftIO . writeIORef r
  eb <- changes b
  reactimate' $ writeIORef r `fmapFuture` eb

fmapFuture :: (a -> b) -> Event (Future a) -> Event (Future b)
fmapFuture = fmap . fmap
