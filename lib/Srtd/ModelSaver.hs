{-# LANGUAGE RecordWildCards #-}

-- | A process that auto-saves the model. Import qualified.
module Srtd.ModelSaver (ModelSaver, startModelSaver, linkModelSaver, exitGracefully) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (mask_)
import Control.Monad (unless, when)
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy qualified as B
import Data.Maybe (isJust)
import Srtd.Config (model_filename)
import Srtd.Log (Priority (..), glogL)
import Srtd.Model (Model, modelToDiskModel)
import Srtd.ModelServer qualified as MS
import Srtd.Util (whenJust)
import System.Clock qualified as C
import System.Timeout (timeout)

data ModelSaver = ModelSaver
  { msAsync :: Async ()
  , msIsDirtyV :: TVar Bool
  , msShallQuitV :: TVar Bool
  }

data State = State
  { lastSaveTime :: Maybe C.TimeSpec
  -- ^ NB this will be Just except on the first run.
  }

-- | Max microsecond delay before saving a changed model to the file, in case of throttling. Note
-- that writes are instant unless we're throttling.
saveThrottleMicros :: (Integral a) => a
saveThrottleMicros = 2 * 1_000_000

-- | Max timeout for gracefully quitting the server. Must be > saveThrottleMicros (see below)
quitTimeoutMicros :: (Integral a) => a
quitTimeoutMicros = 5 * 1_000_000

-- | Spawns and returns an async model saver thread, running infinitely long. It's unlinked to the
-- parent by default. Use `linkModelServer` to link.
startModelSaver :: MS.ModelServer -> IO ModelSaver
startModelSaver mserver = do
  msIsDirtyV <- newTVarIO False
  msShallQuitV <- newTVarIO False
  msAsync <- async $ server mserver msIsDirtyV msShallQuitV (State Nothing)
  let saver = ModelSaver {..}
  MS.subscribe mserver (const $ notify saver)
  return saver

notify :: ModelSaver -> IO ()
notify ModelSaver {msIsDirtyV} = atomically $ writeTVar msIsDirtyV True

-- | Link a model saver to the current thread.
linkModelSaver :: ModelSaver -> IO ()
linkModelSaver ModelSaver {msAsync} = link msAsync

server :: MS.ModelServer -> TVar Bool -> TVar Bool -> State -> IO ()
server mserver isDirtyV shallQuitV s_ = go s_
 where
  go s@State {lastSaveTime} = do
    (isDirty, shallQuit) <- atomically $ do
      isDirty <- readTVar isDirtyV
      shallQuit <- readTVar shallQuitV
      check $ isDirty || shallQuit
      return (isDirty, shallQuit)
    if shallQuit
      then when isDirty (doSave =<< MS.getModel mserver)
      else do
        whenJust lastSaveTime $ \lst -> do
          now <- C.getTime C.Monotonic
          let waitMicros = saveThrottleMicros - (C.toNanoSecs (C.diffTimeSpec now lst) `div` 1_000)
          -- NB this won't be interrupted by a quit signal. If we care about that, we should do a `race` against reading `check =<< shallQuitV`.
          when (waitMicros > 0) $ threadDelay (fromIntegral waitMicros)
        -- Order matters! We set it here so that dirty signals during the wait are ignored (we will
        -- save!) but those during the save are considered. Atomicity is nice but not actually needed.
        model <- atomically $ do
          writeTVar isDirtyV False
          MS.getModelSTM mserver
        doSave model
        now <- C.getTime C.Monotonic
        go $ s {lastSaveTime = Just now}
  doSave model = do
    glogL DEBUG $ "Writing " ++ model_filename
    writeModelToFile model

-- | Attempt to exit the modelsaver gracefully or kill it hard after a timeout (which is then
-- a problem).
--
-- Graceful shutdown avoids two issues (these are not hypothetical! I've seen this happen!):
-- 1. Remaining dirty state is written to the file so no changes are lost.
-- 2. When the saver is just killed (e.g., on exit) during write, data is corrupted. O_O
--
-- SOMEDAY should we refuse to kill it hard, for safety?
--
-- SOMEDAY could be simpler & safer to trap the cancel exception and bracket the write. Then the
-- parent can just cancel (but it * cannot * just end !) and no explicit quit signal is needed.
exitGracefully :: ModelSaver -> IO ()
exitGracefully ModelSaver {..} = do
  atomically $ writeTVar msShallQuitV True
  ok <- isJust <$> timeout quitTimeoutMicros (wait msAsync)
  unless ok $ do
    glogL ERROR "ModelSaver timed out on quit."
    cancel msAsync

writeModelToFile :: Model -> IO ()
writeModelToFile model =
  -- Mask so that the write cannot be killed (e.g. by the main thread exiting), which would corrupt
  -- data.
  mask_ $
    B.writeFile model_filename (encodePretty' prettyConfig $ modelToDiskModel model)
 where
  prettyConfig = defConfig {confIndent = Spaces 2, confCompare = keyComp}
  keyComp = keyOrder ["id", "name"] `mappend` compare
