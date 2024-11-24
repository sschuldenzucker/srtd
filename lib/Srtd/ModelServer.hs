{-# LANGUAGE ImplicitParams #-}

{-| Synchronization framework for concurrent model access.

There's not actually a server here but we *may* want to make it one later.
-}
module Srtd.ModelServer (ModelServer, MsgModelUpdated (..), getModel, modifyModelOnServer, startModelServer, subscribe) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_)
import Data.Aeson
import Data.Maybe
import Data.Time (getCurrentTimeZone)
import Srtd.Config
import Srtd.Log
import Srtd.Model
import System.IO.Error

data MsgModelUpdated = MsgModelUpdated deriving (Show)

-- | Stores the model and a list of subscribers (which are represented as arbitrary IO actions)
--
-- Note: You should be careful to make sure the notifiers don't lead to deadlock. Usually, these will be of type `putChan xxx`.
--
-- SOMEDAY some safety when we can't run one of the subscribers b/c someone else crashed? Should we just force them to be channels?
data ModelServer = ModelServer (TVar Model) (TVar [MsgModelUpdated -> IO ()])

instance Show ModelServer where
  show _ = "<ModelServer>"

getModel :: ModelServer -> IO Model
getModel (ModelServer mv _) = readTVarIO mv

-- SOMEDAY this prob shouldn't exist?
-- The main issue seems to be that the caller currently assumes that the model is updated immediately after calling this.
-- This needs to change if becomes an actual server (or this has to be a "call"-type transaction.).
-- NB There are quite a few function rn that assume that this is synchronous, including notifications.
modifyModelOnServer :: ModelServer -> ((?mue :: ModelUpdateEnv) => Model -> Model) -> IO ()
modifyModelOnServer server@(ModelServer mv _) f = do
  -- SOMEDAY it mayy be a problem that we pull the timezone each time we update the model but it's probably fine.
  tz <- getCurrentTimeZone
  let f1 = let ?mue = ModelUpdateEnv {mueTimeZone = tz} in f
  atomically $ modifyTVar' mv f1
  notifyAll msg server
 where
  -- Dummy right now but might do something later when we accept more structured updates.
  msg = MsgModelUpdated

startModelServer :: IO ModelServer
startModelServer = do
  tz <- getCurrentTimeZone
  let ?mue = ModelUpdateEnv {mueTimeZone = tz}
  mmodel <- readModelFromFile
  let model = fromMaybe (diskModelToModel $ emptyDiskModel) mmodel
  mv <- newTVarIO model
  subs <- newTVarIO []
  return (ModelServer mv subs)

readModelFromFile :: (?mue :: ModelUpdateEnv) => IO (Maybe Model)
readModelFromFile = run `catch` handleFileNotFound
 where
  handleFileNotFound e
    | isDoesNotExistError e = return Nothing
    | otherwise = throwIO e
  run = do
    edmodel <- eitherDecodeFileStrict model_filename :: IO (Either String DiskModel)
    case edmodel of
      Left e -> do
        glogL ERROR ("Failed to decode file " ++ model_filename ++ ": " ++ e)
        throwIO $ AesonException e
      Right m -> return . Just $ diskModelToModel m

subscribe :: ModelServer -> (MsgModelUpdated -> IO ()) -> IO ()
subscribe (ModelServer _ msubs) sub = atomically $ modifyTVar' msubs (sub :)

notifyAll :: MsgModelUpdated -> ModelServer -> IO ()
notifyAll msg (ModelServer _ msubs) = do
  subs <- readTVarIO msubs
  forM_ subs $ \f -> f msg
