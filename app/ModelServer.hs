-- | Synchronization framework for concurrent model access.
--
-- There's not actually a server here but we *may* want to make it one later.
module ModelServer (ModelServer, MsgModelUpdated (..), getModel, modifyModelOnServer, startModelServer, subscribe) where

import Config
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_)
import Data.Aeson
import Data.Maybe
import Log
import Model
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
modifyModelOnServer :: ModelServer -> (Model -> Model) -> IO ()
modifyModelOnServer server@(ModelServer mv _) f = do
  atomically $ modifyTVar' mv f
  notifyAll msg server
  where
    -- Dummy right now but might do something later when we accept more structured updates.
    msg = MsgModelUpdated

startModelServer :: IO ModelServer
startModelServer = do
  mmodel <- readModelFromFile
  let model = fromMaybe emptyModel mmodel
  mv <- newTVarIO model
  subs <- newTVarIO []
  return (ModelServer mv subs)

readModelFromFile :: IO (Maybe Model)
readModelFromFile = run `catch` handleFileNotFound
  where
    handleFileNotFound e
      | isDoesNotExistError e = return Nothing
      | otherwise = throwIO e
    run = do
      emodel <- eitherDecodeFileStrict model_filename :: IO (Either String Model)
      case emodel of
        Left e -> do
          glogL ERROR ("Failed to decode file " ++ model_filename ++ ": " ++ e)
          throwIO $ AesonException e
        Right m -> return $ Just m

subscribe :: ModelServer -> (MsgModelUpdated -> IO ()) -> IO ()
subscribe (ModelServer _ msubs) sub = atomically $ modifyTVar' msubs (sub :)

notifyAll :: MsgModelUpdated -> ModelServer -> IO ()
notifyAll msg (ModelServer _ msubs) = do
  subs <- readTVarIO msubs
  forM_ subs $ \f -> f msg
