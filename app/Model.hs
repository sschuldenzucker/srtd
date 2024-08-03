module Model (Model (..), ModelServer, getModel, startModelServer, modifyModelOnServer) where

import Control.Concurrent.STM

-- TODO dummy
data Model = Model
  { val :: Int
  }

emptyModel :: Model
emptyModel = Model 0

-- SOMEDAY put this into a separate module

-- There's not actually a server here but we *may* want to make it one later.
newtype ModelServer = ModelServer (TVar Model)

getModel :: ModelServer -> IO Model
getModel (ModelServer mv) = readTVarIO mv

-- TODO this prob shouldn't exist
modifyModelOnServer :: ModelServer -> (Model -> Model) -> IO ()
modifyModelOnServer (ModelServer mv) f = atomically $ modifyTVar' mv f

-- TODO dummy
startModelServer :: IO ModelServer
startModelServer = do
  mv <- newTVarIO emptyModel
  return (ModelServer mv)
