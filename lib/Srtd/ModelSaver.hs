
-- | A process that auto-saves the model.
module Srtd.ModelSaver where

import Control.Concurrent
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy qualified as B
import Srtd.Config (model_filename)
import Srtd.Model (Model, modelToDiskModel)
import Srtd.ModelServer qualified as MS

-- SOMEDAY some handling of crashes (at least re-raise exceptions, or honestly restart like Erlang)
startModelSaver :: MS.ModelServer -> IO ThreadId
startModelSaver mserver = do
  parentThread <- myThreadId
  chan <- newChan
  MS.subscribe mserver (writeChan chan)
  let server = forever $ do
        -- SOMEDAY do I wanna throttle the rate at which updates are processed?
        -- (in case there are many for some reason)
        _ <- readChan chan
        model <- MS.getModel mserver
        writeModelToFile model
  let handler :: SomeException -> IO ()
      handler = throwTo parentThread
  -- I'm not 100% sure this is the right way. Feels unideomatic.
  forkIO (server `catch` handler)

writeModelToFile :: Model -> IO ()
writeModelToFile model = B.writeFile model_filename (encodePretty' prettyConfig $ modelToDiskModel model)
 where
  prettyConfig = defConfig {confIndent = Spaces 2, confCompare = keyComp}
  keyComp = keyOrder ["id", "name"] `mappend` compare
