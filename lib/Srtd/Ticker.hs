-- | A process that executes a given IO action every full minute
module Srtd.Ticker where

-- SOMEDAY for a basic bad feeling about how exactly concurrency works here, see ModelSaver.

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever)

-- | Start the ticker thread. `startTicker n act` executes the action `act` every `n` seconds.
--
-- Note that execution is performed inside the ticker thread and no effort is made to synchronize
-- the delay with anything, i.e., this is *not* high-precision.
--
-- SOMEDAY run it *at the beginning of every minute*, not every 60 seconds. (that's the actually-correct
-- semantics in our context)
--
-- You may want to `link` the ticker to the parent.
startTicker :: Int -> IO () -> IO (Async ())
startTicker delaySeconds action = do
  let delayMicroseconds = delaySeconds * 1_000_000
  let server = forever $ do
        threadDelay delayMicroseconds
        action
  async server
