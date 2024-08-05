-- |
-- Some convenience helpers for logging.
--
-- May see some changes going forward (e.g., for concurrency)
module Log (setupLogger, glogL, uglogL, Priority (..)) where

import GHC.IO.Unsafe (unsafePerformIO)
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger hiding (getLogger)

-- | Setup global "Srtd" logger.
setupLogger :: IO ()
setupLogger = do
  -- logger <- L.getLogger "Srtd"
  -- h <-
  --   fileHandler "srtd.log" INFO >>= \lh ->
  --     return $
  --       setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  -- let logger' = setLevel INFO . addHandler h . removeHandler $ logger
  -- saveGlobalLogger logger'
  fileH <- fileHandler "srtd.log" DEBUG
  let formatter = simpleLogFormatter "[$time $loggername $prio] $msg"
  let fileH' = setFormatter fileH formatter
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName (setLevel INFO . addHandler fileH')

-- | Log a message using the global logger.
glogL :: Priority -> String -> IO ()
glogL prio msg = do
  l <- getRootLogger
  logL l prio msg

-- | Unsafe variant of `glogL`. Often has unexpected effects b/c of lazy evaluation. You've been warned.
uglogL :: Priority -> String -> a -> a
uglogL prio msg x = unsafePerformIO (glogL prio msg >> return x)
