{-|
Some convenience helpers for logging.

May see some changes going forward (e.g., for concurrency)
-}
module Log where

import qualified System.Log.Logger as L
import System.Log.Logger hiding (getLogger)
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import GHC.IO.Unsafe (unsafePerformIO)

-- | Setup global "Srtd" logger.
setupLogger :: IO ()
setupLogger = do
  logger <- L.getLogger "Srtd"
  h <- fileHandler "srtd.log" INFO >>= \lh -> return $
           setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  let logger' = setLevel INFO $ addHandler h logger
  saveGlobalLogger logger'

ggetLogger :: IO Logger
ggetLogger = L.getLogger "Srtd"

-- | Log a message using the global logger.
glogL :: Priority -> String -> IO ()
glogL prio msg = do
  l <- ggetLogger
  logL l prio msg

-- | Unsafe variant of `glogL`. Often has unexpected effects b/c of lazy evaluation. You've been warned.
uglogL :: Priority -> String -> a -> a
uglogL prio msg x = unsafePerformIO (glogL prio msg >> return x)
