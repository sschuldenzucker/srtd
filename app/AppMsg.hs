-- | Simple module containing type definitions for the app. Separate to avoid circulat dependencies.
module AppMsg where

import ModelServer (ModelServer)

data AppResourceName = MainList deriving (Eq, Ord, Show)

-- No messages here yet
type AppMsg = ()

data AppContext = AppContext
  { acModelServer :: ModelServer
  }
