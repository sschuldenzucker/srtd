-- | Little abstraction helper
module Srtd.MonadBrick where

import Brick (EventM)
import Control.Monad.Except (ExceptT)
import Control.Monad.Writer.Strict
import Srtd.Component (AppResourceName)

-- Have to be specific in AppResourceName b/c otherwise I get some type ambiguity I don't understand.
class MonadBrick m s where
  liftEventM :: EventM AppResourceName s a -> m a

instance MonadBrick (EventM AppResourceName s) s where
  liftEventM = id

instance (Monoid w, Monad m, MonadBrick m s) => MonadBrick (WriterT w m) s where
  liftEventM = lift . liftEventM

instance (Monad m, MonadBrick m s) => MonadBrick (ExceptT e m) s where
  liftEventM = lift . liftEventM
