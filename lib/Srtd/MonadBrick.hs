{-# LANGUAGE FunctionalDependencies #-}
-- required b/c the decidability rule ignores fundeps (?!?). It's fine!!
{-# LANGUAGE UndecidableInstances #-}

-- | Little abstraction helper: Things that can be lifted into Brick's 'EventM'
module Srtd.MonadBrick where

import Brick (EventM)
import Brick.Types (nestEventM)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (MonadState)
import Control.Monad.Writer.Strict

class (MonadState s m) => MonadBrick n s m | m -> n s where
  liftEventM :: EventM n s a -> m a

instance MonadBrick n s (EventM n s) where
  liftEventM = id

instance (Monoid w, Monad m, MonadBrick n s m) => MonadBrick n s (WriterT w m) where
  liftEventM = lift . liftEventM

instance (Monad m, MonadBrick n s m) => MonadBrick n s (ExceptT e m) where
  liftEventM = lift . liftEventM
