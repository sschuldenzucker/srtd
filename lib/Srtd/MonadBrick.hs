{-# LANGUAGE FunctionalDependencies #-}
-- required b/c the decidability rule ignores fundeps (?!?). It's fine!!
{-# LANGUAGE UndecidableInstances #-}

-- | Little abstraction helper: Things that can be lifted into Brick's 'EventM'
module Srtd.MonadBrick where

import Brick (EventM)
import Brick.Types (nestEventM)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Writer.Strict

class (MonadState s m) => MonadBrick n s m | m -> n s where
  liftEventM :: EventM n s a -> m a

instance MonadBrick n s (EventM n s) where
  liftEventM = id

-- SOMEDAY we can also do this with a single instance via MonadTrans and OverlappingInstances but careful that our recursion actually terminates, lol.

instance (Monoid w, Monad m, MonadBrick n s m) => MonadBrick n s (WriterT w m) where
  liftEventM = lift . liftEventM

instance (Monad m, MonadBrick n s m) => MonadBrick n s (ReaderT r m) where
  liftEventM = lift . liftEventM

instance (Monad m, MonadBrick n s m) => MonadBrick n s (ExceptT e m) where
  liftEventM = lift . liftEventM
