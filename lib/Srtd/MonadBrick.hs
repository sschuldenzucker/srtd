-- | Little abstraction helper
module Srtd.MonadBrick where

import Brick (EventM)
import Brick.Types (nestEventM)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (MonadState)
import Control.Monad.Writer.Strict
import Srtd.Component (AppComponent (Event), AppEventM, AppResourceName)

-- Have to be specific in AppResourceName b/c otherwise I get some type ambiguity I don't understand.
class (MonadState s m) => MonadBrick m s where
  liftEventM :: EventM AppResourceName s a -> m a

instance MonadBrick (EventM AppResourceName s) s where
  liftEventM = id

instance (Monoid w, Monad m, MonadBrick m s) => MonadBrick (WriterT w m) s where
  liftEventM = lift . liftEventM

instance (Monad m, MonadBrick m s) => MonadBrick (ExceptT e m) s where
  liftEventM = lift . liftEventM

-- | Run an AppEventM with explicitly provided state, returning explicit events. Like `nestEventM`
-- but for AppEventM.
--
-- Unrelated helper,,, Shouldn't really go here.
nestAppEventM :: (AppComponent s) => s -> AppEventM s a -> AppEventM t (s, (a, [Event s]))
nestAppEventM s act = liftEventM $ nestEventM s (runWriterT act)
