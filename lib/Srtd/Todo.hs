module Srtd.Todo where

import GHC.Stack (HasCallStack)

todo :: (HasCallStack) => a
todo = error "todo"
