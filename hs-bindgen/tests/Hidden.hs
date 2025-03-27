module Hidden where

import Control.Exception (Exception(displayException), throw)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

newtype HiddenException = HiddenException CallStack
  deriving Show

instance Exception HiddenException where
  displayException (HiddenException cs) = unlines
    [ "A test accessed data that was explicitly hidden."
    , prettyCallStack cs
    ]

hidden :: HasCallStack => a
hidden = throw $ HiddenException callStack
