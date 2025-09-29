module Test.Internal.Tasty (
    -- * Assertions
    (@=?!)
  ) where

import Control.Exception (Exception, evaluate, try)
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit (Assertion, (@=?))

{-------------------------------------------------------------------------------
  Assertions
-------------------------------------------------------------------------------}

(@=?!) ::
     (Eq a, Eq e, Exception e, HasCallStack, Show a)
  => e
  -> a
  -> Assertion
expected @=?! actual = (Left expected @=?) =<< try (evaluate actual)
