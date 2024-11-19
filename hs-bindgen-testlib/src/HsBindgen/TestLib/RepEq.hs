-- TODO documentation
module HsBindgen.TestLib.RepEq (
    -- * RepEq
    RepEq(..)
  , repEqOn
    -- * Tasty
  , assertRepEq
  , (@!=?)
  ) where

import Control.Monad (unless)
import Foreign.C qualified as FC
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit (Assertion, assertFailure)

{-------------------------------------------------------------------------------
  RepEq
-------------------------------------------------------------------------------}

-- TODO documentation
class RepEq a where
  repEq :: a -> a -> Bool

  default repEq :: Eq a => a -> a -> Bool
  repEq = (==)

instance RepEq FC.CChar

instance RepEq FC.CInt

instance RepEq FC.CFloat where
  repEq l r = l == r || (isNaN l && isNaN r)

instance RepEq FC.CDouble where
  repEq l r = l == r || (isNaN l && isNaN r)

-- TODO documentation
repEqOn :: RepEq b => (a -> b) -> a -> a -> Bool
repEqOn f l r = f l `repEq` f r

{-------------------------------------------------------------------------------
  Tasty
-------------------------------------------------------------------------------}

-- | Assert that the specified actual value is representationally equal to the
-- expected value
assertRepEq ::
     (RepEq a, Show a, HasCallStack)
  => Maybe String -- ^ Error message preface
  -> a            -- ^ Expected value
  -> a            -- ^ Actual value
  -> Assertion
assertRepEq mPreface expected actual =
    unless (actual `repEq` expected) $ assertFailure msg
  where
    msg :: String
    msg =
      maybe "" (++ "\n") mPreface
        ++ "expected: " ++ show expected
        ++ "\n but got: " ++ show actual

-- | Assert that the specified actual value is representationally equal to the
-- expected value
infix 1 @!=?
(@!=?) ::
     (RepEq a, Show a, HasCallStack)
  => a -- ^ Expected value
  -> a -- ^ Actual value
  -> Assertion
actual @!=? expected = assertRepEq Nothing expected actual
