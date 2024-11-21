module HsBindgen.TestLib.SameSemantics (
    -- * SameSemantics
    SameSemantics(..)
  , sameSemanticsOn
    -- * Tasty
  , assertSameSemantics
  , (@=~?)
  , assertNotSameSemantics
  , (@/=~?)
    -- * Properties
  , prop_SameSemanticsRefl
  , assertSameSemanticsRefl
  ) where

import Control.Monad (when, unless)
import Foreign.C qualified as FC
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit (Assertion, assertFailure)

{-------------------------------------------------------------------------------
  SameSemantics
-------------------------------------------------------------------------------}

-- | Same semantics
--
-- This class defines /semantic/ equality.  This is equality for most types, so
-- the default implementation of 'sameSemantics' is '(==)'.  Exceptions include:
--
-- * The 'FC.CBool' instance uses the semantics of C conditionals: @0@
--   represents 'False', while any number greather than @0@ represents 'True'.
-- * In instances for floating point types, @NaN@ is semantically equal to
--   itself, even though @NaN /= NaN@.
--
-- prop> x `sameSemantics` x
class SameSemantics a where
  -- | Determine if two values have the same semantics
  sameSemantics :: a -> a -> Bool

  default sameSemantics :: Eq a => a -> a -> Bool
  sameSemantics = (==)

instance SameSemantics FC.CChar

instance SameSemantics FC.CSChar

instance SameSemantics FC.CUChar

instance SameSemantics FC.CShort

instance SameSemantics FC.CUShort

instance SameSemantics FC.CInt

instance SameSemantics FC.CUInt

instance SameSemantics FC.CLong

instance SameSemantics FC.CULong

instance SameSemantics FC.CPtrdiff

instance SameSemantics FC.CSize

instance SameSemantics FC.CWchar

instance SameSemantics FC.CSigAtomic

instance SameSemantics FC.CLLong

instance SameSemantics FC.CULLong

instance SameSemantics FC.CBool where
  sameSemantics l r = (l == 0) == (r == 0)

instance SameSemantics FC.CIntPtr

instance SameSemantics FC.CUIntPtr

instance SameSemantics FC.CIntMax

instance SameSemantics FC.CUIntMax

instance SameSemantics FC.CClock

instance SameSemantics FC.CTime

{- TODO remove or fix
instance SameSemantics FC.CUSeconds
-}

instance SameSemantics FC.CSUSeconds

instance SameSemantics FC.CFloat where
  sameSemantics l r = l == r || (isNaN l && isNaN r)

instance SameSemantics FC.CDouble where
  sameSemantics l r = l == r || (isNaN l && isNaN r)

-- | Determine same semantics using a projection
sameSemanticsOn :: SameSemantics b => (a -> b) -> a -> a -> Bool
sameSemanticsOn f l r = f l `sameSemantics` f r

{-------------------------------------------------------------------------------
  Tasty
-------------------------------------------------------------------------------}

-- | Assert that the specified actual value has the same semantics as the
-- expected value
assertSameSemantics ::
     (SameSemantics a, Show a, HasCallStack)
  => a            -- ^ Expected value
  -> a            -- ^ Actual value
  -> Assertion
assertSameSemantics expected actual =
    unless (actual `sameSemantics` expected) $ assertFailure msg
  where
    msg :: String
    msg =
      show expected ++ " does not have the same semantics as " ++ show actual

-- | Assert that the specified actual value has the same semantics as the
-- expected value
infix 1 @=~?
(@=~?) ::
     (SameSemantics a, Show a, HasCallStack)
  => a -- ^ Expected value
  -> a -- ^ Actual value
  -> Assertion
(@=~?) = assertSameSemantics

-- | Assert that the specified actual value does /not/ have the same semantics
-- as the expected value
assertNotSameSemantics ::
     (SameSemantics a, Show a, HasCallStack)
  => a -- ^ Expected value
  -> a -- ^ Actual value
  -> Assertion
assertNotSameSemantics expected actual =
    when (actual `sameSemantics` expected) $ assertFailure msg
  where
    msg :: String
    msg = show expected ++ " has the same semantics as " ++ show actual

-- | Assert that the specified actual value does /not/ have the same semantics
-- as the expected value
infix 1 @/=~?
(@/=~?) ::
     (SameSemantics a, Show a, HasCallStack)
  => a -- ^ Expected value
  -> a -- ^ Actual value
  -> Assertion
(@/=~?) = assertNotSameSemantics

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Same semantics is reflexive
prop_SameSemanticsRefl :: SameSemantics a => a -> Bool
prop_SameSemanticsRefl x = x `sameSemantics` x

-- | Same semantics is reflexive
assertSameSemanticsRefl :: (SameSemantics a, Show a) => a -> Assertion
assertSameSemanticsRefl x = x @=~? x
