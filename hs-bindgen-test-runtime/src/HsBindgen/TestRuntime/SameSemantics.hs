module HsBindgen.TestRuntime.SameSemantics (
    -- * SameSemantics
    SameSemantics(..)
  , sameSemanticsOn
    -- * QuickCheck
  , (==~)
  , (/=~)
    -- * HUnit
  , assertSameSemantics
  , (@==~?)
  , assertNotSameSemantics
  , (@/=~?)
    -- * Properties
  , nameXSameSemanticsX
  , prop_XSameSemanticsX
  , assertXSameSemanticsX
  ) where

import Control.Monad (unless, when)
import Foreign.C qualified as FC
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit (Assertion, assertFailure)
import Test.Tasty.QuickCheck (Property, counterexample)

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
class SameSemantics a where
  -- | Determine if two values have the same semantics
  --
  -- prop> x `sameSemantics` x
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

instance SameSemantics FC.CFloat where
  sameSemantics l r = l == r || (isNaN l && isNaN r)

instance SameSemantics FC.CDouble where
  sameSemantics l r = l == r || (isNaN l && isNaN r)

-- | Determine same semantics using a projection
sameSemanticsOn :: SameSemantics b => (a -> b) -> a -> a -> Bool
sameSemanticsOn f l r = f l `sameSemantics` f r

{-------------------------------------------------------------------------------
  QuickCheck
-------------------------------------------------------------------------------}

-- | Assert that two values have the same semantics
infix 4 ==~
(==~) :: (SameSemantics a, Show a) => a -> a -> Property
l ==~ r = counterexample (show l ++ interpret res ++ show r) res
  where
    res :: Bool
    res = l `sameSemantics` r

    interpret :: Bool -> String
    interpret True  = " ==~ "
    interpret False = " /=~ "

-- | Assert that two values do /not/ have the same semantics
infix 4 /=~
(/=~) :: (SameSemantics a, Show a) => a -> a -> Property
l /=~ r = counterexample (show l ++ interpret res ++ show r) res
  where
    res :: Bool
    res = not $ l `sameSemantics` r

    interpret :: Bool -> String
    interpret True  = " /=~ "
    interpret False = " ==~ "

{-------------------------------------------------------------------------------
  HUnit
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
infix 1 @==~?
(@==~?) ::
     (SameSemantics a, Show a, HasCallStack)
  => a -- ^ Expected value
  -> a -- ^ Actual value
  -> Assertion
(@==~?) = assertSameSemantics

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
nameXSameSemanticsX :: String
nameXSameSemanticsX = "XSameSemanticsX"

-- | Same semantics is reflexive
prop_XSameSemanticsX :: (SameSemantics a, Show a) => a -> Property
prop_XSameSemanticsX x = x ==~ x

-- | Same semantics is reflexive
assertXSameSemanticsX :: (SameSemantics a, Show a) => a -> Assertion
assertXSameSemanticsX x = x @==~? x
