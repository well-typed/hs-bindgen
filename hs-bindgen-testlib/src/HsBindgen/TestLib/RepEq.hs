-- TODO documentation
module HsBindgen.TestLib.RepEq (
    -- * RepEq
    RepEq(..)
  , repEqOn
    -- * Tasty
  , assertRepEq
  , (@!=?)
  , assertNotRepEq
  , (@!/=?)
    -- * Properties
  , prop_Reflexive
  , assertReflexive
  ) where

import Control.Monad (when, unless)
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

instance RepEq FC.CSChar

instance RepEq FC.CUChar

instance RepEq FC.CShort

instance RepEq FC.CUShort

instance RepEq FC.CInt

instance RepEq FC.CUInt

instance RepEq FC.CLong

instance RepEq FC.CULong

instance RepEq FC.CPtrdiff

instance RepEq FC.CSize

instance RepEq FC.CWchar

instance RepEq FC.CSigAtomic

instance RepEq FC.CLLong

instance RepEq FC.CULLong

instance RepEq FC.CBool

instance RepEq FC.CIntPtr

instance RepEq FC.CUIntPtr

instance RepEq FC.CIntMax

instance RepEq FC.CUIntMax

instance RepEq FC.CClock

instance RepEq FC.CTime

{- TODO remove or fix
instance RepEq FC.CUSeconds
-}

instance RepEq FC.CSUSeconds

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

assertNotRepEq ::
     (RepEq a, Show a, HasCallStack)
  => a -- ^ Expected value
  -> a -- ^ Actual value
  -> Assertion
assertNotRepEq expected actual =
    when (actual `repEq` expected) $ assertFailure msg
  where
    msg :: String
    msg = show expected ++ " representationally equal to " ++ show actual

-- | Assert that the specified actual value is /not/ representationally equal to
-- the expected value
infix 1 @!/=?
(@!/=?) ::
     (RepEq a, Show a, HasCallStack)
  => a -- ^ Expected value
  -> a -- ^ Actual value
  -> Assertion
actual @!/=? expected = assertNotRepEq expected actual

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Representational equality is reflexive
prop_Reflexive :: RepEq a => a -> Bool
prop_Reflexive x = x `repEq` x

-- | Representational equality is reflexive
assertReflexive :: (RepEq a, Show a) => a -> Assertion
assertReflexive x = x @!=? x
