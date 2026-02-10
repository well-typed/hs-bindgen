{-# LANGUAGE DeriveAnyClass #-}

module Test.Callbacks.Operators (
    tests
    -- * Exported for haddocks
    -- ** Unary operators
  , prop_square_equiv_toFromFunPtr_square
  , prop_square_equiv_fromFunPtr_squarePtr
  , prop_apply1_squarePtr
  , prop_apply1_hsFun
    -- ** Binary operators
  , prop_plus_equiv_toFromFunPtr_plus
  , prop_plus_equiv_fromFunPtr_plusPtr
  , prop_apply2_plusPtr
  , prop_apply2_hsFun
  ) where

import Foreign.C.Types (CInt)
import Foreign.Ptr (FunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Fun, Property, applyFun, ioProperty, testProperty,
                              (===))

import HsBindgen.Runtime.Prelude (FromFunPtr (fromFunPtr), ToFunPtr (toFunPtr))

import Generated.Callbacks.Operators qualified as Types
import Generated.Callbacks.Operators.FunPtr qualified as FunPtr
import Generated.Callbacks.Operators.Safe qualified as Safe
import Test.Util.FunPtr (castFunPtrCoercible)
import Test.Util.Orphans ()

tests :: TestTree
tests = testGroup "Test.Callbacks.Operators" [
      -- * Unary operators
      testProperty "prop_square_equiv_toFromFunPtr_square" prop_square_equiv_toFromFunPtr_square
    , testProperty "prop_square_equiv_fromFunPtr_squarePtr" prop_square_equiv_fromFunPtr_squarePtr
    , testProperty "prop_apply1_squarePtr" prop_apply1_squarePtr
    , testProperty "prop_apply1_hsFun" prop_apply1_hsFun
      -- * Binary operators
    , testProperty "prop_plus_equiv_toFromFunPtr_plus" prop_plus_equiv_toFromFunPtr_plus
    , testProperty "prop_plus_equiv_fromFunPtr_plusPtr" prop_plus_equiv_fromFunPtr_plusPtr
    , testProperty "prop_apply2_plusPtr" prop_apply2_plusPtr
    , testProperty "prop_apply2_hsFun" prop_apply2_hsFun
    ]

{-------------------------------------------------------------------------------
  Unary operators
-------------------------------------------------------------------------------}

-- | \( \forall x. \text{Safe.square} ~ x = (\text{fromFunPtr} ~ (\text{toFunPtr} ~ \text{Safe.square})) ~ x \)
prop_square_equiv_toFromFunPtr_square :: CInt -> Property
prop_square_equiv_toFromFunPtr_square x = ioProperty $ do
    z1 <- Safe.square x
    square' <- fromFunPtr <$> toFunPtr Safe.square
    z2 <- square' x
    pure $ z1 === z2

-- | \( \forall x. \text{Safe.square} ~ x = (\text{fromFunPtr} ~ \text{FunPtr.square}) ~ x \)
prop_square_equiv_fromFunPtr_squarePtr :: CInt -> Property
prop_square_equiv_fromFunPtr_squarePtr x = ioProperty $ do
    z1 <- Safe.square x
    z2 <- fromFunPtr FunPtr.square x
    pure $ z1 === z2

-- | \( \forall x. x * x = \text{Safe.apply1} ~ \text{FunPtr.square} ~ x \)
prop_apply1_squarePtr :: CInt -> Property
prop_apply1_squarePtr x = ioProperty $ do
    z <- Safe.apply1 squarePtr x
    pure $ x * x === z
  where
    squarePtr :: FunPtr Types.Unary_op
    squarePtr = castFunPtrCoercible FunPtr.square

-- | \( \forall f, x. f ~ x = \text{Safe.apply1} ~ (\text{toFunPtr} ~ f) ~ x \)
prop_apply1_hsFun :: Fun CInt CInt -> CInt -> Property
prop_apply1_hsFun f x = ioProperty $ do
    fPtr <- toFunPtr fOp
    z <- Safe.apply1 fPtr x
    pure $ applyFun f x === z
  where
    fOp :: Types.Unary_op
    fOp = Types.Unary_op $ \arg -> pure $ applyFun f arg

{-------------------------------------------------------------------------------
  Binary operators
-------------------------------------------------------------------------------}

-- | \( \forall x, y. \text{Safe.plus} ~ x ~ y = (\text{fromFunPtr} ~ (\text{toFunPtr} ~ \text{Safe.plus})) ~ x ~ y \)
prop_plus_equiv_toFromFunPtr_plus :: CInt -> CInt -> Property
prop_plus_equiv_toFromFunPtr_plus x y = ioProperty $ do
    z1 <- Safe.plus x y
    plus' <- fromFunPtr <$> toFunPtr Safe.plus
    z2 <- plus' x y
    pure $ z1 === z2

-- | \( \forall x, y. \text{Safe.plus} ~ x ~ y = (\text{fromFunPtr} ~ \text{FunPtr.plus}) ~ x ~ y\)
prop_plus_equiv_fromFunPtr_plusPtr :: CInt -> CInt -> Property
prop_plus_equiv_fromFunPtr_plusPtr x y = ioProperty $ do
    z1 <- Safe.plus x y
    z2 <- fromFunPtr FunPtr.plus x y
    pure $ z1 === z2

-- | \( \forall x, y. x + y = \text{Safe.apply2} ~ \text{FunPtr.plus} ~ x ~ y \)
prop_apply2_plusPtr :: CInt -> CInt -> Property
prop_apply2_plusPtr x y = ioProperty $ do
    z <- Safe.apply2 plusPtr x y
    pure $ x + y === z
  where
    plusPtr :: FunPtr Types.Bin_op
    plusPtr = castFunPtrCoercible FunPtr.plus

-- | \( \forall f, x, y. f ~ x ~ y = \text{Safe.apply2} ~ (\text{toFunPtr} ~ f) ~ x ~ y \)
prop_apply2_hsFun :: Fun (CInt, CInt) CInt -> CInt -> CInt -> Property
prop_apply2_hsFun f x y = ioProperty $ do
    fPtr <- toFunPtr fOp
    z <- Safe.apply2 fPtr x y
    pure $ applyFun f (x, y) === z
  where
    fOp :: Types.Bin_op
    fOp = Types.Bin_op $ \arg1 arg2 -> pure $ applyFun f (arg1, arg2)
