module Test.Callbacks.Basic (
    tests
    -- * Properties (exported for haddocks)
  , prop_square_equiv_toFromFunPtr_square
  , prop_square_equiv_fromFunPtr_square
  , prop_apply_int_op_square
  , prop_apply_int_op_hsFun
  ) where

import Foreign.C.Types (CInt)
import Foreign.Ptr (FunPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Fun, Property, applyFun, ioProperty, testProperty,
                              (===))

import HsBindgen.Runtime.Prelude (FromFunPtr (fromFunPtr), ToFunPtr (toFunPtr),
                                  safeCastFunPtr)

import Generated.Callbacks.Basic qualified as Types
import Generated.Callbacks.Basic.FunPtr qualified as FunPtr
import Generated.Callbacks.Basic.Safe qualified as Safe
import Test.Util.Orphans ()

tests :: TestTree
tests = testGroup "Test.Callbacks.Basic" [
      testProperty "prop_square_equiv_toFromFunPtr_square" prop_square_equiv_toFromFunPtr_square
    , testProperty "prop_square_equiv_fromFunPtr_square" prop_square_equiv_fromFunPtr_square
    , testProperty "prop_apply_int_op_square" prop_apply_int_op_square
    , testProperty "prop_apply_int_op_hsFun" prop_apply_int_op_hsFun
    ]

-- |
-- \[
--  \forall x.
--      \text{Safe.square} ~ x
--    = (\text{fromFunPtr}
--        ~ (\text{toFunPtr} ~ \text{Safe.square})
--      ) ~ x
-- \]
--
prop_square_equiv_toFromFunPtr_square :: CInt -> Property
prop_square_equiv_toFromFunPtr_square x = ioProperty $ do
    z1 <- Safe.square x
    square' <- fromFunPtr <$> toFunPtr Safe.square
    z2 <- square' x
    pure $ z1 === z2

-- |
-- \[
--  \forall x.
--      \text{Safe.square} ~ x
--    = (\text{fromFunPtr} ~ \text{FunPtr.square}
--      ) ~ x
-- \]
--
prop_square_equiv_fromFunPtr_square :: CInt -> Property
prop_square_equiv_fromFunPtr_square x = ioProperty $ do
    z1 <- Safe.square x
    z2 <- fromFunPtr FunPtr.square x
    pure $ z1 === z2

-- |
-- \[
--  \forall x.
--      x * x
--    = \text{Safe.apply_int_op}
--        ~ \text{FunPtr.square}
--        ~ x
-- \]
--
prop_apply_int_op_square :: CInt -> Property
prop_apply_int_op_square x = ioProperty $ do
    z <- Safe.apply_int_op squarePtr x
    pure $ x * x === z
  where
    squarePtr :: FunPtr Types.Int_op
    squarePtr = safeCastFunPtr FunPtr.square

-- |
-- \[
--  \forall f, x.
--      f ~ x
--    = \text{Safe.apply_int_op}
--        ~ (\text{toFunPtr} ~ f)
--        ~ x
-- \]
--
prop_apply_int_op_hsFun :: Fun CInt CInt -> CInt -> Property
prop_apply_int_op_hsFun f x = ioProperty $ do
    fPtr <- toFunPtr fOp
    z <- Safe.apply_int_op fPtr x
    pure $ applyFun f x === z
  where
    fOp :: Types.Int_op
    fOp = Types.Int_op $ \arg -> pure $ applyFun f arg
