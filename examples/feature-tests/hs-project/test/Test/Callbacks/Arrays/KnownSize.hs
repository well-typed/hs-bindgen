{-# LANGUAGE DataKinds #-}

module Test.Callbacks.Arrays.KnownSize (
    tests
    -- * Properties (exported for haddocks)
  , prop_reverse_vec5_equiv_toFromFunPtr_reverse_vec5
  , prop_reverse_vec5_equiv_fromFunPtr_reverse_vec5
  , prop_apply_vec5_op_reverse_vec5
  , prop_apply_vec5_op_hsFun
  ) where

import Control.Monad (forM_)
import Data.Proxy (Proxy (Proxy))
import Foreign.C.Types (CInt (CInt))
import Foreign.Ptr (FunPtr)
import Foreign.Storable (Storable (peek, peekElemOff, pokeElemOff))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Fun, Property, applyFun,
                              ioProperty, testProperty, vector, (===))

import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.IsArray qualified as IsA
import HsBindgen.Runtime.Prelude (FromFunPtr (fromFunPtr), IsArray (Elem),
                                  ToFunPtr (toFunPtr), safeCastFunPtr)

import Generated.Callbacks.Arrays.KnownSize qualified as Types
import Generated.Callbacks.Arrays.KnownSize.FunPtr qualified as FunPtr
import Generated.Callbacks.Arrays.KnownSize.Safe qualified as Safe
import Test.Util.Orphans ()

tests :: TestTree
tests = testGroup "Test.Callbacks.KnownSize" [
      testProperty "prop_reverse_vec5_equiv_toFromFunPtr_reverse_vec5"
        prop_reverse_vec5_equiv_toFromFunPtr_reverse_vec5
    , testProperty "prop_reverse_vec5_equiv_fromFunPtr_reverse_vec5"
        prop_reverse_vec5_equiv_fromFunPtr_reverse_vec5
    , testProperty "prop_apply_vec5_op_reverse_vec5"
        prop_apply_vec5_op_reverse_vec5
    , testProperty "prop_apply_vec5_op_hsFun"
        prop_apply_vec5_op_hsFun
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- |
-- \[
--  \forall xs, len.
--      \text{Safe.reverse_vec5} ~ xs ~ len
--    = (\text{fromFunPtr}
--        ~ (\text{toFunPtr} ~ \text{Safe.reverse_vec5})
--      ) ~ xs
--        ~ len
-- \]
--
prop_reverse_vec5_equiv_toFromFunPtr_reverse_vec5 :: Vec5 -> Property
prop_reverse_vec5_equiv_toFromFunPtr_reverse_vec5 (Vec5 xs) = ioProperty $ do
    zs1 <- runVec5_op reverse_vec5 xs
    reverse_vec5' <- fromFunPtr <$> toFunPtr reverse_vec5
    zs2 <- runVec5_op reverse_vec5' xs
    pure $ zs1 === zs2
  where
    reverse_vec5 :: Types.Vec5_op
    reverse_vec5 = Types.Vec5_op Safe.reverse_vec5

-- |
-- \[
--  \forall xs, len.
--      \text{Safe.reverse_vec5} ~ xs ~ len
--    = (\text{fromFunPtr} ~ \text{FunPtr.reverse_vec5})
--      ) ~ xs
--        ~ len
-- \]
--
prop_reverse_vec5_equiv_fromFunPtr_reverse_vec5 :: Vec5 -> Property
prop_reverse_vec5_equiv_fromFunPtr_reverse_vec5 (Vec5 xs) = ioProperty $ do
    zs1 <- runVec5_op reverse_vec5 xs
    zs2 <- runVec5_op reverse_vec5' xs
    pure $ zs1 === zs2
  where
    reverse_vec5 :: Types.Vec5_op
    reverse_vec5 = Types.Vec5_op Safe.reverse_vec5
    reverse_vec5Ptr :: FunPtr Types.Vec5_op
    reverse_vec5Ptr = safeCastFunPtr FunPtr.reverse_vec5
    reverse_vec5' :: Types.Vec5_op
    reverse_vec5' = fromFunPtr reverse_vec5Ptr

-- |
-- \[
--  \forall xs, len.
--      \text{reverseVec5} ~ xs ~ len
--    = \text{Safe.apply_vec5_op}
--        ~ \text{FunPtr.reverse_vec5}
--        ~ xs
--        ~ len
-- \]
--
prop_apply_vec5_op_reverse_vec5 :: Vec5 -> Property
prop_apply_vec5_op_reverse_vec5 (Vec5 xs) = ioProperty $ do
    zs <- apply_vec5_opHelper reverse_vec5Ptr xs
    pure $ reverseVec5 xs === zs
  where
    reverse_vec5Ptr :: FunPtr Types.Vec5_op
    reverse_vec5Ptr = safeCastFunPtr FunPtr.reverse_vec5

-- |
-- \[
--  \forall f, xs, len.
--      f ~ xs ~ len
--    = \text{Safe.apply_vec5_op}
--        ~ (\text{toFunPtr} ~ f)
--        ~ xs
--        ~ len
-- \]
--
prop_apply_vec5_op_hsFun :: Vec5Op -> Vec5 -> Property
prop_apply_vec5_op_hsFun f (Vec5 xs) = ioProperty $ do
    zs1 <- runVec5_op fOp xs
    fPtr <- toFunPtr fOp
    zs2 <- apply_vec5_opHelper fPtr xs
    pure $ zs1 === zs2
  where
    fOp :: Types.Vec5_op
    fOp = getVec5_op f

{-------------------------------------------------------------------------------
  Modelled callback function
-------------------------------------------------------------------------------}

reverseVec5 :: Types.Vec5 -> Types.Vec5
reverseVec5 (Types.Vec5 xs) = Types.Vec5 (CA.fromList (reverse (CA.toList xs)))

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

runVec5_op :: Types.Vec5_op -> Types.Vec5 -> IO Types.Vec5
runVec5_op (Types.Vec5_op f) xs = do
    IsA.withElemPtr xs $ \xsPtr -> do
        f xsPtr
        peek (CA.toPtr (Proxy @5) xsPtr)

apply_vec5_opHelper :: FunPtr Types.Vec5_op -> Types.Vec5 -> IO Types.Vec5
apply_vec5_opHelper fPtr xs = do
    IsA.withElemPtr xs $ \xsPtr -> do
        Safe.apply_vec5_op fPtr xsPtr
        peek (CA.toPtr (Proxy @5) xsPtr)


getVec5_op :: Vec5Op -> Types.Vec5_op
getVec5_op (Vec5Op f) = Types.Vec5_op $ \xsPtr -> do
    forM_ [0.. 4] $ \i -> do
      x <- peekElemOff xsPtr i
      let y = applyFun f x
      pokeElemOff xsPtr i y

{-------------------------------------------------------------------------------
  Arbitrary
-------------------------------------------------------------------------------}

newtype Vec5 = Vec5 Types.Vec5
  deriving stock (Show, Eq)

instance Arbitrary Vec5 where
  arbitrary = do
      xs <- vector 5
      pure (Vec5 (Types.Vec5 (CA.fromList xs)))
  shrink (Vec5 (Types.Vec5 (CA.toList -> xs))) =
      [ Vec5 (Types.Vec5 (CA.fromList xs'))
      | xs' <- shrink xs
      , length xs' == 5
      ]

-- Intended to be opaque: use 'getVec5_op' to obtain a 'Types.Vec5_op'
newtype Vec5Op = Vec5Op (Fun (Elem Types.Vec5) (Elem Types.Vec5))

deriving stock instance Show Vec5Op
deriving newtype instance Arbitrary Vec5Op
