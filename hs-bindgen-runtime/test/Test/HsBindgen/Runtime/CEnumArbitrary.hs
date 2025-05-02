{-# OPTIONS_GHC -Wno-orphans #-}

module Test.HsBindgen.Runtime.CEnumArbitrary () where

import Test.QuickCheck (Arbitrary (arbitrary), Small (Small), Gen)

import HsBindgen.Runtime.CEnum (CEnum (CEnumZ, toCEnum),
                                AsCEnum (WrapCEnum),
                                AsSequentialCEnum (WrapSequentialCEnum))

instance CEnum a => Arbitrary (AsCEnum a) where
  arbitrary = do
    (Small n) <- arbitrary :: Gen (Small (CEnumZ a))
    pure $ WrapCEnum $ toCEnum n

instance CEnum a => Arbitrary (AsSequentialCEnum a) where
  arbitrary = do
    (Small n) <- arbitrary :: Gen (Small (CEnumZ a))
    pure $ WrapSequentialCEnum $ toCEnum n
