{-# OPTIONS_GHC -Wno-orphans #-}

module Test.HsBindgen.Runtime.CEnumArbitrary () where

import Test.QuickCheck (Arbitrary (arbitrary), Gen, Small (Small))

import HsBindgen.Runtime.CEnum (AsCEnum (WrapCEnum),
                                AsSequentialCEnum (WrapSequentialCEnum),
                                CEnum (CEnumZ, toCEnum))

instance CEnum a => Arbitrary (AsCEnum a) where
  arbitrary = do
    (Small n) <- arbitrary :: Gen (Small (CEnumZ a))
    pure $ WrapCEnum $ toCEnum n

instance CEnum a => Arbitrary (AsSequentialCEnum a) where
  arbitrary = do
    (Small n) <- arbitrary :: Gen (Small (CEnumZ a))
    pure $ WrapSequentialCEnum $ toCEnum n
