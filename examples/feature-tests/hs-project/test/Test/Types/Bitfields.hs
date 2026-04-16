-- | C bit-field tests
--
-- This module implements two types of tests for various @struct@s:
--
-- * Peek tests use a C function to set the fields of a @struct@, peek the
--   @struct@, and check that the read field values are as expected.
-- * Poke tests poke the @struct@ and then use a C function to check that the
--   fields are as expected.
--
-- Much of the code in this module follows the same pattern.  This has not been
-- abstracted, using TH for example, because this whole module will eventually
-- be replaced with generated tests.

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Types.Bitfields (tests) where

import Data.Bits ((.&.))
import Data.Bits qualified as Bits
import Data.Maybe qualified as Maybe
import Data.Proxy
import Data.Word
import Foreign qualified
import Foreign.C qualified as C
import Test.QuickCheck ((===))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import HsBindgen.Runtime.Internal.Bitfield (Bitfield)
import HsBindgen.Runtime.Internal.Bitfield qualified as Bitfield
import HsBindgen.Runtime.Marshal qualified as Marshal

import Generated.Types.Bitfields qualified as Bitfields
import Generated.Types.Bitfields.Unsafe qualified as Bitfields

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Get an initial value from some bits in a random 'Word64'
initValue :: Bitfield a => Int -> Int -> Word64 -> a
initValue off width w64 =
    Bitfield.extend (Bits.shiftR w64 off .&. Bitfield.loMask width) width

allocaAligned :: forall a b.
     Marshal.StaticSize a
  => (Foreign.Ptr a -> IO b)
  -> IO b
allocaAligned = Foreign.allocaBytesAligned size 8
  where
    size :: Int
    size = Marshal.staticSizeOf @a Proxy

{-------------------------------------------------------------------------------
  foo_8: not packed, <=8-bit fields
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Foo_8 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Foo_8{
        Bitfields.foo_8_a = initValue  0 3 w1
      , Bitfields.foo_8_b = initValue  3 3 w1
      , Bitfields.foo_8_c = initValue  6 2 w1
      , Bitfields.foo_8_d = initValue  8 3 w1
      , Bitfields.foo_8_e = initValue 11 8 w1
      , Bitfields.foo_8_f = initValue 19 5 w1
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.foo_8_a x /= 0
          then Just x{ Bitfields.foo_8_a = 0 }
          else Nothing
      , if Bitfields.foo_8_b x /= 0
          then Just x{ Bitfields.foo_8_b = 0 }
          else Nothing
      , if Bitfields.foo_8_c x /= 0
          then Just x{ Bitfields.foo_8_c = 0 }
          else Nothing
      , if Bitfields.foo_8_d x /= 0
          then Just x{ Bitfields.foo_8_d = 0 }
          else Nothing
      , if Bitfields.foo_8_e x /= 0
          then Just x{ Bitfields.foo_8_e = 0 }
          else Nothing
      , if Bitfields.foo_8_f x /= 0
          then Just x{ Bitfields.foo_8_f = 0 }
          else Nothing
      ]

test_foo_8 :: TestTree
test_foo_8 = testGroup "<=8-bit fields" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Foo_8 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_foo_8
          ptr
          (Bitfields.foo_8_a x)
          (Bitfields.foo_8_b x)
          (Bitfields.foo_8_c x)
          (Bitfields.foo_8_d x)
          (Bitfields.foo_8_e x)
          (Bitfields.foo_8_f x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Foo_8 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_foo_8
          ptr
          (Bitfields.foo_8_a x)
          (Bitfields.foo_8_b x)
          (Bitfields.foo_8_c x)
          (Bitfields.foo_8_d x)
          (Bitfields.foo_8_e x)
          (Bitfields.foo_8_f x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  foo_16: not packed, <=16-bit fields
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Foo_16 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    w2 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Foo_16{
        Bitfields.foo_16_a = initValue  0  6 w1
      , Bitfields.foo_16_b = initValue  6 10 w1
      , Bitfields.foo_16_c = initValue 16 16 w1
      , Bitfields.foo_16_d = initValue 32 16 w1
      , Bitfields.foo_16_e = initValue 48 12 w1
      , Bitfields.foo_16_f = initValue  0 12 w2
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.foo_16_a x /= 0
          then Just x{ Bitfields.foo_16_a = 0 }
          else Nothing
      , if Bitfields.foo_16_b x /= 0
          then Just x{ Bitfields.foo_16_b = 0 }
          else Nothing
      , if Bitfields.foo_16_c x /= 0
          then Just x{ Bitfields.foo_16_c = 0 }
          else Nothing
      , if Bitfields.foo_16_d x /= 0
          then Just x{ Bitfields.foo_16_d = 0 }
          else Nothing
      , if Bitfields.foo_16_e x /= 0
          then Just x{ Bitfields.foo_16_e = 0 }
          else Nothing
      , if Bitfields.foo_16_f x /= 0
          then Just x{ Bitfields.foo_16_f = 0 }
          else Nothing
      ]

test_foo_16 :: TestTree
test_foo_16 = testGroup "<=16-bit fields" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Foo_16 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_foo_16
          ptr
          (Bitfields.foo_16_a x)
          (Bitfields.foo_16_b x)
          (Bitfields.foo_16_c x)
          (Bitfields.foo_16_d x)
          (Bitfields.foo_16_e x)
          (Bitfields.foo_16_f x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Foo_16 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_foo_16
          ptr
          (Bitfields.foo_16_a x)
          (Bitfields.foo_16_b x)
          (Bitfields.foo_16_c x)
          (Bitfields.foo_16_d x)
          (Bitfields.foo_16_e x)
          (Bitfields.foo_16_f x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  foo_32: not packed, <=32-bit fields
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Foo_32 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    w2 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Foo_32{
        Bitfields.foo_32_a = initValue  0  6 w1
      , Bitfields.foo_32_b = initValue  6 12 w1
      , Bitfields.foo_32_c = initValue 18 14 w1
      , Bitfields.foo_32_d = initValue 42 10 w1
      , Bitfields.foo_32_e = initValue  0 32 w2
      , Bitfields.foo_32_f = initValue 32  6 w2
      , Bitfields.foo_32_g = initValue 38 24 w2
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.foo_32_a x /= 0
          then Just x{ Bitfields.foo_32_a = 0 }
          else Nothing
      , if Bitfields.foo_32_b x /= 0
          then Just x{ Bitfields.foo_32_b = 0 }
          else Nothing
      , if Bitfields.foo_32_c x /= 0
          then Just x{ Bitfields.foo_32_c = 0 }
          else Nothing
      , if Bitfields.foo_32_d x /= 0
          then Just x{ Bitfields.foo_32_d = 0 }
          else Nothing
      , if Bitfields.foo_32_e x /= 0
          then Just x{ Bitfields.foo_32_e = 0 }
          else Nothing
      , if Bitfields.foo_32_f x /= 0
          then Just x{ Bitfields.foo_32_f = 0 }
          else Nothing
      , if Bitfields.foo_32_g x /= 0
          then Just x{ Bitfields.foo_32_g = 0 }
          else Nothing
      ]

test_foo_32 :: TestTree
test_foo_32 = testGroup "<=32-bit fields" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Foo_32 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_foo_32
          ptr
          (Bitfields.foo_32_a x)
          (Bitfields.foo_32_b x)
          (Bitfields.foo_32_c x)
          (Bitfields.foo_32_d x)
          (Bitfields.foo_32_e x)
          (Bitfields.foo_32_f x)
          (Bitfields.foo_32_g x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Foo_32 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_foo_32
          ptr
          (Bitfields.foo_32_a x)
          (Bitfields.foo_32_b x)
          (Bitfields.foo_32_c x)
          (Bitfields.foo_32_d x)
          (Bitfields.foo_32_e x)
          (Bitfields.foo_32_f x)
          (Bitfields.foo_32_g x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  foo_64: not packed, <=64-bit fields
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Foo_64 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    w2 <- QC.arbitrarySizedBoundedIntegral
    w3 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Foo_64{
        Bitfields.foo_64_a = initValue  0 24 w1
      , Bitfields.foo_64_b = initValue 24 40 w1
      , Bitfields.foo_64_c = initValue  0 64 w2
      , Bitfields.foo_64_d = initValue  0 36 w3
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.foo_64_a x /= 0
          then Just x{ Bitfields.foo_64_a = 0 }
          else Nothing
      , if Bitfields.foo_64_b x /= 0
          then Just x{ Bitfields.foo_64_b = 0 }
          else Nothing
      , if Bitfields.foo_64_c x /= 0
          then Just x{ Bitfields.foo_64_c = 0 }
          else Nothing
      , if Bitfields.foo_64_d x /= 0
          then Just x{ Bitfields.foo_64_d = 0 }
          else Nothing
      ]

test_foo_64 :: TestTree
test_foo_64 = testGroup "<=64-bit fields" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Foo_64 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_foo_64
          ptr
          (Bitfields.foo_64_a x)
          (Bitfields.foo_64_b x)
          (Bitfields.foo_64_c x)
          (Bitfields.foo_64_d x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Foo_64 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_foo_64
          ptr
          (Bitfields.foo_64_a x)
          (Bitfields.foo_64_b x)
          (Bitfields.foo_64_c x)
          (Bitfields.foo_64_d x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  bar_8_8: packed, <=8-bit field crosses 8-bit word boundary
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Bar_8_8 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Bar_8_8{
        Bitfields.bar_8_8_a = initValue 0 6 w1
      , Bitfields.bar_8_8_b = initValue 6 4 w1
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.bar_8_8_a x /= 0
          then Just x{ Bitfields.bar_8_8_a = 0 }
          else Nothing
      , if Bitfields.bar_8_8_b x /= 0
          then Just x{ Bitfields.bar_8_8_b = 0 }
          else Nothing
      ]

test_bar_8_8 :: TestTree
test_bar_8_8 = testGroup "<=8-bit field crosses 8-bit word boundary" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Bar_8_8 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_bar_8_8
          ptr
          (Bitfields.bar_8_8_a x)
          (Bitfields.bar_8_8_b x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Bar_8_8 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_bar_8_8
          ptr
          (Bitfields.bar_8_8_a x)
          (Bitfields.bar_8_8_b x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  bar_8_16: packed, <=8-bit field crosses 16-bit word boundary
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Bar_8_16 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Bar_8_16{
        Bitfields.bar_8_16_a = initValue  0 14 w1
      , Bitfields.bar_8_16_b = initValue 14  4 w1
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.bar_8_16_a x /= 0
          then Just x{ Bitfields.bar_8_16_a = 0 }
          else Nothing
      , if Bitfields.bar_8_16_b x /= 0
          then Just x{ Bitfields.bar_8_16_b = 0 }
          else Nothing
      ]

test_bar_8_16 :: TestTree
test_bar_8_16 = testGroup "<=8-bit field crosses 16-bit word boundary" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Bar_8_16 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_bar_8_16
          ptr
          (Bitfields.bar_8_16_a x)
          (Bitfields.bar_8_16_b x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Bar_8_16 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_bar_8_16
          ptr
          (Bitfields.bar_8_16_a x)
          (Bitfields.bar_8_16_b x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  bar_8_32: packed, <=8-bit field crosses 32-bit word boundary
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Bar_8_32 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Bar_8_32{
        Bitfields.bar_8_32_a = initValue  0 30 w1
      , Bitfields.bar_8_32_b = initValue 30  4 w1
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.bar_8_32_a x /= 0
          then Just x{ Bitfields.bar_8_32_a = 0 }
          else Nothing
      , if Bitfields.bar_8_32_b x /= 0
          then Just x{ Bitfields.bar_8_32_b = 0 }
          else Nothing
      ]

test_bar_8_32 :: TestTree
test_bar_8_32 = testGroup "<=8-bit field crosses 32-bit word boundary" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Bar_8_32 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_bar_8_32
          ptr
          (Bitfields.bar_8_32_a x)
          (Bitfields.bar_8_32_b x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Bar_8_32 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_bar_8_32
          ptr
          (Bitfields.bar_8_32_a x)
          (Bitfields.bar_8_32_b x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  bar_8_64: packed, <=8-bit field crosses 64-bit word boundary
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Bar_8_64 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    w2 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Bar_8_64{
        Bitfields.bar_8_64_a = initValue 0 62 w1
      , Bitfields.bar_8_64_b = initValue 0  4 w2
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.bar_8_64_a x /= 0
          then Just x{ Bitfields.bar_8_64_a = 0 }
          else Nothing
      , if Bitfields.bar_8_64_b x /= 0
          then Just x{ Bitfields.bar_8_64_b = 0 }
          else Nothing
      ]

test_bar_8_64 :: TestTree
test_bar_8_64 = testGroup "<=8-bit field crosses 64-bit word boundary" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Bar_8_64 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_bar_8_64
          ptr
          (Bitfields.bar_8_64_a x)
          (Bitfields.bar_8_64_b x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Bar_8_64 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_bar_8_64
          ptr
          (Bitfields.bar_8_64_a x)
          (Bitfields.bar_8_64_b x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  bar_16_16: packed, <=16-bit field crosses 16-bit word boundary
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Bar_16_16 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Bar_16_16{
        Bitfields.bar_16_16_a = initValue  0 14 w1
      , Bitfields.bar_16_16_b = initValue 14 14 w1
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.bar_16_16_a x /= 0
          then Just x{ Bitfields.bar_16_16_a = 0 }
          else Nothing
      , if Bitfields.bar_16_16_b x /= 0
          then Just x{ Bitfields.bar_16_16_b = 0 }
          else Nothing
      ]

test_bar_16_16 :: TestTree
test_bar_16_16 = testGroup "<=16-bit field crosses 16-bit word boundary" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Bar_16_16 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_bar_16_16
          ptr
          (Bitfields.bar_16_16_a x)
          (Bitfields.bar_16_16_b x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Bar_16_16 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_bar_16_16
          ptr
          (Bitfields.bar_16_16_a x)
          (Bitfields.bar_16_16_b x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  bar_16_32: packed, <=16-bit field crosses 32-bit word boundary
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Bar_16_32 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Bar_16_32{
        Bitfields.bar_16_32_a = initValue  0 24 w1
      , Bitfields.bar_16_32_b = initValue 24 14 w1
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.bar_16_32_a x /= 0
          then Just x{ Bitfields.bar_16_32_a = 0 }
          else Nothing
      , if Bitfields.bar_16_32_b x /= 0
          then Just x{ Bitfields.bar_16_32_b = 0 }
          else Nothing
      ]

test_bar_16_32 :: TestTree
test_bar_16_32 = testGroup "<=16-bit field crosses 32-bit word boundary" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Bar_16_32 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_bar_16_32
          ptr
          (Bitfields.bar_16_32_a x)
          (Bitfields.bar_16_32_b x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Bar_16_32 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_bar_16_32
          ptr
          (Bitfields.bar_16_32_a x)
          (Bitfields.bar_16_32_b x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  bar_16_64: packed, <=16-bit field crosses 64-bit word boundary
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Bar_16_64 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    w2 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Bar_16_64{
        Bitfields.bar_16_64_a = initValue 0 56 w1
      , Bitfields.bar_16_64_b = initValue 0 14 w2
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.bar_16_64_a x /= 0
          then Just x{ Bitfields.bar_16_64_a = 0 }
          else Nothing
      , if Bitfields.bar_16_64_b x /= 0
          then Just x{ Bitfields.bar_16_64_b = 0 }
          else Nothing
      ]

test_bar_16_64 :: TestTree
test_bar_16_64 = testGroup "<=16-bit field crosses 64-bit word boundary" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Bar_16_64 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_bar_16_64
          ptr
          (Bitfields.bar_16_64_a x)
          (Bitfields.bar_16_64_b x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Bar_16_64 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_bar_16_64
          ptr
          (Bitfields.bar_16_64_a x)
          (Bitfields.bar_16_64_b x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  bar_32_32: packed, <=32-bit field crosses 32-bit word boundary
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Bar_32_32 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Bar_32_32{
        Bitfields.bar_32_32_a = initValue  0 30 w1
      , Bitfields.bar_32_32_b = initValue 30 30 w1
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.bar_32_32_a x /= 0
          then Just x{ Bitfields.bar_32_32_a = 0 }
          else Nothing
      , if Bitfields.bar_32_32_b x /= 0
          then Just x{ Bitfields.bar_32_32_b = 0 }
          else Nothing
      ]

test_bar_32_32 :: TestTree
test_bar_32_32 = testGroup "<=32-bit field crosses 32-bit word boundary" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Bar_32_32 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_bar_32_32
          ptr
          (Bitfields.bar_32_32_a x)
          (Bitfields.bar_32_32_b x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Bar_32_32 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_bar_32_32
          ptr
          (Bitfields.bar_32_32_a x)
          (Bitfields.bar_32_32_b x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  bar_32_64: packed, <=32-bit field crosses 64-bit word boundary
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Bar_32_64 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    w2 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Bar_32_64{
        Bitfields.bar_32_64_a = initValue 0 56 w1
      , Bitfields.bar_32_64_b = initValue 0 30 w2
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.bar_32_64_a x /= 0
          then Just x{ Bitfields.bar_32_64_a = 0 }
          else Nothing
      , if Bitfields.bar_32_64_b x /= 0
          then Just x{ Bitfields.bar_32_64_b = 0 }
          else Nothing
      ]

test_bar_32_64 :: TestTree
test_bar_32_64 = testGroup "<=32-bit field crosses 64-bit word boundary" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Bar_32_64 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_bar_32_64
          ptr
          (Bitfields.bar_32_64_a x)
          (Bitfields.bar_32_64_b x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Bar_32_64 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_bar_32_64
          ptr
          (Bitfields.bar_32_64_a x)
          (Bitfields.bar_32_64_b x)
      return $ isEq === C.CBool 1


{-------------------------------------------------------------------------------
  bar_64_64: packed, <=64-bit field crosses 64-bit word boundary
-------------------------------------------------------------------------------}

instance QC.Arbitrary Bitfields.Bar_64_64 where
  arbitrary = do
    w1 <- QC.arbitrarySizedBoundedIntegral
    w2 <- QC.arbitrarySizedBoundedIntegral
    return Bitfields.Bar_64_64{
        Bitfields.bar_64_64_a = initValue 0 56 w1
      , Bitfields.bar_64_64_b = initValue 0 40 w2
      }

  shrink x = Maybe.catMaybes [
        if Bitfields.bar_64_64_a x /= 0
          then Just x{ Bitfields.bar_64_64_a = 0 }
          else Nothing
      , if Bitfields.bar_64_64_b x /= 0
          then Just x{ Bitfields.bar_64_64_b = 0 }
          else Nothing
      ]

test_bar_64_64 :: TestTree
test_bar_64_64 = testGroup "<=64-bit field crosses 64-bit word boundary" [
      testProperty "peek" peek_prop
    , testProperty "poke" poke_prop
    ]
  where
    peek_prop :: Bitfields.Bar_64_64 -> QC.Property
    peek_prop x = QC.ioProperty $ do
      y <- allocaAligned $ \ptr -> do
        Bitfields.set_bar_64_64
          ptr
          (Bitfields.bar_64_64_a x)
          (Bitfields.bar_64_64_b x)
        Foreign.peek ptr
      return $ y === x

    poke_prop :: Bitfields.Bar_64_64 -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        Bitfields.eq_bar_64_64
          ptr
          (Bitfields.bar_64_64_a x)
          (Bitfields.bar_64_64_b x)
      return $ isEq === C.CBool 1

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Types.Bitfields" [
      testGroup "non-packed" [
          test_foo_8
        , test_foo_16
        , test_foo_32
        , test_foo_64
        ]
    , testGroup "packed" [
          test_bar_8_8
        , test_bar_8_16
        , test_bar_8_32
        , test_bar_8_64
        , test_bar_16_16
        , test_bar_16_32
        , test_bar_16_64
        , test_bar_32_32
        , test_bar_32_64
        , test_bar_64_64
        ]
    ]
