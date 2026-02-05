{-# OPTIONS_GHC -Wno-orphans #-}

module Test.HsBindgen.Runtime.SizedByteArray (tests) where

import Data.Primitive.ByteArray qualified as BA
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import GHC.TypeNats (KnownNat)
import Test.QuickCheck (Arbitrary (..), Property, ioProperty, vectorOf, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Testable, testProperty)

import HsBindgen.Runtime.Internal.SizedByteArray (SizedByteArray (..))
import HsBindgen.Runtime.Marshal

import Test.Util.Orphans ()

{-------------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------------}

-- | Create a 'SizedByteArray' from a list of bytes
--
-- This number of bytes must match the size of the array.
fromBytes :: forall n m.
     (KnownNat n, KnownNat m)
  => [Word8]
  -> SizedByteArray n m
fromBytes bytes
    | length bytes == size = SizedByteArray $ BA.byteArrayFromList bytes
    | otherwise            = error "fromBytes length mismatch"
  where
    size :: Int
    size = staticSizeOf (Proxy :: Proxy (SizedByteArray n m))

-- | Extract bytes from a 'SizedByteArray'
toBytes :: SizedByteArray n m -> [Word8]
toBytes (SizedByteArray arr) = BA.foldrByteArray (:) [] arr

-- | Read bytes using 'Storable'
--
-- This does /not/ use the 'Storable' instance of 'SizedByteArray'.
peekSBA :: forall n m.
     (KnownNat n, KnownNat m)
  => Ptr (SizedByteArray n m)
  -> IO (SizedByteArray n m)
peekSBA ptr = do
    let ptr' = castPtr ptr
        size = staticSizeOf (Proxy :: Proxy (SizedByteArray n m))
    fromBytes <$> mapM (peekElemOff ptr') [0 .. size - 1]

-- | Write bytes using 'Storable'
--
-- This does /not/ use the 'Storable' instance of 'SizedByteArray'.
pokeSBA :: Ptr a -> (SizedByteArray n m) -> IO ()
pokeSBA ptr sba = do
    let ptr' = castPtr ptr
    mapM_ (uncurry (pokeElemOff ptr')) $ zip [0..] (toBytes sba)

{-------------------------------------------------------------------------------
  Arbitrary instances
-------------------------------------------------------------------------------}

instance (KnownNat n, KnownNat m) => Arbitrary (SizedByteArray n m) where
    arbitrary =
      let size = staticSizeOf (Proxy :: Proxy (SizedByteArray n m))
      in  fromBytes <$> vectorOf size (arbitrary @Word8)

    shrink sba = case span (== 0x00) (toBytes sba) of
      (ls, (_:rs)) -> [fromBytes (ls ++ 0x00 : rs)]
      (_,  [])     -> []

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.Runtime.SizedByteArray" [
      testGroup "StaticSize" [
          test_staticSizeOf
        , test_staticAlignment
        ]
    , testGroup "ReadRaw+WriteRaw" [
          testGroup "readRaw reads" [
              aux8  prop_readRaw
            , aux16 prop_readRaw
            , aux32 prop_readRaw
            ]
        , testGroup "writeRaw writes" [
              aux8  prop_writeRaw
            , aux16 prop_writeRaw
            , aux32 prop_writeRaw
            ]
        , testGroup "read read (reading twice gets the same results)" [
              aux8  prop_readRead
            , aux16 prop_readRead
            , aux32 prop_readRead
            ]
        , testGroup "write read (you get back what you put in)" [
              aux8  prop_writeRead
            , aux16 prop_writeRead
            , aux32 prop_writeRead
            ]
        , testGroup "read write (putting back what you got out has no effect)" [
              aux8  prop_readWrite
            , aux16 prop_readWrite
            , aux32 prop_readWrite
            ]
        , testGroup "write write (putting twice is same as putting once)" [
              aux8  prop_writeWrite
            , aux16 prop_writeWrite
            , aux32 prop_writeWrite
            ]
        ]
    ]
  where
    aux8 :: Testable a => (Proxy (SizedByteArray 8 2) -> a) -> TestTree
    aux8 mkProp = testProperty "SizedByteArray 8 2" $ mkProp Proxy

    aux16 :: Testable a => (Proxy (SizedByteArray 16 4) -> a) -> TestTree
    aux16 mkProp = testProperty "SizedByteArray 16 4" $ mkProp Proxy

    aux32 :: Testable a => (Proxy (SizedByteArray 32 8) -> a) -> TestTree
    aux32 mkProp = testProperty "SizedByteArray 32 8" $ mkProp Proxy

{-------------------------------------------------------------------------------
  StaticSize
-------------------------------------------------------------------------------}

test_staticSizeOf :: TestTree
test_staticSizeOf = testGroup "staticSizeof" [
      testCase "4"  $ staticSizeOf (Proxy :: Proxy (SizedByteArray  4 2)) @?= 4
    , testCase "8"  $ staticSizeOf (Proxy :: Proxy (SizedByteArray  8 2)) @?= 8
    , testCase "16" $ staticSizeOf (Proxy :: Proxy (SizedByteArray 16 2)) @?= 16
    , testCase "32" $ staticSizeOf (Proxy :: Proxy (SizedByteArray 32 2)) @?= 32
    ]

test_staticAlignment :: TestTree
test_staticAlignment = testGroup "staticAlignment" [
      testCase "1" $ staticAlignment (Proxy :: Proxy (SizedByteArray 8 1)) @?= 1
    , testCase "2" $ staticAlignment (Proxy :: Proxy (SizedByteArray 8 2)) @?= 2
    , testCase "4" $ staticAlignment (Proxy :: Proxy (SizedByteArray 8 4)) @?= 4
    , testCase "8" $ staticAlignment (Proxy :: Proxy (SizedByteArray 8 8)) @?= 8
    ]

{-------------------------------------------------------------------------------
  ReadRaw+WriteRaw
-------------------------------------------------------------------------------}

prop_readRaw :: forall n m.
     (KnownNat n, KnownNat m)
  => Proxy (SizedByteArray n m)
  -> SizedByteArray n m
  -> Property
prop_readRaw proxy sba = ioProperty $
    allocaBytes (staticSizeOf proxy) $ \ptr -> do
      pokeSBA ptr sba
      sba' <- readRaw ptr
      return $ sba === sba'

prop_writeRaw :: forall n m.
     (KnownNat n, KnownNat m)
  => Proxy (SizedByteArray n m)
  -> SizedByteArray n m
  -> Property
prop_writeRaw proxy sba = ioProperty $
    allocaBytes (staticSizeOf proxy) $ \ptr -> do
      writeRaw ptr sba
      sba' <- peekSBA ptr
      return $ sba === sba'

prop_readRead :: forall n m.
     (KnownNat n, KnownNat m)
  => Proxy (SizedByteArray n m)
  -> SizedByteArray n m
  -> Property
prop_readRead proxy sba = ioProperty $
    allocaBytes (staticSizeOf proxy) $ \ptr -> do
      pokeSBA (ptr :: Ptr (SizedByteArray 8 2)) sba
      sba2 <- readRaw ptr
      sba3 <- readRaw ptr
      return $ sba2 === sba3

prop_writeRead :: forall n m.
     (KnownNat n, KnownNat m)
  => Proxy (SizedByteArray n m)
  -> SizedByteArray n m
  -> Property
prop_writeRead proxy sba = ioProperty $
    allocaBytes (staticSizeOf proxy) $ \ptr -> do
      writeRaw ptr sba
      sba' <- readRaw ptr
      return $ sba === sba'

prop_readWrite :: forall n m.
     (KnownNat n, KnownNat m)
  => Proxy (SizedByteArray n m)
  -> SizedByteArray n m
  -> Property
prop_readWrite proxy sba = ioProperty $
    allocaBytes (staticSizeOf proxy) $ \ptr -> do
      pokeSBA ptr sba
      writeRaw ptr =<< readRaw ptr
      sba' <- peekSBA ptr
      return $ sba === sba'

prop_writeWrite :: forall n m.
     (KnownNat n, KnownNat m)
  => Proxy (SizedByteArray n m)
  -> SizedByteArray n m
  -> Property
prop_writeWrite proxy sba = ioProperty $
    allocaBytes (staticSizeOf proxy) $ \ptr -> do
      writeRaw ptr sba
      sba2 <- peekSBA ptr
      writeRaw ptr sba
      sba3 <- peekSBA ptr
      return $ sba2 === sba3
