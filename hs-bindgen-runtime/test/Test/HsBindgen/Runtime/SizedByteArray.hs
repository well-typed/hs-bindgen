{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.HsBindgen.Runtime.SizedByteArray (tests) where

import Data.Array.Byte (ByteArray (..))
import Data.Primitive.ByteArray qualified as BA
import Data.Primitive.PrimArray qualified as PA
import Data.Primitive.Ptr qualified as PP
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64, Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))
import GHC.TypeNats (KnownNat)
import Test.QuickCheck (Arbitrary (..), Positive (..), Property, Small (..),
                        ioProperty, vectorOf, (===))
import Test.QuickCheck.Classes (Laws (..), primLaws)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperties, testProperty)

import HsBindgen.Runtime.SizedByteArray (SizedByteArray (..))

{-------------------------------------------------------------------------------
  Arbitrary instances
-------------------------------------------------------------------------------}

instance (KnownNat n, KnownNat m) => Arbitrary (SizedByteArray n m) where
    arbitrary = do
        let size = sizeOf (undefined :: SizedByteArray n m)
        bytes <- vectorOf size arbitrary
        return $ mkSizedByteArray bytes

    shrink sba =
        let bytes = toBytes sba
        in [mkSizedByteArray bytes' | bytes' <- shrink bytes]

{-------------------------------------------------------------------------------
  Helper: Create SizedByteArray from bytes
-------------------------------------------------------------------------------}

-- Create a SizedByteArray from a list of bytes
mkSizedByteArray :: forall n m. (KnownNat n, KnownNat m) => [Word8] -> SizedByteArray n m
mkSizedByteArray bytes =
    SizedByteArray $ BA.runByteArray $ do
        let n = sizeOf (undefined :: SizedByteArray n m)
        arr <- BA.newByteArray n
        mapM_ (uncurry (BA.writeByteArray arr)) (zip [0..] bytes)
        return arr

-- Extract bytes from SizedByteArray
toBytes :: forall n m. (KnownNat n, KnownNat m) => SizedByteArray n m -> [Word8]
toBytes (SizedByteArray (ByteArray ba)) =
    let n = sizeOf (undefined :: SizedByteArray n m)
    in map (\i -> BA.indexByteArray (ByteArray ba) i) [0..n-1]

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.Runtime.SizedByteArray" [
      testGroup "Prim instance" [
          testGroup "ByteArray operations" [
              testCase "indexByteArray single element" test_indexByteArray_single
            , testCase "indexByteArray multiple elements" test_indexByteArray_multiple
            , testProperty "readByteArray roundtrip" prop_readByteArray_roundtrip
            , testProperty "writeByteArray roundtrip" prop_writeByteArray_roundtrip
            ]
        , testGroup "Addr operations" [
              testProperty "readOffAddr roundtrip" prop_readOffAddr_roundtrip
            , testProperty "writeOffAddr roundtrip" prop_writeOffAddr_roundtrip
            , testProperty "indexOffAddr matches readOffAddr" prop_indexOffAddr_consistency
            ]
        , testGroup "PrimArray integration" [
              testCase "create and index PrimArray" test_primArray_basic
            , testProperty "PrimArray roundtrip" prop_primArray_roundtrip
            ]
        , testGroup "Size and alignment" [
              testCase "sizeOf# matches type parameter" test_sizeOf
            , testCase "alignment# matches type parameter" test_alignment
            ]
        , testGroup "quickcheck-classes primLaws" [
              testProperties "SizedByteArray 4 4"
                (lawsProperties (primLaws (Proxy @(SizedByteArray 4 4))))
            , testProperties "SizedByteArray 8 8"
                (lawsProperties (primLaws (Proxy @(SizedByteArray 8 8))))
            , testProperties "SizedByteArray 16 8"
                (lawsProperties (primLaws (Proxy @(SizedByteArray 16 8))))
            , testProperties "SizedByteArray 32 16"
                (lawsProperties (primLaws (Proxy @(SizedByteArray 32 16))))
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  ByteArray operations tests
-------------------------------------------------------------------------------}

test_indexByteArray_single :: IO ()
test_indexByteArray_single = do
    -- Create a ByteArray containing one SizedByteArray of 4 bytes
    let input = mkSizedByteArray @4 @4 [0x12, 0x34, 0x56, 0x78]

    -- Create a PrimArray with one element
    let arr = PA.primArrayFromList [input]
    let result = PA.indexPrimArray arr 0

    toBytes result @?= [0x12, 0x34, 0x56, 0x78]

test_indexByteArray_multiple :: IO ()
test_indexByteArray_multiple = do
    -- Create three SizedByteArrays
    let input1 = mkSizedByteArray @4 @4 [0x11, 0x22, 0x33, 0x44]
    let input2 = mkSizedByteArray @4 @4 [0x55, 0x66, 0x77, 0x88]
    let input3 = mkSizedByteArray @4 @4 [0xAA, 0xBB, 0xCC, 0xDD]

    -- Create a PrimArray with three elements
    let arr = PA.primArrayFromList [input1, input2, input3]

    toBytes (PA.indexPrimArray arr 0) @?= [0x11, 0x22, 0x33, 0x44]
    toBytes (PA.indexPrimArray arr 1) @?= [0x55, 0x66, 0x77, 0x88]
    toBytes (PA.indexPrimArray arr 2) @?= [0xAA, 0xBB, 0xCC, 0xDD]

prop_readByteArray_roundtrip :: [Word8] -> Positive (Small Int) -> Property
prop_readByteArray_roundtrip bytes (Positive (Small idx)) = ioProperty $ do
    let input = mkSizedByteArray @8 @8 (take 8 $ bytes ++ repeat 0)
    let arraySize = 8 * (idx + 1)  -- Ensure array is large enough for the index

    -- Create a mutable array, write the value at index idx, read it back
    marr <- BA.newByteArray arraySize
    BA.writeByteArray marr idx input
    result <- BA.readByteArray marr idx

    return $ toBytes input === toBytes (result :: SizedByteArray 8 8)

prop_writeByteArray_roundtrip :: [Word8] -> Positive (Small Int) -> Property
prop_writeByteArray_roundtrip bytes (Positive (Small idx)) = ioProperty $ do
    let input = mkSizedByteArray @8 @8 (take 8 $ bytes ++ repeat 0)
    let arraySize = 8 * (idx + 1)  -- Ensure array is large enough for the index

    -- Create a mutable array and write to it at index idx
    marr <- BA.newByteArray arraySize
    BA.writeByteArray marr idx input
    arr <- BA.freezeByteArray marr 0 arraySize

    -- Read back using indexByteArray at index idx
    let result = BA.indexByteArray arr idx :: SizedByteArray 8 8

    return $ toBytes input === toBytes result

{-------------------------------------------------------------------------------
  Addr operations tests
-------------------------------------------------------------------------------}

prop_readOffAddr_roundtrip :: Word32 -> Positive (Small Int) -> Property
prop_readOffAddr_roundtrip val (Positive (Small idx)) = ioProperty $ do
    let arraySize = 4 * (idx + 1)  -- Ensure array is large enough for the index
    allocaBytes arraySize $ \ptr -> do
        -- Write a Word32 value at offset idx
        pokeElemOff ptr idx val

        -- Read it as SizedByteArray at index idx
        result <- PP.readOffPtr (castPtr ptr) idx :: IO (SizedByteArray 4 4)

        -- Write it back to a different location
        allocaBytes 4 $ \ptr2 -> do
            PP.writeOffPtr (castPtr ptr2) 0 result

            -- Read it back as Word32
            val' <- peek ptr2
            return $ val === val'

prop_writeOffAddr_roundtrip :: Word64 -> Positive (Small Int) -> Property
prop_writeOffAddr_roundtrip val (Positive (Small idx)) = ioProperty $ do
    let arraySize = 8 * (idx + 1)  -- Ensure array is large enough for the index
    allocaBytes 8 $ \ptr -> do
        -- Create a SizedByteArray from Word64
        poke ptr val
        input <- PP.readOffPtr (castPtr ptr) 0 :: IO (SizedByteArray 8 8)

        -- Write it to a new location at index idx
        allocaBytes arraySize $ \ptr2 -> do
            PP.writeOffPtr (castPtr ptr2) idx input

            -- Read back as Word64 from index idx
            val' <- peekElemOff ptr2 idx
            return $ val === val'

prop_indexOffAddr_consistency :: Word32 -> Positive (Small Int) -> Property
prop_indexOffAddr_consistency val (Positive (Small idx)) = ioProperty $ do
    let arraySize = 4 * (idx + 1)  -- Ensure array is large enough for the index
    allocaBytes arraySize $ \ptr -> do
        -- Write value at index idx
        pokeElemOff ptr idx val

        -- Read using indexOffPtr at index idx
        let indexed = PP.indexOffPtr (castPtr ptr) idx :: SizedByteArray 4 4

        -- Read using readOffPtr at index idx
        readResult <- PP.readOffPtr (castPtr ptr) idx :: IO (SizedByteArray 4 4)

        return $ toBytes indexed === toBytes readResult

{-------------------------------------------------------------------------------
  PrimArray integration tests
-------------------------------------------------------------------------------}

test_primArray_basic :: IO ()
test_primArray_basic = do
    -- Create a PrimArray of SizedByteArrays
    let val1 = mkSizedByteArray @4 @4 [1, 2, 3, 4]
    let val2 = mkSizedByteArray @4 @4 [5, 6, 7, 8]

    marr <- PA.newPrimArray 2
    PA.writePrimArray marr 0 val1
    PA.writePrimArray marr 1 val2

    arr <- PA.freezePrimArray marr 0 2

    toBytes (PA.indexPrimArray arr 0) @?= [1, 2, 3, 4]
    toBytes (PA.indexPrimArray arr 1) @?= [5, 6, 7, 8]

prop_primArray_roundtrip :: [Word8] -> [Word8] -> [Word8] -> Property
prop_primArray_roundtrip b1 b2 b3 = ioProperty $ do
    let val1 = mkSizedByteArray @8 @8 (take 8 $ b1 ++ repeat 0)
    let val2 = mkSizedByteArray @8 @8 (take 8 $ b2 ++ repeat 0)
    let val3 = mkSizedByteArray @8 @8 (take 8 $ b3 ++ repeat 0)

    -- Create array using primArrayFromList
    let arr = PA.primArrayFromList [val1, val2, val3]

    -- Read back
    let r1 = PA.indexPrimArray arr 0
    let r2 = PA.indexPrimArray arr 1
    let r3 = PA.indexPrimArray arr 2

    return $ (toBytes val1, toBytes val2, toBytes val3) ===
             (toBytes r1, toBytes r2, toBytes r3)

{-------------------------------------------------------------------------------
  Size and alignment tests
-------------------------------------------------------------------------------}

test_sizeOf :: IO ()
test_sizeOf = do
    sizeOf (undefined :: SizedByteArray 4 4) @?= 4
    sizeOf (undefined :: SizedByteArray 8 8) @?= 8
    sizeOf (undefined :: SizedByteArray 16 8) @?= 16
    sizeOf (undefined :: SizedByteArray 32 16) @?= 32

test_alignment :: IO ()
test_alignment = do
    alignment (undefined :: SizedByteArray 4 4) @?= 4
    alignment (undefined :: SizedByteArray 8 8) @?= 8
    alignment (undefined :: SizedByteArray 16 8) @?= 8
    alignment (undefined :: SizedByteArray 32 16) @?= 16
