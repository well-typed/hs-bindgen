{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.HsBindgen.Runtime.SizedByteArray (tests) where

import Data.Array.Byte (ByteArray (..))
import Data.Primitive.ByteArray qualified as BA
import Data.Primitive.PrimArray qualified as PA
import Data.Primitive.Ptr qualified as PP
import Data.Word (Word32, Word64, Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))
import GHC.TypeNats (KnownNat)
import Test.QuickCheck (Property, ioProperty, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import HsBindgen.Runtime.SizedByteArray (SizedByteArray (..))

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
        ]
    ]

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

prop_readByteArray_roundtrip :: [Word8] -> Property
prop_readByteArray_roundtrip bytes = ioProperty $ do
    let input = mkSizedByteArray @8 @8 (take 8 $ bytes ++ repeat 0)

    -- Create a mutable array, write the value, read it back
    marr <- BA.newByteArray 8
    BA.writeByteArray marr 0 input
    result <- BA.readByteArray marr 0

    return $ toBytes input === toBytes (result :: SizedByteArray 8 8)

prop_writeByteArray_roundtrip :: [Word8] -> Property
prop_writeByteArray_roundtrip bytes = ioProperty $ do
    let input = mkSizedByteArray @8 @8 (take 8 $ bytes ++ repeat 0)

    -- Create a mutable array and write to it
    marr <- BA.newByteArray 8
    BA.writeByteArray marr 0 input
    arr <- BA.freezeByteArray marr 0 8

    -- Read back using indexByteArray
    let result = BA.indexByteArray arr 0 :: SizedByteArray 8 8

    return $ toBytes input === toBytes result

{-------------------------------------------------------------------------------
  Addr operations tests
-------------------------------------------------------------------------------}

prop_readOffAddr_roundtrip :: Word32 -> Property
prop_readOffAddr_roundtrip val = ioProperty $
    allocaBytes 4 $ \ptr -> do
        -- Write a Word32 value
        poke ptr val

        -- Read it as SizedByteArray
        result <- PP.readOffPtr (castPtr ptr) 0 :: IO (SizedByteArray 4 4)

        -- Write it back to a different location
        allocaBytes 4 $ \ptr2 -> do
            PP.writeOffPtr (castPtr ptr2) 0 result

            -- Read it back as Word32
            val' <- peek ptr2
            return $ val === val'

prop_writeOffAddr_roundtrip :: Word64 -> Property
prop_writeOffAddr_roundtrip val = ioProperty $
    allocaBytes 8 $ \ptr -> do
        -- Create a SizedByteArray from Word64
        poke ptr val
        input <- PP.readOffPtr (castPtr ptr) 0 :: IO (SizedByteArray 8 8)

        -- Write it to a new location
        allocaBytes 8 $ \ptr2 -> do
            PP.writeOffPtr (castPtr ptr2) 0 input

            -- Read back as Word64
            val' <- peek ptr2
            return $ val === val'

prop_indexOffAddr_consistency :: Word32 -> Property
prop_indexOffAddr_consistency val = ioProperty $
    allocaBytes 4 $ \ptr -> do
        poke ptr val

        -- Read using indexOffPtr
        let indexed = PP.indexOffPtr (castPtr ptr) 0 :: SizedByteArray 4 4

        -- Read using readOffPtr
        readResult <- PP.readOffPtr (castPtr ptr) 0 :: IO (SizedByteArray 4 4)

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
