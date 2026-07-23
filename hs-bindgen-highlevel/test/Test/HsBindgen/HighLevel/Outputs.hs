{-# LANGUAGE OverloadedStrings #-}

-- | Out-parameters: 'output' and the unmarshallers behind it
-- ('unmarshalOut' \/ 'unmarshalOutWith' \/ 'peekCStringOut' \/ 'byteStringOut' \/
-- 'outForeignPtr'), how several outputs flatten into one result tuple, and how
-- an output's allocation nests around a later input.
module Test.HsBindgen.HighLevel.Outputs (tests) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CChar (..), CInt (..), CUChar)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca, finalizerFree, mallocBytes)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import HsBindgen.HighLevel (discardResult, dropTrailingUnit, input, output,
                            resultPure, toHighLevel)
import HsBindgen.HighLevel.Marshaller (scalar, unmarshalOut, unmarshalOutWith)
import HsBindgen.HighLevel.Marshaller.Utils (byteStringOut, outForeignPtr,
                                             peekCStringOut, withCStringIn)

import Test.HsBindgen.HighLevel.Util (peekIntOut)

{-------------------------------------------------------------------------------
  Fixtures
-------------------------------------------------------------------------------}

-- | Fills six out-parameters with @1..6@; the return value is discarded.
sixOut :: Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt
       -> IO CInt
sixOut p1 p2 p3 p4 p5 p6 = do
    poke p1 1; poke p2 2; poke p3 3; poke p4 4; poke p5 5; poke p6 6
    pure 0

-- | Fills seven out-parameters; with the discarded return that is a full
-- eight-component result, the largest the flattening supports.
sevenOut :: Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt
         -> Ptr CInt -> IO CInt
sevenOut p1 p2 p3 p4 p5 p6 p7 = do
    poke p1 1; poke p2 2; poke p3 3; poke p4 4; poke p5 5; poke p6 6; poke p7 7
    pure 0

-- | Writes an out-parameter before and after reading an @int@ input.
interleaved :: Ptr CInt -> CInt -> Ptr CInt -> IO CInt
interleaved pOut1 (CInt n) pOut2 = do
    poke pOut1 (CInt n)
    poke pOut2 (CInt (n * 2))
    pure 0

writeInt :: Ptr CInt -> IO CInt
writeInt p = poke p (CInt 41) >> pure 0

-- | Writes @"boom"@ into the output buffer and returns a status.
writeErrorString :: Ptr CChar -> IO CInt
writeErrorString buf = pokeArray buf [c 'b', c 'o', c 'o', c 'm', CChar 0] >> pure 1
  where c = CChar . fromIntegral . fromEnum

-- | Writes the first byte of the input string into the output buffer.
firstCharInto :: Ptr CChar -> PtrConst CChar -> IO CInt
firstCharInto outBuf inStr = do
    c <- PtrConst.peek inStr
    pokeArray outBuf [c, CChar 0]
    pure 1

writeThreeBytes :: Ptr CUChar -> IO CInt
writeThreeBytes p = pokeArray p [0x61, 0x62, 0x63] >> pure 0   -- "abc"

-- | Mallocs a @CInt@, pokes @v@, and hands the pointer back through a @T**@
-- out-parameter (the @thing_open@ idiom).
openThing :: CInt -> Ptr (Ptr CInt) -> IO CInt
openThing v outp = do
    p <- mallocBytes 8
    poke p v
    poke outp p
    pure 0

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "outputs"
    [ testGroup "flattening"
        [ testCase "six outputs flatten into one tuple, trailing () from the void closer" $
            hsSixOut >>= (@?= (1, 2, 3, 4, 5, 6, ()))
        , testCase "eight components is the largest result the flattening supports" $
            hsSevenOut >>= (@?= (1, 2, 3, 4, 5, 6, 7, ()))
        , testCase "interleaved out-in-out: the outputs sandwich an input" $
            hsInterleaved 7 >>= (@?= (7, 14, 0))
        ]

    , testGroup "unmarshallers"
        [ testCase "unmarshalOut: read an out-parameter with an IO conversion" $
            hsUnmarshalOut >>= (@?= (42, 0))
        , testCase "unmarshalOutWith: a custom allocator and reader" $
            hsUnmarshalOutWith >>= (@?= (41, 0))
        , testCase "peekCStringOut: a fixed-cap NUL-terminated buffer" $
            hsWriteError >>= (@?= ("boom", 1))
        , testCase "byteStringOut: a caller-sized buffer read back as bytes" $
            hsBytesOut >>= (@?= ("abc", 0))
        , testCase "output then bracket input: the input nests inside the output" $
            hsFirstChar "xyz" >>= (@?= ("x", 1))
        ]

    , testCase "outForeignPtr: a T** out-parameter becomes a managed ForeignPtr" $
        hsOpenThing 55 >>= (@?= 55)
    ]

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

hsSixOut :: IO (Int, Int, Int, Int, Int, Int, ())
hsSixOut = toHighLevel
    ( output peekIntOut $ output peekIntOut $ output peekIntOut
    $ output peekIntOut $ output peekIntOut $ output peekIntOut
    $ discardResult
    ) sixOut

hsSevenOut :: IO (Int, Int, Int, Int, Int, Int, Int, ())
hsSevenOut = toHighLevel
    ( output peekIntOut $ output peekIntOut $ output peekIntOut
    $ output peekIntOut $ output peekIntOut $ output peekIntOut
    $ output peekIntOut
    $ discardResult
    ) sevenOut

hsInterleaved :: Int -> IO (Int, Int, Int)
hsInterleaved = toHighLevel
    ( output peekIntOut
    $ input (scalar (CInt . fromIntegral))
    $ output peekIntOut
    $ resultPure fromIntegral
    ) interleaved

hsUnmarshalOut :: IO (Int, Int)
hsUnmarshalOut = toHighLevel
    ( output (unmarshalOut (\(CInt n) -> pure (fromIntegral n + 1)))
    $ resultPure fromIntegral
    ) writeInt

hsUnmarshalOutWith :: IO (Int, Int)
hsUnmarshalOutWith = toHighLevel
    (output (unmarshalOutWith alloca readInt) $ resultPure fromIntegral) writeInt
  where
    readInt :: Ptr CInt -> IO Int
    readInt p = (\(CInt n) -> fromIntegral n) <$> peek p

hsWriteError :: IO (String, Int)
hsWriteError = toHighLevel
    (output (peekCStringOut 32) $ resultPure fromIntegral) writeErrorString

hsBytesOut :: IO (ByteString, Int)
hsBytesOut = toHighLevel
    (output (byteStringOut 3) $ resultPure fromIntegral) writeThreeBytes

hsFirstChar :: String -> IO (String, Int)
hsFirstChar = toHighLevel
    ( output (peekCStringOut 8)
    $ input withCStringIn
    $ resultPure fromIntegral
    ) firstCharInto

hsOpenThing :: Int -> IO Int
hsOpenThing v = do
    fp <- toHighLevel
            ( dropTrailingUnit
            $ input (scalar (fromIntegral :: Int -> CInt))
            $ output (outForeignPtr finalizerFree)
            $ discardResult
            ) openThing v
    withForeignPtr fp (fmap fromIntegral . peek)
