{-# LANGUAGE TypeApplications #-}

-- | Hidden C arguments the wrapper does not expose: 'scratch' and
-- 'scratchArray' (buffers the callee writes into), 'fixed' (a pinned constant),
-- and 'nullConst' (a pinned NULL pointer).
module Test.HsBindgen.HighLevel.Scratch (tests) where

import Foreign.C.Types (CChar (..), CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import HsBindgen.HighLevel (fixed, input, output, resultPure, scratch,
                            scratchArray, toHighLevel)
import HsBindgen.HighLevel.Marshaller (Unmarshaller, scalar)
import HsBindgen.HighLevel.Marshaller.Utils (nullConst, peekIncompleteArrayOut)

import Test.HsBindgen.HighLevel.Util (cAdd)

{-------------------------------------------------------------------------------
  Fixtures
-------------------------------------------------------------------------------}

-- | Writes into a scratch buffer it is handed (the caller never sees it) and
-- returns a fixed status.
writeScratch :: Ptr CChar -> IO CInt
writeScratch buf = poke buf (CChar 7) >> pure 5

-- | Ignores a scratch buffer and fills the real @int@ output array.
writeBuffers :: Ptr CChar -> Ptr CInt -> IO CInt
writeBuffers _scratchBuf outBuf = pokeArray outBuf [CInt 10, CInt 20, CInt 30] >> pure 0

-- | Reports whether a @const@ pointer argument arrived NULL.
reportNull :: PtrConst CChar -> IO CInt
reportNull p = pure (if PtrConst.unsafeToPtr p == nullPtr then 1 else 0)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "scratch and fixed arguments"
    [ testCase "scratch: a hidden buffer contributes no argument or result" $
        hsScratch >>= (@?= 5)

    , testCase "scratchArray + output: the scratch is dropped, the output kept" $ do
        (arr, status) <- hsWriteBuffers
        IA.toList arr @?= [CInt 10, CInt 20, CInt 30]
        status        @?= 0

    , testCase "fixed: a constant argument is supplied but not exposed" $
        hsFixed 5 >>= (@?= 105)

    , testCase "nullConst: a fixed NULL const pointer reaches C as NULL" $
        hsIsNull >>= (@?= 1)
    ]

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

hsScratch :: IO Int
hsScratch = toHighLevel
    (scratch (allocaBytes 16) $ resultPure fromIntegral) writeScratch

hsWriteBuffers :: IO (IA.IncompleteArray CInt, Int)
hsWriteBuffers = toHighLevel
    ( scratchArray @CChar 16
    $ output intArrOut
    $ resultPure fromIntegral
    ) writeBuffers
  where
    intArrOut :: Unmarshaller (Ptr CInt) (IA.IncompleteArray CInt)
    intArrOut = peekIncompleteArrayOut 3

-- | 'fixed' pins the first C argument to @100@; the wrapper exposes only the second.
hsFixed :: Int -> IO Int
hsFixed = toHighLevel
    ( fixed (100 :: CInt)
    $ input (scalar (fromIntegral :: Int -> CInt))
    $ resultPure fromIntegral
    ) cAdd

hsIsNull :: IO Int
hsIsNull = toHighLevel (fixed nullConst $ resultPure fromIntegral) reportNull
