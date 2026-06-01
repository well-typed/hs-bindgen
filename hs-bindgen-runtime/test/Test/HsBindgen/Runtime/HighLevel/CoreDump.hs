-- | A 'ToHighLevel' wrapper compiled at @-O2@ with its optimised Core dumped to a
-- file, so the generated code can be read by hand. The combinators carry
-- @INLINE@ pragmas, so at @-O2@ they fuse away: the Core for 'coreExample' should
-- be raw 'Foreign.Marshal.Alloc.alloca' \/ 'Foreign.Storable.poke' \/
-- 'Foreign.Storable.peek' with no 'ToHighLevel', 'InMarshaller', or
-- 'OutMarshaller' constructors left.
--
-- The dump lands next to the object file; read the 'coreExample' binding (kept
-- standalone by its @NOINLINE@):
--
-- > dist-newstyle/build/<arch>/ghc-<ver>/hs-bindgen-runtime-<ver>/t/test-hs-bindgen-runtime/build/test-hs-bindgen-runtime/test-hs-bindgen-runtime-tmp/test/Test/HsBindgen/Runtime/HighLevel/CoreDump.dump-simpl
--
-- The rest of the suite builds at @-O0@; only this module forces @-O2@, so it is
-- the one place the inlining is worth reading.
{-# OPTIONS_GHC -O2 -ddump-simpl -ddump-to-file
                -dsuppress-coercions -dsuppress-idinfo
                -dsuppress-uniques -dsuppress-module-prefixes #-}

-- 'coreExample' is exported so it survives as a named top-level binding in the
-- dump (an unexported, used-once binding is folded into the test instead).
module Test.HsBindgen.Runtime.HighLevel.CoreDump (coreExample, tests) where

import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.Runtime.HighLevel.ToHighLevel (OutMarshaller, input, output,
                                                peekOutPure, pureIn, resultPure,
                                                toHighLevel)

-- | Two out-parameters sandwich one scalar input, with the C status kept as the
-- last tuple component. Same shape as the interleaved smoke fixture; this copy
-- exists so its Core is dumped at @-O2@.
coreExample :: Int -> IO (Int, Int, Int)
{-# NOINLINE coreExample #-}
coreExample = toHighLevel
  ( output peekIntOut
  $ input  (pureIn (CInt . fromIntegral))
  $ output peekIntOut
  $ resultPure fromIntegral
  ) interleaved
  where
    peekIntOut :: OutMarshaller (Ptr CInt) Int
    peekIntOut = peekOutPure (\(CInt n) -> fromIntegral n)

-- | Stand-in low-level callable: write both out-parameters, return status 0.
interleaved :: Ptr CInt -> CInt -> Ptr CInt -> IO CInt
interleaved pOut1 (CInt n) pOut2 = do
  poke pOut1 (CInt n)
  poke pOut2 (CInt (n * 2))
  pure 0

tests :: TestTree
tests = testGroup "HighLevel.CoreDump"
  [ testCase "interleaved out-in-out wrapper (Core dumped at -O2)" $ do
      (a, b, status) <- coreExample 7
      a      @?= 7
      b      @?= 14
      status @?= 0
  ]
