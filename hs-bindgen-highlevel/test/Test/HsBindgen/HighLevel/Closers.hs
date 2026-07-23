-- | Closers and the trailing-unit pass: 'resultPure' \/ 'resultIO' \/
-- 'discardResult' converting the C return, 'assertPure' exposing a
-- deterministic wrapper as a pure function, and 'dropTrailingUnit' collapsing
-- the @()@ a void closer leaves beside outputs.
module Test.HsBindgen.HighLevel.Closers (tests) where

import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.HighLevel (assertPure, discardResult, dropTrailingUnit, input,
                            output, resultIO, resultPure, toHighLevel)
import HsBindgen.HighLevel.Marshaller (scalar)

import Test.HsBindgen.HighLevel.Util (cAdd, peekIntOut, returnsStatus)

{-------------------------------------------------------------------------------
  Fixtures
-------------------------------------------------------------------------------}

oneOut :: Ptr CInt -> IO CInt
oneOut p = poke p (CInt 7) >> pure 0

twoOut :: Ptr CInt -> Ptr CInt -> IO CInt
twoOut p q = poke p (CInt 3) >> poke q (CInt 4) >> pure 0

-- | Writes @n + 1@ to the out-parameter and returns @0@.
argThenOut :: CInt -> Ptr CInt -> IO CInt
argThenOut (CInt n) p = poke p (CInt (n + 1)) >> pure 0

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "closers and dropTrailingUnit"
    [ testGroup "closers"
        [ testCase "resultPure: convert the C return purely" $
            hsResultPure 4 >>= (@?= 12)
        , testCase "resultIO: convert the C return in IO" $
            hsResultIO 21 >>= (@?= 42)
        , testCase "discardResult: leaves a trailing () beside the output" $
            hsOneOut >>= (@?= (7, ()))
        , testCase "assertPure: an IO wrapper becomes a pure function" $
            pureAdd 3 4 @?= 7
        ]

    , testGroup "dropTrailingUnit"
        [ testCase "one output plus a void closer collapses to the value" $
            hsOneOutDropped >>= (@?= 7)
        , testCase "two outputs plus a void closer drop the trailing ()" $
            hsTwoOutDropped >>= (@?= (3, 4))
        , testCase "threads under a remaining wrapper argument" $
            hsArgThenOutDropped 41 >>= (@?= 42)
        ]
    ]

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

hsResultPure :: CInt -> IO Int
hsResultPure = toHighLevel
    (input (scalar id) $ resultPure (\(CInt n) -> fromIntegral n * 3)) returnsStatus

hsResultIO :: CInt -> IO Int
hsResultIO = toHighLevel
    (input (scalar id) $ resultIO (\(CInt n) -> pure (fromIntegral n * 2))) returnsStatus

hsOneOut :: IO (Int, ())
hsOneOut = toHighLevel (output peekIntOut $ discardResult) oneOut

pureAdd :: Int -> Int -> Int
pureAdd = assertPure
    (toHighLevel
      ( input (scalar (fromIntegral :: Int -> CInt))
      $ input (scalar (fromIntegral :: Int -> CInt))
      $ resultPure (\(CInt n) -> fromIntegral n)
      ) cAdd)

hsOneOutDropped :: IO Int
hsOneOutDropped =
    toHighLevel (dropTrailingUnit $ output peekIntOut $ discardResult) oneOut

hsTwoOutDropped :: IO (Int, Int)
hsTwoOutDropped = toHighLevel
    ( dropTrailingUnit
    $ output peekIntOut
    $ output peekIntOut
    $ discardResult
    ) twoOut

hsArgThenOutDropped :: Int -> IO Int
hsArgThenOutDropped = toHighLevel
    ( dropTrailingUnit
    $ input (scalar (CInt . fromIntegral))
    $ output peekIntOut
    $ discardResult
    ) argThenOut
