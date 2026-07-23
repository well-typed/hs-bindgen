-- | Closers as assemblers: 'resultPure' \/ 'resultIO' \/ 'discardResult'
-- converting the C return, the assembler receiving each output ahead of it and
-- deciding the result shape, and 'toHighLevelPure' exposing a deterministic wrapper
-- as a pure function.
module Test.HsBindgen.HighLevel.Closers (tests) where

import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.HighLevel (discardResult, input, output, resultIO, resultPure,
                            toHighLevel, toHighLevelPure)
import HsBindgen.HighLevel.Auto (auto)
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

-- | A result type that is not a tuple, to pin that the assembler is free.
data Pair = Pair Int Int
  deriving stock (Eq, Show)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "closers"
    [ testGroup "converting the C return"
        [ testCase "resultPure: convert the C return purely" $
            hsResultPure 4 >>= (@?= 12)
        , testCase "resultIO: convert the C return in IO" $
            hsResultIO 21 >>= (@?= 42)
        , testCase "discardResult: no outputs, C return dropped" $
            hsDiscard 4 >>= (@?= ())
        , testCase "toHighLevelPure: the wrapper is a pure function" $
            pureAdd 3 4 @?= 7
        , testCase "toHighLevelPure: auto reads the types off the pure signature" $
            pureAddAuto 3 4 @?= 7
        , testCase "toHighLevelPure: no arguments at all" $
            pureStatus @?= 0
        ]

    , testGroup "the assembler decides the result shape"
        [ testCase "one output, C return ignored, result is the value alone" $
            hsOneOutOnly >>= (@?= 7)
        , testCase "two outputs, C return ignored, result is a flat pair" $
            hsTwoOutOnly >>= (@?= (3, 4))
        , testCase "the tuple constructor gives outputs plus the C return" $
            hsTwoOutAndStatus >>= (@?= (3, 4, 0))
        , testCase "assembles under a remaining wrapper argument" $
            hsArgThenOut 41 >>= (@?= 42)
        , testCase "the assembler may build any type, not just a tuple" $
            hsAsRecord >>= (@?= Pair 3 4)
        ]
    ]

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

hsResultPure :: CInt -> IO Int
hsResultPure = toHighLevel returnsStatus
             $ input (scalar id)
             $ resultPure (\(CInt n) -> fromIntegral n * 3)

hsResultIO :: CInt -> IO Int
hsResultIO = toHighLevel returnsStatus
           $ input (scalar id)
           $ resultIO (\(CInt n) -> pure (fromIntegral n * 2))

hsDiscard :: CInt -> IO ()
hsDiscard = toHighLevel returnsStatus
          $ input (scalar id)
          $ discardResult

pureAdd :: Int -> Int -> Int
pureAdd = toHighLevelPure cAdd
        $ input (scalar (fromIntegral :: Int -> CInt))
        $ input (scalar (fromIntegral :: Int -> CInt))
        $ resultPure (\(CInt n) -> fromIntegral n)

-- | 'auto' reads the argument types off the signature, and here the signature is the
-- pure one, so the high-level type has to be computed from it rather than inferred out
-- of the spec.
pureAddAuto :: Int -> Int -> Int
pureAddAuto = toHighLevelPure cAdd auto

-- | The base case of the walk, with no arguments to recurse under.
pureStatus :: Int
pureStatus = toHighLevelPure (returnsStatus 0) auto

-- | Keeping the output and discarding the C return needs no dedicated closer: the
-- assembler ignores the return value with a wildcard.
hsOneOutOnly :: IO Int
hsOneOutOnly = toHighLevel oneOut
             $ output peekIntOut
             $ resultPure (\v _ -> v)

hsTwoOutOnly :: IO (Int, Int)
hsTwoOutOnly = toHighLevel twoOut
             $ output peekIntOut
             $ output peekIntOut
             $ resultPure (\a b _ -> (a, b))

-- | The flat tuple of outputs plus the C return is the tuple constructor.
hsTwoOutAndStatus :: IO (Int, Int, Int)
hsTwoOutAndStatus = toHighLevel twoOut
                  $ output peekIntOut
                  $ output peekIntOut
                  $ resultPure (\a b (CInt n) -> (a, b, fromIntegral n))

hsArgThenOut :: Int -> IO Int
hsArgThenOut = toHighLevel argThenOut
             $ input (scalar (CInt . fromIntegral))
             $ output peekIntOut
             $ resultPure (\v _ -> v)

hsAsRecord :: IO Pair
hsAsRecord = toHighLevel twoOut
           $ output peekIntOut
           $ output peekIntOut
           $ resultPure (\a b _ -> Pair a b)
