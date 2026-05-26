{-# LANGUAGE OverloadedStrings #-}

-- | Smoke tests for the value-level 'Spec' DSL.
--
-- Exercises the GADT, the 'toHighLevel' runner, and the standard marshallers
-- against libc and a small Haskell-side fixture with two out-parameters.
module Test.HsBindgen.Runtime.HighLevel.SpecSmoke (tests) where

import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.Runtime.HighLevel.Spec (OutMarshaller, discardRes, funPtrIn,
                                         input, nullableIn, output, peekOut,
                                         pureIn, pureRes, result, toHighLevel,
                                         useAsByteStringLenIn, withCStringIn)
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

{-------------------------------------------------------------------------------
  Foreign imports (against libc)
-------------------------------------------------------------------------------}

foreign import ccall unsafe "string.h strlen"
  c_strlen :: PtrConst CChar -> IO CSize

foreign import ccall unsafe "string.h strncmp"
  c_strncmp :: PtrConst CChar -> PtrConst CChar -> CSize -> IO CInt

{-------------------------------------------------------------------------------
  Wrappers built via the Spec DSL
-------------------------------------------------------------------------------}

-- | Single input, simple result. Demonstrates the minimal pipeline.
hsStrlen :: String -> IO Int
hsStrlen = toHighLevel ( input  withCStringIn
                       $ result (pureRes fromIntegral)
                       ) c_strlen

-- | Mixed slot widths in one signature: a 'String' (1 C arg) followed by a
-- 'ByteString' (2 C args via the @&@ pattern), then a simple result.
-- Tests that the 'ThreadIn' constraint on 'In' threads the @String@ bracket
-- through the @ByteString@'s two slots correctly.
hsStrncmpMixed :: String -> ByteString -> IO Int
hsStrncmpMixed = toHighLevel ( input  withCStringIn
                             $ input  useAsByteStringLenIn
                             $ result (pureRes fromIntegral)
                             ) c_strncmp

{-------------------------------------------------------------------------------
  Declarative-output fixture (no libc — a small Haskell-side C-like function)
-------------------------------------------------------------------------------}

-- | Writes 42 into both out parameters, returns 0 as status.
sampleOut :: Ptr CInt -> Ptr CInt -> IO CInt
sampleOut p p' = do
  poke p  42
  poke p' 42
  pure 0

-- | Reusable peek for a CInt out parameter as an Int.
peekIntOut :: OutMarshaller Int (Ptr CInt)
peekIntOut = peekOut (\(CInt n) -> pure (fromIntegral n))

-- | Two declarative outputs + a status result.
--
-- The result is a /right-associative nested tuple/ because each 'output'
-- prepends one component to the inner @IO@'s payload. With two outputs +
-- one result this is @IO (a, (b, c))@ — readable but worth knowing.
hsSampleOut :: IO (Int, (Int, Int))
hsSampleOut = toHighLevel ( output peekIntOut
                          $ output peekIntOut
                          $ result (pureRes fromIntegral)
                          ) sampleOut

-- | Two outputs + 'discardRes' produces @IO (a, (b, ()))@. The trailing
-- unit is awkward at the call site, so we flatten with a pattern match.
hsSampleOutDiscard :: IO (Int, Int)
hsSampleOutDiscard = do
  (a, (b, ())) <- toHighLevel ( output peekIntOut
                              $ output peekIntOut
                              $ discardRes
                              ) sampleOut
  pure (a, b)

{-------------------------------------------------------------------------------
  Interleaved input/output fixture: out-in-out
-------------------------------------------------------------------------------}

-- | C signature with an input sandwiched between two outputs:
-- writes @n@ into the first slot, leaves @n*2@ in the second slot.
interleaved :: Ptr CInt -> CInt -> Ptr CInt -> IO CInt
interleaved pOut1 (CInt n) pOut2 = do
  poke pOut1 (CInt n)
  poke pOut2 (CInt (n * 2))
  pure 0

-- | Same C signature lifted to a Haskell wrapper with the input
-- sandwiched between the two output positions in the spec. The
-- result type is @Int -> IO (Int, (Int, Int))@: the @Int@ on the
-- left of the arrow is the Hs input; the nested tuple holds the
-- two outputs and the status, in C-arg order.
hsInterleaved :: Int -> IO (Int, (Int, Int))
hsInterleaved = toHighLevel ( output peekIntOut                     -- 1st C arg: out
                            $ input  (pureIn (CInt . fromIntegral)) -- 2nd C arg: in
                            $ output peekIntOut                     -- 3rd C arg: out
                            $ result (pureRes fromIntegral)         -- C return: status
                            ) interleaved

{-------------------------------------------------------------------------------
  Nullable-input fixture: Maybe String via `nullableIn`
-------------------------------------------------------------------------------}

-- | Returns 0 if the pointer is NULL, 1 otherwise.
checkPresent :: PtrConst CChar -> IO CInt
checkPresent p
  | PtrConst.unsafeToPtr p == nullPtr = pure 0
  | otherwise                         = pure 1

-- | Lifts 'checkPresent' to accept @Maybe String@ via 'nullableIn':
-- 'Nothing' marshals to the NULL pointer, 'Just' delegates to 'withCStringIn'.
hsCheckPresent :: Maybe String -> IO Bool
hsCheckPresent = toHighLevel ( input  (nullableIn withCStringIn)
                             $ result (pureRes (/= 0))
                             ) checkPresent

{-------------------------------------------------------------------------------
  Callback fixture: in-call invocation via `funPtrIn`
-------------------------------------------------------------------------------}

-- | Callback shape used by the fixture below. The TH-generated
-- 'HsBindgen.Runtime.Internal.FunPtr.ToFunPtr' instance for @CInt -> IO ()@
-- is what makes 'funPtrIn' work for this signature.
type Callback = CInt -> IO ()

-- | Round-trip a 'FunPtr' back into a callable function.
foreign import ccall "dynamic"
  callCallback :: FunPtr Callback -> Callback

-- | A Haskell-side fixture: invokes the callback once with @42@,
-- then returns. The callback is consumed /during/ the call, which is
-- exactly the lifetime 'funPtrIn' (bracketed via 'withFunPtr')
-- supports.
callOnce :: FunPtr Callback -> IO ()
callOnce fp = callCallback fp 42

-- | Lifts 'callOnce' to accept a Haskell-level @Callback@.
hsCallOnce :: Callback -> IO ()
hsCallOnce = toHighLevel ( input  funPtrIn
                         $ discardRes
                         ) callOnce

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HighLevel.SpecSmoke"
  [ testCase "strlen \"hello\"" $ do
      n <- hsStrlen "hello"
      n @?= 5

  , testCase "strlen \"\"" $ do
      n <- hsStrlen ""
      n @?= 0

  , testCase "strncmp String + ByteString (equal prefix)" $ do
      r <- hsStrncmpMixed "hello world" "hello"
      r @?= 0

  , testCase "strncmp String + ByteString (different)" $ do
      r <- hsStrncmpMixed "hello" "abcde"
      (r /= 0) @?= True

  , testCase "two outputs + result peek both out-params and the status" $ do
      (a, (b, status)) <- hsSampleOut
      a      @?= 42
      b      @?= 42
      status @?= 0

  , testCase "two outputs + discardRes peek both out-params, drop status" $ do
      (a, b) <- hsSampleOutDiscard
      a @?= 42
      b @?= 42

  , testCase "interleaved out-in-out: outputs sandwich an input" $ do
      (a, (b, status)) <- hsInterleaved 7
      a      @?= 7        -- first  output: n
      b      @?= 14       -- second output: n * 2
      status @?= 0

  , testCase "nullableIn: Nothing maps to a NULL pointer" $ do
      r <- hsCheckPresent Nothing
      r @?= False

  , testCase "nullableIn: Just \"x\" maps to a non-NULL pointer" $ do
      r <- hsCheckPresent (Just "x")
      r @?= True

  , testCase "funPtrIn: callback invoked during the call" $ do
      ref <- newIORef 0
      hsCallOnce $ \n -> writeIORef ref (fromIntegral n :: Int)
      v <- readIORef ref
      v @?= 42
  ]
