{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Smoke tests for the value-level 'Refine' DSL.
module Test.HsBindgen.Runtime.HighLevel.RefineSmoke (tests) where

import Control.Exception (Exception, try)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.Runtime.HighLevel.Refine (OutMarshaller, discardRes, funPtrIn,
                                           input, nullableIn, output,
                                           peekCStringOut,
                                           peekIncompleteArrayOut, peekOut,
                                           pureIn, pureRes, result, scratchOut,
                                           throwOn, throwOnNonZero, toHighLevel,
                                           useAsByteStringLenIn, withCStringIn)
import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

{-------------------------------------------------------------------------------
  libc-backed wrappers

  Foreign imports use 'Ptr CChar': FFI marshalling of 'PtrConst CChar'
  requires the data constructor of t'ConstPtr' in scope, which only exists
  on base-4.18+. Haskell-side wrappers re-expose the @const@-correct type.
-------------------------------------------------------------------------------}

foreign import ccall unsafe "string.h strlen"
  c_strlen_raw :: Ptr CChar -> IO CSize

foreign import ccall unsafe "string.h strncmp"
  c_strncmp_raw :: Ptr CChar -> Ptr CChar -> CSize -> IO CInt

c_strlen :: PtrConst CChar -> IO CSize
c_strlen p = c_strlen_raw (PtrConst.unsafeToPtr p)

c_strncmp :: PtrConst CChar -> PtrConst CChar -> CSize -> IO CInt
c_strncmp p1 p2 = c_strncmp_raw (PtrConst.unsafeToPtr p1) (PtrConst.unsafeToPtr p2)

hsStrlen :: String -> IO Int
hsStrlen = toHighLevel ( input  withCStringIn
                       $ result (pureRes fromIntegral)
                       ) c_strlen

-- | Mixed slot widths: 'String' (1 C arg) then 'ByteString' (2 C args).
hsStrncmpMixed :: String -> ByteString -> IO Int
hsStrncmpMixed = toHighLevel ( input  withCStringIn
                             $ input  useAsByteStringLenIn
                             $ result (pureRes fromIntegral)
                             ) c_strncmp

{-------------------------------------------------------------------------------
  Two-output fixture
-------------------------------------------------------------------------------}

sampleOut :: Ptr CInt -> Ptr CInt -> IO CInt
sampleOut p p' = do
  poke p  42
  poke p' 42
  pure 0

peekIntOut :: OutMarshaller (Ptr CInt) Int
peekIntOut = peekOut (\(CInt n) -> pure (fromIntegral n))

-- | Two outputs + 'discardRes' — result is @IO (a, (b, ()))@.
hsSampleOutDiscard :: IO (Int, Int)
hsSampleOutDiscard = do
  (a, (b, ())) <- toHighLevel ( output peekIntOut
                              $ output peekIntOut
                              $ discardRes
                              ) sampleOut
  pure (a, b)

{-------------------------------------------------------------------------------
  Interleaved out-in-out
-------------------------------------------------------------------------------}

interleaved :: Ptr CInt -> CInt -> Ptr CInt -> IO CInt
interleaved pOut1 (CInt n) pOut2 = do
  poke pOut1 (CInt n)
  poke pOut2 (CInt (n * 2))
  pure 0

hsInterleaved :: Int -> IO (Int, (Int, Int))
hsInterleaved = toHighLevel ( output peekIntOut
                            $ input  (pureIn (CInt . fromIntegral))
                            $ output peekIntOut
                            $ result (pureRes fromIntegral)
                            ) interleaved

{-------------------------------------------------------------------------------
  Nullable input: Maybe String
-------------------------------------------------------------------------------}

checkPresent :: PtrConst CChar -> IO CInt
checkPresent p
  | PtrConst.unsafeToPtr p == nullPtr = pure 0
  | otherwise                         = pure 1

hsCheckPresent :: Maybe String -> IO Bool
hsCheckPresent = toHighLevel ( input  (nullableIn withCStringIn)
                             $ result (pureRes (/= 0))
                             ) checkPresent

{-------------------------------------------------------------------------------
  Callback input: funPtrIn
-------------------------------------------------------------------------------}

type Callback = CInt -> IO ()

foreign import ccall "dynamic"
  callCallback :: FunPtr Callback -> Callback

callOnce :: FunPtr Callback -> IO ()
callOnce fp = callCallback fp 42

hsCallOnce :: Callback -> IO ()
hsCallOnce = toHighLevel ( input  funPtrIn
                         $ discardRes
                         ) callOnce

{-------------------------------------------------------------------------------
  Fixed-size string output: peekCStringOut
-------------------------------------------------------------------------------}

writeErrorString :: Ptr CChar -> IO CInt
writeErrorString buf = do
  pokeArray buf [c 'b', c 'o', c 'o', c 'm', CChar 0]
  pure 1
  where
    c = CChar . fromIntegral . fromEnum

hsWriteError :: IO (String, Int)
hsWriteError = toHighLevel
  ( output (peekCStringOut 32)
  $ result (pureRes fromIntegral)
  ) writeErrorString

{-------------------------------------------------------------------------------
  Scratch buffer + typed output
-------------------------------------------------------------------------------}

writeBuffers :: Ptr CChar -> Ptr CInt -> IO CInt
writeBuffers _tempBuf outBuf = do
  pokeArray outBuf [CInt 10, CInt 20, CInt 30]
  pure 0

hsWriteBuffers :: IO ((), (IA.IncompleteArray CInt, Int))
hsWriteBuffers = toHighLevel
  ( output charScratch
  $ output intArrOut
  $ result (pureRes fromIntegral)
  ) writeBuffers
  where
    charScratch :: OutMarshaller (Ptr CChar) ()
    charScratch = scratchOut 16
    intArrOut :: OutMarshaller (Ptr CInt) (IA.IncompleteArray CInt)
    intArrOut = peekIncompleteArrayOut 3

{-------------------------------------------------------------------------------
  Exception handling: throwOn and throwOnNonZero
-------------------------------------------------------------------------------}

returnsStatus :: CInt -> IO CInt
returnsStatus = pure

newtype SampleErr = SampleErr CInt
  deriving stock (Show)
instance Exception SampleErr

hsCheckStatus :: CInt -> IO ()
hsCheckStatus = toHighLevel
  ( input  (pureIn id)
  $ result (throwOnNonZero SampleErr)
  ) returnsStatus

-- | Negative status throws; non-negative flows through the inner
-- 'ResMarshaller'.
hsThrowOnNeg :: CInt -> IO Int
hsThrowOnNeg = toHighLevel
  ( input  (pureIn id)
  $ result (throwOn classify (pureRes fromIntegral))
  ) returnsStatus
  where
    classify c
      | c < 0     = Just (SampleErr c)
      | otherwise = Nothing

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HighLevel.RefineSmoke"
  [ testCase "strlen \"hello\"" $ do
      n <- hsStrlen "hello"
      n @?= 5

  , testCase "strncmp String + ByteString (equal prefix)" $ do
      r <- hsStrncmpMixed "hello world" "hello"
      r @?= 0

  , testCase "two outputs + discardRes peek both out-params, drop status" $ do
      (a, b) <- hsSampleOutDiscard
      a @?= 42
      b @?= 42

  , testCase "interleaved out-in-out: outputs sandwich an input" $ do
      (a, (b, status)) <- hsInterleaved 7
      a      @?= 7
      b      @?= 14
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

  , testCase "peekCStringOut: peek a fixed-cap NUL-terminated string buffer" $ do
      (s, status) <- hsWriteError
      s      @?= "boom"
      status @?= 1

  , testCase "scratchOut + peekIncompleteArrayOut: scratch + real output" $ do
      ((), (arr, status)) <- hsWriteBuffers
      IA.toList arr @?= [CInt 10, CInt 20, CInt 30]
      status        @?= 0

  , testCase "throwOnNonZero: status 0 returns ()" $ do
      hsCheckStatus 0

  , testCase "throwOnNonZero: status /= 0 throws" $ do
      e <- try (hsCheckStatus 42) :: IO (Either SampleErr ())
      case e of
        Left (SampleErr n) -> n @?= 42
        Right ()           -> fail "expected exception"

  , testCase "throwOn: classifier returns Nothing -> inner marshaller fires" $ do
      n <- hsThrowOnNeg 7
      n @?= 7

  , testCase "throwOn: classifier returns Just e -> throws" $ do
      e <- try (hsThrowOnNeg (-1)) :: IO (Either SampleErr Int)
      case e of
        Left (SampleErr n) -> n @?= (-1)
        Right _            -> fail "expected exception"
  ]
