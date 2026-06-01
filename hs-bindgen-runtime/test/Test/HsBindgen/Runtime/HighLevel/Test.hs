{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Tests for the 'ToHighLevel' wrapper combinators.
module Test.HsBindgen.Runtime.HighLevel.Test (tests) where

import Control.Exception (Exception, try)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.Runtime.HighLevel.ToHighLevel (OutMarshaller, discardRes,
                                                input, optionalIn, output,
                                                peekOutPure, pureIn, resultPure,
                                                scratchArray, throwOn,
                                                throwOnNonZero, throwOnOut,
                                                toHighLevel)
import HsBindgen.Runtime.HighLevel.ToHighLevel.Defaults (DefInArrow,
                                                         DefaultIn (..),
                                                         defaultRes)
import HsBindgen.Runtime.HighLevel.ToHighLevel.Marshallers (funPtrIn,
                                                            peekCStringOut,
                                                            peekIncompleteArrayOut,
                                                            useAsByteStringLenIn,
                                                            withCStringIn)
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

-- | Mixed @cArgs@ widths: 'String' (1 C arg) then 'ByteString' (2 C args).
hsStrncmpMixed :: String -> ByteString -> IO Int
hsStrncmpMixed = toHighLevel ( input  withCStringIn
                             $ input  useAsByteStringLenIn
                             $ resultPure fromIntegral
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
peekIntOut = peekOutPure (\(CInt n) -> fromIntegral n)

-- | Two outputs + 'discardRes': flat @IO (a, b)@, the discarded result leaving
-- no trailing @()@.
hsSampleOutDiscard :: IO (Int, Int)
hsSampleOutDiscard = toHighLevel ( output peekIntOut
                                 $ output peekIntOut
                                 $ discardRes
                                 ) sampleOut

{-------------------------------------------------------------------------------
  Six-output fixture: exercises the widest flat-tuple builder (a sixth output
  fills the 6-tuple; a seventh is a 'TooManyOutputs' compile error).
-------------------------------------------------------------------------------}

sixOut :: Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt
       -> IO CInt
sixOut p1 p2 p3 p4 p5 p6 = do
  poke p1 1; poke p2 2; poke p3 3; poke p4 4; poke p5 5; poke p6 6
  pure 0

hsSixOut :: IO (Int, Int, Int, Int, Int, Int)
hsSixOut = toHighLevel ( output peekIntOut $ output peekIntOut $ output peekIntOut
                       $ output peekIntOut $ output peekIntOut $ output peekIntOut
                       $ discardRes
                       ) sixOut

{-------------------------------------------------------------------------------
  Interleaved out-in-out
-------------------------------------------------------------------------------}

interleaved :: Ptr CInt -> CInt -> Ptr CInt -> IO CInt
interleaved pOut1 (CInt n) pOut2 = do
  poke pOut1 (CInt n)
  poke pOut2 (CInt (n * 2))
  pure 0

hsInterleaved :: Int -> IO (Int, Int, Int)
hsInterleaved = toHighLevel ( output peekIntOut
                            $ input  (pureIn (CInt . fromIntegral))
                            $ output peekIntOut
                            $ resultPure fromIntegral
                            ) interleaved

-- | All-default chain: 'String' input and 'CSize' -> 'Int' result, no annotations.
hsStrlenAuto :: String -> IO Int
hsStrlenAuto = toHighLevel (input defaultIn $ defaultRes) c_strlen

-- | All-default with /two/ scalar inputs (each @Int -> CInt@) and an @Int@ result.
hsAddAuto :: Int -> Int -> IO Int
hsAddAuto = toHighLevel (input defaultIn $ input defaultIn $ defaultRes) cAdd
  where
    cAdd :: CInt -> CInt -> IO CInt
    cAdd a b = pure (a + b)

-- | All-default where one Haskell argument fills /two/ C arguments: a 'ByteString'
-- becomes the @(const char *, size_t)@ pair, exercising a default that consumes
-- two C arguments from one Haskell value.
hsBsLenAuto :: ByteString -> IO Int
hsBsLenAuto = toHighLevel (input defaultIn $ defaultRes) cBsLen
  where
    cBsLen :: PtrConst CChar -> CSize -> IO CInt
    cBsLen _ n = pure (fromIntegral n)

{-------------------------------------------------------------------------------
  User-extensible default: a custom type supplies its own DefInArrow row
-------------------------------------------------------------------------------}

-- | A binding-defined default for a custom type, exercising the /open/
-- 'DefInArrow' family: @'input' 'defaultIn'@ works for 'Handle' from the single
-- row defined here, with no change to the library.
newtype Handle = Handle CInt

instance DefaultIn Handle where
  type DefInArrow Handle lo = CInt -> lo
  defaultIn = pureIn (\(Handle h) -> h)

hsHandleAuto :: Handle -> IO Int
hsHandleAuto = toHighLevel (input defaultIn $ defaultRes) cHandle
  where
    cHandle :: CInt -> IO CInt
    cHandle h = pure (h + 1)

{-------------------------------------------------------------------------------
  Optional inputs via 'optionalIn': one C arg (Maybe String) and N C args
  (Maybe ByteString), both with the single gap-filler combinator.
-------------------------------------------------------------------------------}

checkPresent :: PtrConst CChar -> IO CInt
checkPresent p
  | PtrConst.unsafeToPtr p == nullPtr = pure 0
  | otherwise                         = pure 1

nullCharPtr :: PtrConst CChar
nullCharPtr = PtrConst.unsafeFromPtr nullPtr

-- | One C argument: 'Nothing' supplies the null @const char *@ via the
-- @('$' nullCharPtr)@ filler.
hsCheckPresent :: Maybe String -> IO Bool
hsCheckPresent = toHighLevel ( input  (optionalIn ($ nullCharPtr) withCStringIn)
                             $ resultPure (/= 0)
                             ) checkPresent

-- | Two C arguments from one optional value: 'Nothing' fills /both/ the pointer
-- and the length, exercising 'optionalIn' on an N-to-1 marshaller with the same
-- combinator (the case a single-@c@ default could not express).
checkLen :: PtrConst CChar -> CSize -> IO CInt
checkLen p n
  | PtrConst.unsafeToPtr p == nullPtr = pure (-1)
  | otherwise                         = pure (fromIntegral n)

hsCheckLen :: Maybe ByteString -> IO Int
hsCheckLen = toHighLevel
  ( input  (optionalIn (\lo -> lo nullCharPtr 0) useAsByteStringLenIn)
  $ resultPure fromIntegral
  ) checkLen

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
  $ resultPure fromIntegral
  ) writeErrorString

{-------------------------------------------------------------------------------
  Output followed by a bracket input: peekCStringOut then withCStringIn
-------------------------------------------------------------------------------}

-- | Write the first byte of the input string into the output buffer.
firstCharInto :: Ptr CChar -> PtrConst CChar -> IO CInt
firstCharInto outBuf inStr = do
  c <- peek (PtrConst.unsafeToPtr inStr)
  pokeArray outBuf [c, CChar 0]
  pure 1

-- | An 'output' immediately followed by a bracketed input (the one
-- interleaving shape the other cases don't pin): the input bracket must fire
-- inside the output's allocation, around the call.
hsFirstChar :: String -> IO (String, Int)
hsFirstChar = toHighLevel
  ( output (peekCStringOut 8)
  $ input  withCStringIn
  $ resultPure fromIntegral
  ) firstCharInto

{-------------------------------------------------------------------------------
  Scratch buffer + typed output
-------------------------------------------------------------------------------}

writeBuffers :: Ptr CChar -> Ptr CInt -> IO CInt
writeBuffers _tempBuf outBuf = do
  pokeArray outBuf [CInt 10, CInt 20, CInt 30]
  pure 0

hsWriteBuffers :: IO (IA.IncompleteArray CInt, Int)
hsWriteBuffers = toHighLevel
  ( scratchArray @CChar 16
  $ output intArrOut
  $ resultPure fromIntegral
  ) writeBuffers
  where
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
  $ throwOnNonZero SampleErr
  ) returnsStatus

-- | Negative status throws; otherwise the status is /transformed/ to a 'Bool',
-- exercising @hs /= hs'@ in 'throwOn'.
hsThrowOnNegBool :: CInt -> IO Bool
hsThrowOnNegBool = toHighLevel
  ( input  (pureIn id)
  $ throwOn classify (resultPure fromIntegral)
  ) returnsStatus
  where
    classify :: Int -> Either SampleErr Bool
    classify n
      | n < 0     = Left (SampleErr (fromIntegral n))
      | otherwise = Right (n > 0)

{-------------------------------------------------------------------------------
  Error signalled by an output parameter: throwOnOut
-------------------------------------------------------------------------------}

-- | Writes its argument into the out-parameter and returns 0.
writeOutCode :: CInt -> Ptr CInt -> IO CInt
writeOutCode code outp = poke outp code >> pure 0

-- | The error lives in the out-parameter, not the return value, so the check
-- sits at the output via 'throwOnOut'. A negative code throws; otherwise the
-- value is kept and the (zero) return is discarded.
hsCheckOut :: CInt -> IO Int
hsCheckOut = toHighLevel
  ( input  (pureIn id)
  $ output (throwOnOut classify)
  $ discardRes
  ) writeOutCode
  where
    classify :: CInt -> Either SampleErr Int
    classify n
      | n < 0     = Left (SampleErr n)
      | otherwise = Right (fromIntegral n)

{-------------------------------------------------------------------------------
  Default input for IncompleteArray: input defaultIn marshals as a const pointer
-------------------------------------------------------------------------------}

-- | Reads the first element of a @const int *@ argument.
readFirstInt :: PtrConst CInt -> IO CInt
readFirstInt p = peek (PtrConst.unsafeToPtr p)

-- | 'input' 'defaultIn' for an 'IA.IncompleteArray', exercising the
-- @DefaultIn (IncompleteArray a)@ instance: the array marshals as a @const int *@
-- with no annotation.
hsReadFirstInt :: IA.IncompleteArray CInt -> IO Int
hsReadFirstInt = toHighLevel (input defaultIn $ defaultRes) readFirstInt

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HighLevel.ToHighLevel"
  [ testCase "strncmp String + ByteString (equal prefix)" $ do
      r <- hsStrncmpMixed "hello world" "hello"
      r @?= 0

  , testCase "two outputs + discardRes peek both out-params, drop status" $ do
      (a, b) <- hsSampleOutDiscard
      a @?= 42
      b @?= 42

  , testCase "six outputs build a flat 6-tuple (widest builder)" $ do
      r <- hsSixOut
      r @?= (1, 2, 3, 4, 5, 6)

  , testCase "interleaved out-in-out: outputs sandwich an input" $ do
      (a, b, status) <- hsInterleaved 7
      a      @?= 7
      b      @?= 14
      status @?= 0

  , testCase "all-default chain: String input + result" $ do
      n <- hsStrlenAuto "hello"
      n @?= 5

  , testCase "all-default chain: two scalar inputs + result" $ do
      n <- hsAddAuto 3 4
      n @?= 7

  , testCase "all-default chain: one ByteString fills a (ptr, len) C pair" $ do
      n <- hsBsLenAuto "hello"
      n @?= 5

  , testCase "user-defined default for a custom type" $ do
      n <- hsHandleAuto (Handle 41)
      n @?= 42

  , testCase "default input: IncompleteArray marshals as a const pointer" $ do
      n <- hsReadFirstInt (IA.fromList [CInt 7, CInt 8, CInt 9])
      n @?= 7

  , testCase "optionalIn (1 C arg): Nothing maps to a NULL pointer" $ do
      r <- hsCheckPresent Nothing
      r @?= False

  , testCase "optionalIn (1 C arg): Just \"x\" maps to a non-NULL pointer" $ do
      r <- hsCheckPresent (Just "x")
      r @?= True

  , testCase "optionalIn (2 C args): Nothing fills both pointer and length" $ do
      r <- hsCheckLen Nothing
      r @?= (-1)

  , testCase "optionalIn (2 C args): Just fills both from the ByteString" $ do
      r <- hsCheckLen (Just "hello")
      r @?= 5

  , testCase "funPtrIn: callback invoked during the call" $ do
      ref <- newIORef 0
      hsCallOnce $ \n -> writeIORef ref (fromIntegral n :: Int)
      v <- readIORef ref
      v @?= 42

  , testCase "peekCStringOut: peek a fixed-cap NUL-terminated string buffer" $ do
      (s, status) <- hsWriteError
      s      @?= "boom"
      status @?= 1

  , testCase "output then bracket input: input bracket nests inside the output" $ do
      (s, status) <- hsFirstChar "xyz"
      s      @?= "x"
      status @?= 1

  , testCase "scratch + peekIncompleteArrayOut: scratch adds nothing, real output kept" $ do
      (arr, status) <- hsWriteBuffers
      IA.toList arr @?= [CInt 10, CInt 20, CInt 30]
      status        @?= 0

  , testCase "throwOnNonZero: status 0 returns ()" $ do
      hsCheckStatus 0

  , testCase "throwOnNonZero: status /= 0 throws" $ do
      e <- try (hsCheckStatus 42) :: IO (Either SampleErr ())
      case e of
        Left (SampleErr n) -> n @?= 42
        Right ()           -> fail "expected exception"

  , testCase "throwOn: transforms the value (hs /= hs')" $ do
      b <- hsThrowOnNegBool 7
      b @?= True
      e <- try (hsThrowOnNegBool (-3)) :: IO (Either SampleErr Bool)
      case e of
        Left (SampleErr n) -> n @?= (-3)
        Right _            -> fail "expected exception"

  , testCase "throwOnOut: non-negative out-param value flows through" $ do
      n <- hsCheckOut 7
      n @?= 7

  , testCase "throwOnOut: negative out-param value throws" $ do
      e <- try (hsCheckOut (-2)) :: IO (Either SampleErr Int)
      case e of
        Left (SampleErr n) -> n @?= (-2)
        Right _            -> fail "expected exception"
  ]
