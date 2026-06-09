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

import HsBindgen.Runtime.HighLevel (discardResult, input, output, resultIO,
                                    resultPure, scratchArray, throwOn,
                                    throwOnNonZero, throwOnOut, toHighLevel)
import HsBindgen.Runtime.HighLevel.Defaults (DefInArrow, DefaultIn (..), auto,
                                             defaultOut, defaultRes)
import HsBindgen.Runtime.HighLevel.Marshaller (Unmarshaller, marshalOptional,
                                               scalar, unmarshalOutPure)
import HsBindgen.Runtime.HighLevel.Marshaller.Utils (funPtrIn, peekCStringOut,
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
  Shared scalar-output helper
-------------------------------------------------------------------------------}

peekIntOut :: Unmarshaller (Ptr CInt) Int
peekIntOut = unmarshalOutPure (\(CInt n) -> fromIntegral n)

{-------------------------------------------------------------------------------
  Six-output fixture: many outputs flatten into one wide tuple, with 'discardResult'
  leaving a trailing @()@ (the result tops out at eight components).
-------------------------------------------------------------------------------}

sixOut :: Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt
       -> IO CInt
sixOut p1 p2 p3 p4 p5 p6 = do
  poke p1 1; poke p2 2; poke p3 3; poke p4 4; poke p5 5; poke p6 6
  pure 0

hsSixOut :: IO (Int, Int, Int, Int, Int, Int, ())
hsSixOut = toHighLevel ( output peekIntOut $ output peekIntOut $ output peekIntOut
                       $ output peekIntOut $ output peekIntOut $ output peekIntOut
                       $ discardResult
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
                            $ input  (scalar (CInt . fromIntegral))
                            $ output peekIntOut
                            $ resultPure fromIntegral
                            ) interleaved

-- | All-default with /two/ scalar inputs (each @Int -> CInt@) and an @Int@ result.
hsAddAuto :: Int -> Int -> IO Int
hsAddAuto = toHighLevel (input defaultIn $ input defaultIn $ defaultRes) cAdd
  where
    cAdd :: CInt -> CInt -> IO CInt
    cAdd a b = pure (a + b)

-- | The same C function closed by 'auto', but the wrapper keeps every generated C
-- type: two identity @CInt@ inputs and an identity @CInt@ result, no hand-written
-- marshaller. Writing @IO CInt@ (not @IO Int@) selects the identity result default;
-- 'hsAddAuto' closes the same function to 'Int'. This exercises the result default
-- being chosen by the written signature, like an input or an output.
hsAddAutoRaw :: CInt -> CInt -> IO CInt
hsAddAutoRaw = toHighLevel auto cAddRaw
  where
    cAddRaw :: CInt -> CInt -> IO CInt
    cAddRaw a b = pure (a + b)

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
  defaultIn = scalar (\(Handle h) -> h)

hsHandleAuto :: Handle -> IO Int
hsHandleAuto = toHighLevel (input defaultIn $ defaultRes) cHandle
  where
    cHandle :: CInt -> IO CInt
    cHandle h = pure (h + 1)

{-------------------------------------------------------------------------------
  Optional inputs via 'marshalOptional': one C arg (Maybe String) and N C args
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
hsCheckPresent = toHighLevel ( input  (marshalOptional ($ nullCharPtr) withCStringIn)
                             $ resultPure (/= 0)
                             ) checkPresent

-- | Two C arguments from one optional value: 'Nothing' fills /both/ the pointer
-- and the length, exercising 'marshalOptional' on an N-to-1 marshaller with the same
-- combinator (the case a single-@c@ default could not express).
checkLen :: PtrConst CChar -> CSize -> IO CInt
checkLen p n
  | PtrConst.unsafeToPtr p == nullPtr = pure (-1)
  | otherwise                         = pure (fromIntegral n)

hsCheckLen :: Maybe ByteString -> IO Int
hsCheckLen = toHighLevel
  ( input  (marshalOptional (\lo -> lo nullCharPtr 0) useAsByteStringLenIn)
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
                         $ discardResult
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
    intArrOut :: Unmarshaller (Ptr CInt) (IA.IncompleteArray CInt)
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
  ( input  (scalar id)
  $ throwOnNonZero SampleErr
  ) returnsStatus

-- | Negative status throws; otherwise the status is /transformed/ to a 'Bool',
-- exercising @hs /= hs'@ in 'throwOn'.
hsThrowOnNegBool :: CInt -> IO Bool
hsThrowOnNegBool = toHighLevel
  ( input  (scalar id)
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
hsCheckOut :: CInt -> IO (Int, ())
hsCheckOut = toHighLevel
  ( input  (scalar id)
  $ output (throwOnOut classify)
  $ discardResult
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
  'auto': one combinator filling the mundane positions from the signature
-------------------------------------------------------------------------------}

-- | 'auto' as the whole spec: fills the 'String' input and the @CSize -> Int@
-- closer from the signature, with no combinators written at all.
hsStrlenAutoBare :: String -> IO Int
hsStrlenAutoBare = toHighLevel auto c_strlen

-- | 'auto' after an explicit 'output' (the headline shape): the 'output' is kept;
-- 'auto' fills the trailing scalar input and the closer.
outThenIn :: Ptr CInt -> CInt -> IO CInt
outThenIn pOut (CInt n) = poke pOut (CInt (n * 10)) >> pure (CInt n)

hsOutThenInAuto :: Int -> IO (Int, Int)
hsOutThenInAuto = toHighLevel (output peekIntOut $ auto) outThenIn

-- | Multi-C-argument 'auto': a 'ByteString' filling the @(const char *, size_t)@
-- pair is reached by 'auto', not only by explicit @'input' 'defaultIn'@.
cBsLenAuto :: PtrConst CChar -> CSize -> IO CInt
cBsLenAuto _ n = pure (fromIntegral n)

hsBsLenAutoBare :: ByteString -> IO Int
hsBsLenAutoBare = toHighLevel auto cBsLenAuto

{-------------------------------------------------------------------------------
  defaultOut and resultIO: the two closers not exercised above
-------------------------------------------------------------------------------}

-- | Writes 99 into the out-parameter and returns 7.
outAndStatus :: Ptr CInt -> IO CInt
outAndStatus p = poke p 99 >> pure 7

-- | 'output' 'defaultOut': the scalar out-parameter coerces @Ptr CInt -> Int@
-- through the default, with no explicit output marshaller.
hsDefaultOut :: IO (Int, Int)
hsDefaultOut = toHighLevel (output defaultOut $ defaultRes) outAndStatus

-- | 'resultIO': the C return value is converted in 'IO' (here, doubled).
hsResultIO :: CInt -> IO Int
hsResultIO = toHighLevel
  ( input    (scalar id)
  $ resultIO (\(CInt n) -> pure (fromIntegral n * 2))
  ) returnsStatus

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HighLevel.ToHighLevel"
  [ testCase "strncmp String + ByteString (equal prefix)" $ do
      r <- hsStrncmpMixed "hello world" "hello"
      r @?= 0

  , testCase "six outputs + discardResult build a wide flat tuple with trailing ()" $ do
      r <- hsSixOut
      r @?= (1, 2, 3, 4, 5, 6, ())

  , testCase "interleaved out-in-out: outputs sandwich an input" $ do
      (a, b, status) <- hsInterleaved 7
      a      @?= 7
      b      @?= 14
      status @?= 0

  , testCase "auto (bare): fills the String input and the CSize result" $ do
      n <- hsStrlenAutoBare "hello"
      n @?= 5

  , testCase "auto after output: output kept, auto fills the input + closer" $ do
      (out, status) <- hsOutThenInAuto 5
      out    @?= 50
      status @?= 5

  , testCase "auto (multi-C-arg): a ByteString fills (ptr, len) via auto" $ do
      n <- hsBsLenAutoBare "hello"
      n @?= 5

  , testCase "all-default chain: two scalar inputs + result" $ do
      n <- hsAddAuto 3 4
      n @?= 7

  , testCase "auto over an all-raw-C wrapper: identity inputs and identity result" $ do
      n <- hsAddAutoRaw 3 4
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

  , testCase "marshalOptional (1 C arg): Nothing maps to a NULL pointer" $ do
      r <- hsCheckPresent Nothing
      r @?= False

  , testCase "marshalOptional (1 C arg): Just \"x\" maps to a non-NULL pointer" $ do
      r <- hsCheckPresent (Just "x")
      r @?= True

  , testCase "marshalOptional (2 C args): Nothing fills both pointer and length" $ do
      r <- hsCheckLen Nothing
      r @?= (-1)

  , testCase "marshalOptional (2 C args): Just fills both from the ByteString" $ do
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
      (n, ()) <- hsCheckOut 7
      n @?= 7

  , testCase "throwOnOut: negative out-param value throws" $ do
      e <- try (hsCheckOut (-2)) :: IO (Either SampleErr (Int, ()))
      case e of
        Left (SampleErr n) -> n @?= (-2)
        Right _            -> fail "expected exception"

  , testCase "output defaultOut: scalar out-param coerces via the default" $ do
      (out, status) <- hsDefaultOut
      out    @?= 99
      status @?= 7

  , testCase "resultIO: the C return value is converted in IO" $ do
      n <- hsResultIO 21
      n @?= 42
  ]
