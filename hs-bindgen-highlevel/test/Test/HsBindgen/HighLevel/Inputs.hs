{-# LANGUAGE OverloadedStrings #-}

-- | Supplying wrapper arguments: 'input' and its arity-pinned forms
-- 'input1' \/ 'input2' \/ 'input3', the ready-made argument marshallers
-- ('withCStringIn' and friends), 'marshalOptional', and the const \/ mutable
-- pointer retagging.
module Test.HsBindgen.HighLevel.Inputs (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CInt, CSize)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (ioProperty, testProperty, (===))

import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import HsBindgen.HighLevel (discardResult, input, input1, input2, input3,
                            resultPure, toHighLevel)
import HsBindgen.HighLevel.Marshaller (Marshal, asConstArg, asMutableArg, at,
                                       marshalOptional, scalar, (>>>))
import HsBindgen.HighLevel.Marshaller.Utils (constByteStringLenIn, funPtrIn,
                                             funPtrInAs, nullConst,
                                             unsafeByteStringLenIn,
                                             useAsByteStringLenIn,
                                             withCStringArrayIn, withCStringIn,
                                             withCStringMutIn)

import Test.HsBindgen.HighLevel.Util (Callback, cBsLen, c_strlen, c_strlen_raw,
                                      c_strncmp, callOnce, checkPresent,
                                      firstArgChar, firstByteConst, reportLen)

{-------------------------------------------------------------------------------
  Fixtures (local; the shared C stand-ins live in Util)
-------------------------------------------------------------------------------}

addThree :: CInt -> CInt -> CInt -> IO CInt
addThree a b c = pure (a + b + c)

-- | One Haskell value filling three C arguments, for 'input3'.
tripleIn :: Marshal (Int, Int, Int) (CInt -> CInt -> CInt -> lo') lo'
tripleIn = at (\(a, _, _) -> a) (scalar fromIntegral)
       >>> at (\(_, b, _) -> b) (scalar fromIntegral)
       >>> at (\(_, _, c) -> c) (scalar fromIntegral)

-- | Returns the length for a non-NULL @(const char *, size_t)@, @-1@ for NULL.
checkLen :: PtrConst CChar -> CSize -> IO CInt
checkLen p n
  | PtrConst.unsafeToPtr p == nullPtr = pure (-1)
  | otherwise                         = pure (fromIntegral n)

firstByteMut :: Ptr CChar -> IO CInt
firstByteMut p = fromIntegral <$> peek p

-- | Copy @len@ bytes back out of a @(const char *, size_t)@ buffer during the
-- call, so a zero-copy input can be checked by round-tripping the bytes
-- (embedded NULs included, which a NUL-terminated path would truncate).
echoBytesLen :: PtrConst CChar -> CSize -> IO ByteString
echoBytesLen p n = BS.packCStringLen (PtrConst.unsafeToPtr p, fromIntegral n)

-- | A domain newtype over the covered @CInt@ callback argument.
-- @LabelledInt -> IO ()@ is 'Data.Coerce.Coercible' to 'Callback' but not itself
-- covered by a @ToFunPtr@ instance, so 'funPtrInAs' bridges it.
newtype LabelledInt = LabelledInt CInt

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "inputs"
    [ testGroup "arity"
        [ testCase "input: String (1 C arg) then ByteString (2 C args)" $
            hsStrncmpMixed "hello world" "hello" >>= (@?= 0)
        , testCase "input1: one C argument" $
            hsStrlenOne "hello" >>= (@?= 5)
        , testCase "input2: one value fills a (ptr, len) C pair" $
            hsBsLenTwo "hello" >>= (@?= 5)
        , testCase "input3: one value fills three C arguments" $
            hsAddThree (3, 4, 5) >>= (@?= 12)
        ]

    , testGroup "marshalOptional"
        [ testCase "1 C arg: Nothing becomes a NULL pointer" $
            hsCheckPresent Nothing >>= (@?= False)
        , testCase "1 C arg: Just becomes a non-NULL pointer" $
            hsCheckPresent (Just "x") >>= (@?= True)
        , testCase "2 C args: Nothing fills both pointer and length" $
            hsCheckLen Nothing >>= (@?= (-1))
        , testCase "2 C args: Just fills both from the ByteString" $
            hsCheckLen (Just "hello") >>= (@?= 5)
        ]

    , testGroup "ready-made marshallers"
        [ testCase "withCStringMutIn fills a non-const char*" $
            hsStrlenMut "hello" >>= (@?= 5)
        , testCase "withCStringArrayIn: [String] as const char *const *" $
            hsFirstArgChar ["hi", "there"] >>= (@?= fromEnum 'h')
        , testCase "constByteStringLenIn: ByteString as (const uchar*, ull)" $
            hsReportLen "hello" >>= (@?= 5)
        , testCase "unsafeByteStringLenIn: zero-copy (ptr, len), embedded NUL kept" $
            hsEchoLen "a\NULbc" >>= (@?= "a\NULbc")
        , testProperty "unsafeByteStringLenIn round-trips arbitrary bytes" $
            \(bytes :: [Word8]) ->
              let bs = BS.pack bytes in ioProperty ((=== bs) <$> hsEchoLen bs)
        ]

    , testGroup "callbacks"
        [ testCase "funPtrIn: the callback is invoked during the call" $ do
            ref <- newIORef 0
            hsCallOnce $ \n -> writeIORef ref (fromIntegral n :: Int)
            readIORef ref >>= (@?= 42)
        , testCase "funPtrInAs: a coercible callback bridges onto a covered one" $ do
            ref <- newIORef 0
            hsCallOnceAs $ \(LabelledInt n) -> writeIORef ref (fromIntegral n :: Int)
            readIORef ref >>= (@?= 42)
        ]

    , testGroup "pointer const-ness"
        [ testCase "asMutableArg: a const-pointer marshaller fills a Ptr" $
            hsFirstByteMut "xyz" >>= (@?= fromEnum 'x')
        , testCase "asConstArg: a Ptr-filling marshaller serves a const argument" $
            withCString "z" hsFirstByteConst >>= (@?= fromEnum 'z')
        ]
    ]

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

-- | Mixed C-argument widths: 'String' (1 C arg) then 'ByteString' (2 C args).
hsStrncmpMixed :: String -> ByteString -> IO Int
hsStrncmpMixed = toHighLevel ( input withCStringIn
                             $ input useAsByteStringLenIn
                             $ resultPure fromIntegral
                             ) c_strncmp

hsStrlenOne :: String -> IO Int
hsStrlenOne = toHighLevel (input1 withCStringIn $ resultPure fromIntegral) c_strlen

hsBsLenTwo :: ByteString -> IO Int
hsBsLenTwo = toHighLevel (input2 useAsByteStringLenIn $ resultPure fromIntegral) cBsLen

hsAddThree :: (Int, Int, Int) -> IO Int
hsAddThree = toHighLevel (input3 tripleIn $ resultPure fromIntegral) addThree

hsCheckPresent :: Maybe String -> IO Bool
hsCheckPresent = toHighLevel
    ( input (marshalOptional ($ nullConst) withCStringIn)
    $ resultPure (/= 0)
    ) checkPresent

hsCheckLen :: Maybe ByteString -> IO Int
hsCheckLen = toHighLevel
    ( input (marshalOptional (\lo -> lo nullConst 0) useAsByteStringLenIn)
    $ resultPure fromIntegral
    ) checkLen

hsStrlenMut :: String -> IO Int
hsStrlenMut = toHighLevel
    (input withCStringMutIn $ resultPure fromIntegral) c_strlen_raw

hsFirstArgChar :: [String] -> IO Int
hsFirstArgChar = toHighLevel
    (input withCStringArrayIn $ resultPure fromIntegral) firstArgChar

hsReportLen :: ByteString -> IO Int
hsReportLen = toHighLevel
    (input constByteStringLenIn $ resultPure fromIntegral) reportLen

hsEchoLen :: ByteString -> IO ByteString
hsEchoLen = toHighLevel (input unsafeByteStringLenIn $ resultPure id) echoBytesLen

hsCallOnce :: Callback -> IO ()
hsCallOnce = toHighLevel (input funPtrIn $ discardResult) callOnce

hsCallOnceAs :: (LabelledInt -> IO ()) -> IO ()
hsCallOnceAs = toHighLevel (input funPtrInAs $ discardResult) callOnce

hsFirstByteMut :: String -> IO Int
hsFirstByteMut = toHighLevel
    (input (asMutableArg withCStringIn) $ resultPure fromIntegral) firstByteMut

hsFirstByteConst :: Ptr CChar -> IO Int
hsFirstByteConst = toHighLevel
    (input (asConstArg (scalar id)) $ resultPure fromIntegral) firstByteConst
