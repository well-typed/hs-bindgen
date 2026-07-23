{-# LANGUAGE OverloadedStrings #-}

-- | 'auto' and the default marshallers behind it. 'auto' fills every mundane
-- input and the closer from the wrapper's signature; these cases pin the
-- individual 'DefaultIn' \/ 'DefaultOut' \/ 'DefaultRes' instances it resolves,
-- including a binding-defined default for a custom type.
module Test.HsBindgen.HighLevel.Auto (tests) where

import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CInt (..))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import HsBindgen.HighLevel (input, output, toHighLevel)
import HsBindgen.HighLevel.Defaults (DefaultIn (..), auto, defaultOut,
                                     defaultRes)
import HsBindgen.HighLevel.Marshaller (scalar)

import Test.HsBindgen.HighLevel.Util (Callback, cAdd, cBsLen, c_strlen,
                                      c_strlen_raw, callOnce, checkPresent,
                                      firstArgChar, firstByteConst, peekIntOut,
                                      reportLen)

{-------------------------------------------------------------------------------
  Fixtures (local; the shared C stand-ins live in Util)
-------------------------------------------------------------------------------}

-- | Writes @10 * n@ to the out-parameter and returns @n@.
outThenIn :: Ptr CInt -> CInt -> IO CInt
outThenIn pOut (CInt n) = poke pOut (CInt (n * 10)) >> pure (CInt n)

-- | A binding-defined default for a custom type: one instance and @input defaultIn@
-- (or 'auto') works for 'Handle' with no change to the library. 'Handle'\'s C
-- argument is @CInt@, a different type, so this does not overlap the identity
-- default.
newtype Handle = Handle CInt

instance DefaultIn Handle (CInt -> lo) lo where
  defaultIn = scalar (\(Handle h) -> h)

cHandle :: CInt -> IO CInt
cHandle h = pure (h + 1)

readFirstInt :: PtrConst CInt -> IO CInt
readFirstInt = PtrConst.peek

-- | An opaque generated type with no enumerated default: the catch-all identity
-- @DefaultIn@ instance still passes a @Ptr Widget@ through.
data Widget

takesWidgetPtr :: Ptr Widget -> IO CInt
takesWidgetPtr _ = pure 0

-- | Writes @99@ to the out-parameter and returns @7@.
outAndStatus :: Ptr CInt -> IO CInt
outAndStatus p = poke p 99 >> pure 7

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "auto and defaults"
    [ testGroup "auto"
        [ testCase "bare: fills the String input and the CSize result" $
            hsStrlenAutoBare "hello" >>= (@?= 5)
        , testCase "after an explicit output: auto fills the rest" $
            hsOutThenInAuto 5 >>= (@?= (50, 5))
        , testCase "raw-C wrapper: identity inputs and identity result" $
            hsAddAutoRaw 3 4 >>= (@?= 7)
        , testCase "multi-C-arg: a ByteString fills a (char*, size_t) pair" $
            hsBsLenAutoBare "hello" >>= (@?= 5)
        , testCase "the general ByteString default also fits a (uchar*, ull) pair" $
            hsReportLenAuto "hello" >>= (@?= 5)
        , testCase "Maybe String: Nothing to NULL, Just to a pointer" $ do
            hsCheckPresentAuto Nothing    >>= (@?= 0)
            hsCheckPresentAuto (Just "x") >>= (@?= 1)
        , testCase "PtrConst identity passes a const pointer through" $
            withCString "z" (hsFirstByteConstAuto . PtrConst.unsafeFromPtr) >>= (@?= fromEnum 'z')
        , testCase "a Haskell function fills a FunPtr callback" $ do
            ref <- newIORef 0
            hsCallOnceAuto $ \n -> writeIORef ref (fromIntegral n :: Int)
            readIORef ref >>= (@?= 42)
        , testCase "the catch-all identity passes an arbitrary Ptr through" $
            hsTakesWidgetPtr nullPtr >>= (@?= 0)
        , testCase "[String] argv resolves via DefaultIn" $
            hsFirstArgCharAuto ["zed"] >>= (@?= fromEnum 'z')
        , testCase "DefaultIn String fills a non-const char*" $
            hsStrlenMutAuto "world" >>= (@?= 5)
        ]

    , testGroup "explicit defaults"
        [ testCase "input defaultIn: two scalars and a result" $
            hsAddAuto 3 4 >>= (@?= 7)
        , testCase "input defaultIn: an IncompleteArray as a const pointer" $
            hsReadFirstInt (IA.fromList [CInt 7, CInt 8, CInt 9]) >>= (@?= 7)
        , testCase "input defaultIn: a user-defined default for a custom type" $
            hsHandleAuto (Handle 41) >>= (@?= 42)
        , testCase "output defaultOut: a scalar out-parameter coerces via the default" $
            hsDefaultOut >>= (@?= (99, 7))
        ]
    ]

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

hsStrlenAutoBare :: String -> IO Int
hsStrlenAutoBare = toHighLevel auto c_strlen

hsOutThenInAuto :: Int -> IO (Int, Int)
hsOutThenInAuto = toHighLevel (output peekIntOut $ auto) outThenIn

hsAddAutoRaw :: CInt -> CInt -> IO CInt
hsAddAutoRaw = toHighLevel auto cAdd

hsBsLenAutoBare :: ByteString -> IO Int
hsBsLenAutoBare = toHighLevel auto cBsLen

hsReportLenAuto :: ByteString -> IO Int
hsReportLenAuto = toHighLevel auto reportLen

hsCheckPresentAuto :: Maybe String -> IO Int
hsCheckPresentAuto = toHighLevel auto checkPresent

hsFirstByteConstAuto :: PtrConst CChar -> IO Int
hsFirstByteConstAuto = toHighLevel auto firstByteConst

hsCallOnceAuto :: Callback -> IO ()
hsCallOnceAuto = toHighLevel auto callOnce

hsTakesWidgetPtr :: Ptr Widget -> IO Int
hsTakesWidgetPtr = toHighLevel auto takesWidgetPtr

hsFirstArgCharAuto :: [String] -> IO Int
hsFirstArgCharAuto = toHighLevel auto firstArgChar

hsStrlenMutAuto :: String -> IO Int
hsStrlenMutAuto = toHighLevel auto c_strlen_raw

hsAddAuto :: Int -> Int -> IO Int
hsAddAuto = toHighLevel (input defaultIn $ input defaultIn $ defaultRes) cAdd

hsReadFirstInt :: IA.IncompleteArray CInt -> IO Int
hsReadFirstInt = toHighLevel (input defaultIn $ defaultRes) readFirstInt

hsHandleAuto :: Handle -> IO Int
hsHandleAuto = toHighLevel (input defaultIn $ defaultRes) cHandle

hsDefaultOut :: IO (Int, Int)
hsDefaultOut = toHighLevel (output defaultOut $ defaultRes) outAndStatus
