{-# LANGUAGE OverloadedStrings #-}

-- | 'auto' and the default marshallers behind it. 'auto' fills every mundane
-- input and the closer from the wrapper's signature; these cases pin the
-- individual 'DefaultIn' \/ 'DefaultOut' \/ 'DefaultRes' instances it resolves,
-- including a binding-defined default for a custom type.
module Test.HsBindgen.HighLevel.Auto (tests) where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..), CChar, CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import HsBindgen.HighLevel (input, output, resultPure, scratchArray,
                            toHighLevel)
import HsBindgen.HighLevel.Auto (auto, autoChecked, autoInputs, autoResult,
                                 autoWith, checkedResult)
import HsBindgen.HighLevel.Defaults (DefaultIn (..), defaultOut, defaultRes)
import HsBindgen.HighLevel.Marshaller (Unmarshaller, scalar, unmarshalOutPure,
                                       unmarshalOutWith)

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

-- | Writes NULL to a @Widget **@ out-parameter and returns @7@. A @Widget@ is not one
-- of the types 'DefaultOut' names, so this only resolves through the general identity.
outWidgetPtr :: Ptr (Ptr Widget) -> IO CInt
outWidgetPtr p = poke p nullPtr >> pure 7

-- | The @qrcodegen_encodeText@ shape, down to the @bool@ return: a leading string, a
-- scratch buffer, the out-parameter that is kept, two more mundane inputs, a flag,
-- and a @bool@ status. The c-qrcode example is written exactly like this.
encodeShaped :: PtrConst CChar -> Ptr CChar -> Ptr CInt -> CInt -> CInt -> CBool
             -> IO CBool
encodeShaped _text _tmp out a b (CBool boost) =
    poke out (a + b) >> pure (CBool (if boost /= 0 then 1 else 0))

-- | Three out-parameters and a @void@ return.
threeOutVoid :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
threeOutVoid p q r = poke p 1 >> poke q 2 >> poke r 3

-- | Five out-parameters and a status: the widest shape @auto@ assembles.
fiveOutStatus :: Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
fiveOutStatus a b c d e = do
    poke a 1; poke b 2; poke c 3; poke d 4; poke e 5
    pure 6

-- | Two out-parameters and a @void@ return, the shape @auto@ closes to the outputs
-- alone.
twoOutVoid :: CInt -> Ptr CInt -> Ptr CInt -> IO ()
twoOutVoid n p q = void (twoOutStatus n p q)

-- | Two out-parameters and a status, the shape @auto@ closes to outputs plus return.
twoOutStatus :: CInt -> Ptr CInt -> Ptr CInt -> IO CInt
twoOutStatus n p q = poke p (n + 1) >> poke q (n + 2) >> pure (n * 10)

-- | 'twoOutStatus' with the out-parameters first, C's usual convention. That order
-- is what 'autoChecked' needs, since it closes as soon as the wrapper's arguments
-- run out and so leaves no room for an 'output' below it.
twoOutThenIn :: Ptr CInt -> Ptr CInt -> CInt -> IO CInt
twoOutThenIn p q n = twoOutStatus n p q

-- | Thrown by the 'checkedResult' fixtures when the status is non-zero.
newtype BadStatus = BadStatus Int
  deriving stock (Eq, Show)

instance Exception BadStatus

-- | The check 'checkedResult' is given: reject any non-zero status.
rejectNonZero :: CInt -> IO ()
rejectNonZero c = when (c /= 0) $ throwIO (BadStatus (fromIntegral c))

-- | An out-parameter whose /reader/ is observable, so a test can tell whether the
-- read-back ran at all. 'checkedResult' promises it does not run when the check
-- throws.
trackingOut :: IORef Bool -> Unmarshaller (Ptr CInt) Int
trackingOut ref =
    unmarshalOutWith alloca $ \p -> do
      writeIORef ref True
      fromIntegral <$> peek p

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "auto and defaults"
    [ testGroup "auto"
        [ testCase "bare: fills the String input and the CSize result" $
            hsStrlenAutoBare "hello" >>= (@?= 5)
        , testCase "under an explicit output: fills the input and assembles" $
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
        , testCase "output defaultOut: the identity keeps the C type unconverted" $
            hsDefaultOutRaw >>= (@?= (CInt 99, 7))
        , testCase "output defaultOut: the identity covers an arbitrary Storable" $
            hsDefaultOutWidgetPtr >>= (@?= (nullPtr, 7))
        ]

    , testGroup "auto over a spec with outputs"
        [ testCase "fills the run of inputs after an output, then assembles" $
            hsEncodeShaped "x" 2 3 True >>= (@?= (5, True))
        , testCase "a void return closes to the outputs alone" $
            hsTwoOutVoid 5 >>= (@?= (6, 7))
        , testCase "a non-void return is the last component, converted" $
            hsTwoOutStatus 5 >>= (@?= (6, 7, 50))
        , testCase "autoResult closes a spec whose inputs are explicit" $
            hsTwoOutExplicitIn 5 >>= (@?= (6, 7, 50))
        , testCase "the signature fixes a polymorphic output marshaller" $
            hsTwoOutInferred 5 >>= (@?= (6, 7, 50))
        , testCase "three outputs, void return" $
            hsThreeOutVoid >>= (@?= (1, 2, 3))
        , testCase "five outputs plus a converted return, the widest shape" $
            hsFiveOutStatus >>= (@?= (1, 2, 3, 4, 5, 6))
        , testCase "a hand-written closer still overrides the shape" $
            hsEncodeShapedByHand "x" 2 3 True >>= (@?= (5, "ok"))
        ]

    , testGroup "autoInputs"
        [ testCase "fills the inputs, leaving a hand-written closer" $ do
            hsAddIsZero 3 (-3) >>= (@?= True)
            hsAddIsZero 3 4    >>= (@?= False)
        , testCase "one wrapper argument may still cover two C arguments" $
            hsBsLenIsFive "hello" >>= (@?= True)
        , testCase "the tail may hold outputs, not just a closer" $
            hsTwoOutAutoIn 5 >>= (@?= (6, 7))
        , testCase "a wrapper with no arguments is just its tail" $
            hsNoInputs >>= (@?= (99, 7))
        , testCase "autoInputs autoResult agrees with auto" $ do
            expected <- hsTwoOutStatus 5
            hsTwoOutViaAutoInputs 5 >>= (@?= expected)
        ]

    , testGroup "autoWith and autoChecked"
        [ testCase "autoWith: inputs from the signature, result by hand" $ do
            hsAddIsZeroWith 3 (-3) >>= (@?= True)
            hsAddIsZeroWith 3 4    >>= (@?= False)
        , testCase "autoWith: an output written above it still assembles" $
            hsOutThenInWith 5 >>= (@?= (50, 5))
        , testCase "autoChecked: a passing check assembles the outputs" $
            hsAutoCheckedTwoOut 0 >>= (@?= (1, 2))
        , testCase "autoChecked: a failing check throws" $ do
            r <- try (hsAutoCheckedTwoOut 5)
            r @?= (Left (BadStatus 50) :: Either BadStatus (Int, Int))
        , testCase "autoChecked: no outputs closes to IO ()" $
            hsAutoCheckedUnit 3 (-3) >>= (@?= ())
        , testCase "autoChecked agrees with autoInputs + checkedResult" $ do
            expected <- hsCheckedTwoOut 0
            hsAutoCheckedTwoOut 0 >>= (@?= expected)
        ]

    , testGroup "checkedResult"
        [ testCase "a passing check assembles the outputs, dropping the status" $
            hsCheckedTwoOut 0 >>= (@?= (1, 2))
        , testCase "a failing check throws the check's own exception" $ do
            r <- try (hsCheckedTwoOut 5)
            r @?= (Left (BadStatus 50) :: Either BadStatus (Int, Int))
        , testCase "a failing check skips the read-backs entirely" $ do
            ref <- newIORef False
            _   <- try (hsCheckedTracking ref 5) :: IO (Either BadStatus Int)
            readIORef ref >>= assertBool "read-back must not run past a failed check" . not
        , testCase "a passing check does run the read-backs" $ do
            ref <- newIORef False
            _   <- hsCheckedTracking ref 0
            readIORef ref >>= assertBool "read-back must run when the check passes"
        , testCase "an always-passing check discards the return value" $
            hsIgnoredStatus 5 >>= (@?= (6, 7))
        ]
    ]

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

hsStrlenAutoBare :: String -> IO Int
hsStrlenAutoBare = toHighLevel auto c_strlen

-- | @auto@ picks up directly under an explicit @output@: it fills the mundane input
-- and assembles the output with the converted C return.
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

-- | The c-qrcode wrapper, position for position: one explicit input, one scratch
-- buffer, one output, and 'auto' for the whole remaining run of mundane inputs and
-- then the result.
hsEncodeShaped :: String -> Int -> Int -> Bool -> IO (Int, Bool)
hsEncodeShaped = toHighLevel
    ( input defaultIn            -- text
    $ scratchArray @CChar 16     -- scratch buffer, never seen
    $ output defaultOut          -- the out-parameter we keep
    $ auto                       -- three more mundane inputs, then the result
    ) encodeShaped

-- | The same spec closed by hand, which is how to express a result shape 'auto'
-- does not build.
hsEncodeShapedByHand :: String -> Int -> Int -> Bool -> IO (Int, String)
hsEncodeShapedByHand = toHighLevel
    ( input defaultIn
    $ scratchArray @CChar 16
    $ output defaultOut
    $ input defaultIn $ input defaultIn $ input defaultIn
    $ resultPure (\v ok -> (v, if ok /= CBool 0 then "ok" else "failed"))
    ) encodeShaped

-- | A @void@ return contributes nothing, so 'auto' closes to the outputs alone.
hsTwoOutVoid :: Int -> IO (Int, Int)
hsTwoOutVoid = toHighLevel
    (input defaultIn $ output defaultOut $ output defaultOut $ auto) twoOutVoid

-- | The wider arities of the 'AutoResult' table, which nothing else covers: the
-- assembler's arguments must arrive in spec order at every width.
hsThreeOutVoid :: IO (Int, Int, Int)
hsThreeOutVoid = toHighLevel
    (output defaultOut $ output defaultOut $ output defaultOut $ auto) threeOutVoid

hsFiveOutStatus :: IO (Int, Int, Int, Int, Int, Int)
hsFiveOutStatus = toHighLevel
    ( output defaultOut $ output defaultOut $ output defaultOut
    $ output defaultOut $ output defaultOut
    $ auto
    ) fiveOutStatus

-- | A non-void return becomes the last tuple component, converted by 'DefaultRes'.
hsTwoOutStatus :: Int -> IO (Int, Int, Int)
hsTwoOutStatus = toHighLevel
    (input defaultIn $ output defaultOut $ output defaultOut $ auto) twoOutStatus

-- | 'autoResult' on its own: the inputs carry explicit marshallers, the closer does not.
hsTwoOutExplicitIn :: Int -> IO (Int, Int, Int)
hsTwoOutExplicitIn = toHighLevel
    ( input (scalar (fromIntegral :: Int -> CInt))
    $ output defaultOut
    $ output defaultOut
    $ autoResult
    ) twoOutStatus

-- | The out-parameters use a marshaller polymorphic in its Haskell type. The wrapper
-- signature is what fixes it: 'AutoResult' keeps its type equalities in the instance
-- context precisely so information can flow this way.
hsTwoOutInferred :: Int -> IO (Int, Int, Int)
hsTwoOutInferred = toHighLevel
    ( input defaultIn
    $ output (unmarshalOutPure fromIntegral)
    $ output (unmarshalOutPure fromIntegral)
    $ auto
    ) twoOutStatus

hsDefaultOut :: IO (Int, Int)
hsDefaultOut = toHighLevel
    (output defaultOut $ resultPure (\v c -> (v, fromIntegral (c :: CInt)))) outAndStatus

-- | The same out-parameter kept at its C type. The wrapper's result type is the only
-- thing that decides between this and 'hsDefaultOut'.
hsDefaultOutRaw :: IO (CInt, Int)
hsDefaultOutRaw = toHighLevel
    (output defaultOut $ resultPure (\v c -> (v, fromIntegral (c :: CInt)))) outAndStatus

-- | One general instance covers the identity, so a type nothing enumerates still has
-- an output default.
hsDefaultOutWidgetPtr :: IO (Ptr Widget, Int)
hsDefaultOutWidgetPtr = toHighLevel
    (output defaultOut $ resultPure (\v c -> (v, fromIntegral (c :: CInt)))) outWidgetPtr

{-------------------------------------------------------------------------------
  autoInputs: the inputs from the signature, the rest by hand
-------------------------------------------------------------------------------}

-- | The motivating case: every input is a default, but @CInt -> Bool@ is not a
-- 'DefaultRes', so @auto@ cannot close it. Without 'autoInputs' both inputs would
-- have to be written out just to reach the closer.
hsAddIsZero :: Int -> Int -> IO Bool
hsAddIsZero = toHighLevel (autoInputs $ resultPure (== 0)) cAdd

-- | One wrapper argument, two C arguments: 'autoInputs' hands the tail the callable
-- with /both/ already filled, so a multi-argument default is no obstacle.
hsBsLenIsFive :: ByteString -> IO Bool
hsBsLenIsFive = toHighLevel (autoInputs $ resultPure (== 5)) cBsLen

-- | The tail is a spec, not just a closer: 'autoInputs' stops at the first position
-- the wrapper takes no argument for, and the outputs below it are written as usual.
hsTwoOutAutoIn :: Int -> IO (Int, Int)
hsTwoOutAutoIn = toHighLevel
    ( autoInputs
    $ output defaultOut
    $ output defaultOut
    $ resultPure (\p q _ -> (p, q))   -- the status is dropped
    ) twoOutStatus

-- | The degenerate case: the wrapper takes no arguments at all, so 'autoInputs'
-- fills nothing and is exactly its tail.
hsNoInputs :: IO (Int, Int)
hsNoInputs = toHighLevel
    ( autoInputs
    $ output defaultOut
    $ resultPure (\v c -> (v, fromIntegral (c :: CInt)))
    ) outAndStatus

-- | @autoInputs autoResult@ is what @auto@ does; this pins that they agree.
hsTwoOutViaAutoInputs :: Int -> IO (Int, Int, Int)
hsTwoOutViaAutoInputs = toHighLevel
    ( autoInputs
    $ output defaultOut
    $ output defaultOut
    $ autoResult
    ) twoOutStatus

{-------------------------------------------------------------------------------
  checkedResult: a status guarding the outputs
-------------------------------------------------------------------------------}

-- | The C return is a status, so the result is the outputs alone. Passing @0@ makes
-- 'twoOutStatus' return @0@, which 'rejectNonZero' accepts.
hsCheckedTwoOut :: Int -> IO (Int, Int)
hsCheckedTwoOut = toHighLevel
    ( autoInputs
    $ output defaultOut
    $ output defaultOut
    $ checkedResult rejectNonZero
    ) twoOutStatus

-- | The read-back is observable through @ref@, so a test can pin that a failed check
-- skips it. Only the first out-parameter is tracked; the second is discarded by the
-- single-output result shape.
hsCheckedTracking :: IORef Bool -> Int -> IO Int
hsCheckedTracking ref = toHighLevel
    ( autoInputs
    $ output (trackingOut ref)
    $ scratchArray 1
    $ checkedResult rejectNonZero
    ) twoOutStatus

{-------------------------------------------------------------------------------
  autoWith / autoChecked: the same specs written in one word

  Each of these is the corresponding spec above with @autoInputs $ closer@ collapsed.
  No annotations: the point is that the merged forms infer from the wrapper's
  signature alone, the same as the composed forms they stand for.
-------------------------------------------------------------------------------}

-- | 'hsAddIsZero' written with the merged combinator.
hsAddIsZeroWith :: Int -> Int -> IO Bool
hsAddIsZeroWith = toHighLevel (autoWith (== 0)) cAdd

-- | 'autoWith' over a spec that has an output. The merged forms close as soon as the
-- wrapper runs out of arguments, so any 'output' has to be written /above/ them,
-- which means the C function must take its out-parameters first. That is the usual C
-- convention, and 'outThenIn' follows it.
hsOutThenInWith :: Int -> IO (Int, Int)
hsOutThenInWith = toHighLevel
    (output peekIntOut $ autoWith (\o c -> (o, fromIntegral (c :: CInt)))) outThenIn

-- | 'hsCheckedTwoOut' written with the merged combinator, over the out-parameters
-- first shape.
hsAutoCheckedTwoOut :: Int -> IO (Int, Int)
hsAutoCheckedTwoOut = toHighLevel
    ( output defaultOut
    $ output defaultOut
    $ autoChecked rejectNonZero
    ) twoOutThenIn

-- | With no outputs at all the assembled result is @()@, so a status-only call is
-- one word. 'cAdd' returns its sum, so @3 + (-3)@ passes the check and @3 + 4@ does
-- not.
hsAutoCheckedUnit :: Int -> Int -> IO ()
hsAutoCheckedUnit = toHighLevel (autoChecked rejectNonZero) cAdd

-- | A check that never rejects, which reduces 'checkedResult' to \"assemble the
-- outputs and ignore the C return\": 'HsBindgen.HighLevel.discardResult'
-- generalised to a spec that has outputs.
hsIgnoredStatus :: Int -> IO (Int, Int)
hsIgnoredStatus = toHighLevel
    ( autoInputs
    $ output defaultOut
    $ output defaultOut
    $ checkedResult (\_ -> pure ())
    ) twoOutStatus
