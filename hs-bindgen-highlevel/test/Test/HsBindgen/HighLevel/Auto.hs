{-# LANGUAGE OverloadedStrings #-}

-- | 'auto' and the default marshallers behind it. 'auto' fills every mundane
-- input and the closer from the high-level signature; these cases pin the
-- individual 'DefaultIn' \/ 'DefaultOut' \/ 'DefaultRes' instances it resolves,
-- including a binding-defined default for a custom type.
module Test.HsBindgen.HighLevel.Auto (tests) where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.String (newCString, withCString)
import Foreign.C.Types (CBool (..), CChar, CInt (..), CLLong, CLong, CSChar,
                        CShort, CSize, CUChar, CULLong, CULong, CUShort)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import HsBindgen.HighLevel (ToHighLevel, input, output, resultPure,
                            scratchArray, toHighLevel)
import HsBindgen.HighLevel.Auto (AutoOutputs, auto, autoChecked, autoInputs,
                                 autoMaybe, autoResult, autoWith, checkedResult,
                                 eitherResult, maybeResult)
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
-- is what 'autoChecked' needs, since it closes as soon as the high-level arguments
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

    , testGroup "integral widths, in all three combinators"
        [ testCase "an integral return converts to Int at any width" $ do
            hsWidthsSigned >>= (@?= (1, 2, 3, 4))
        , testCase "an unsigned integral return converts to Word" $
            hsWidthsUnsigned >>= (@?= (1, 2, 3, 4))
        , testCase "the same widths are accepted as arguments" $
            hsWidthArgs 1 2 3 4 >>= (@?= 10)
        , testCase "and read back from out-parameters" $
            hsWidthOuts >>= (@?= (1, 2))
        , testCase "a borrowed const char * is copied out as a String" $
            hsBorrowedString >>= (@?= "borrowed")
        , testCase "a borrowed const char * is copied out as a ByteString" $
            hsBorrowedBytes >>= (@?= "borrowed")
        , testCase "a non-const char * return works the same way" $
            hsBorrowedMut >>= (@?= "borrowed")
        , testCase "a nullable const char * is Nothing at NULL" $ do
            hsMaybeString True  >>= (@?= Just "borrowed")
            hsMaybeString False >>= (@?= Nothing)
        , testCase "the nullable rule over the identity: Maybe (Ptr a)" $ do
            hsMaybePtr True  >>= (\r -> assertBool "a non-NULL return is Just" (r /= Nothing))
            hsMaybePtr False >>= (@?= Nothing)
        , testCase "a default that needs IO still resolves under auto" $
            hsBorrowedStringAuto >>= (@?= "borrowed")
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
        , testCase "one high-level argument may still cover two C arguments" $
            hsBsLenIsFive "hello" >>= (@?= True)
        , testCase "the tail may hold outputs, not just a closer" $
            hsTwoOutAutoIn 5 >>= (@?= (6, 7))
        , testCase "a binding with no arguments is just its tail" $
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
        , testCase "one closer, named once, closes at two different arities" $ do
            hsNamedCloserUnit 3 (-3) >>= (@?= ())
            hsNamedCloserPair 0      >>= (@?= (1, 2))
        ]

    , testGroup "maybeResult and eitherResult"
        [ testCase "maybeResult: an accepted status assembles the outputs" $
            hsMaybeTwoOut 0 >>= (@?= Just (1, 2))
        , testCase "maybeResult: a rejected status is Nothing" $
            hsMaybeTwoOut 5 >>= (@?= Nothing)
        , testCase "maybeResult: a rejected status skips the read-backs" $ do
            ref <- newIORef False
            _   <- hsMaybeTracking ref 5
            readIORef ref >>= assertBool "read-back must not run" . not
        , testCase "maybeResult: an accepted status does run them" $ do
            ref <- newIORef False
            _   <- hsMaybeTracking ref 0
            readIORef ref >>= assertBool "read-back must run"
        , testCase "eitherResult: Right, assembled from the outputs" $
            hsEitherTwoOut 0 >>= (@?= Right (1, 2))
        , testCase "eitherResult: Left, carrying the classified status" $
            hsEitherTwoOut 5 >>= (@?= Left 50)
        , testCase "autoMaybe agrees with autoInputs + maybeResult" $ do
            expected <- hsMaybeTwoOut 0
            hsAutoMaybeTwoOut 0 >>= (@?= expected)
        , testCase "autoMaybe: no outputs closes to IO (Maybe ())" $ do
            hsAutoMaybeUnit 3 (-3) >>= (@?= Just ())
            hsAutoMaybeUnit 3 4    >>= (@?= Nothing)
        ]
    ]

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

hsStrlenAutoBare :: String -> IO Int
hsStrlenAutoBare = toHighLevel c_strlen auto

-- | @auto@ picks up directly under an explicit @output@: it fills the mundane input
-- and assembles the output with the converted C return.
hsOutThenInAuto :: Int -> IO (Int, Int)
hsOutThenInAuto = toHighLevel outThenIn (output peekIntOut $ auto)

hsAddAutoRaw :: CInt -> CInt -> IO CInt
hsAddAutoRaw = toHighLevel cAdd auto

hsBsLenAutoBare :: ByteString -> IO Int
hsBsLenAutoBare = toHighLevel cBsLen auto

hsReportLenAuto :: ByteString -> IO Int
hsReportLenAuto = toHighLevel reportLen auto

hsCheckPresentAuto :: Maybe String -> IO Int
hsCheckPresentAuto = toHighLevel checkPresent auto

hsFirstByteConstAuto :: PtrConst CChar -> IO Int
hsFirstByteConstAuto = toHighLevel firstByteConst auto

hsCallOnceAuto :: Callback -> IO ()
hsCallOnceAuto = toHighLevel callOnce auto

hsTakesWidgetPtr :: Ptr Widget -> IO Int
hsTakesWidgetPtr = toHighLevel takesWidgetPtr auto

hsFirstArgCharAuto :: [String] -> IO Int
hsFirstArgCharAuto = toHighLevel firstArgChar auto

hsStrlenMutAuto :: String -> IO Int
hsStrlenMutAuto = toHighLevel c_strlen_raw auto

hsAddAuto :: Int -> Int -> IO Int
hsAddAuto = toHighLevel cAdd (input defaultIn $ input defaultIn $ defaultRes)

hsReadFirstInt :: IA.IncompleteArray CInt -> IO Int
hsReadFirstInt = toHighLevel readFirstInt (input defaultIn $ defaultRes)

hsHandleAuto :: Handle -> IO Int
hsHandleAuto = toHighLevel cHandle (input defaultIn $ defaultRes)

-- | The c-qrcode wrapper, combinator for combinator: one explicit input, one scratch
-- buffer, one output, and 'auto' for the whole remaining run of mundane inputs and
-- then the result.
hsEncodeShaped :: String -> Int -> Int -> Bool -> IO (Int, Bool)
hsEncodeShaped = toHighLevel encodeShaped
               $ input defaultIn        -- text
               $ scratchArray @CChar 16 -- scratch buffer, never seen
               $ output defaultOut      -- the out-parameter we keep
               $ auto                   -- three more mundane inputs, then the result

-- | The same spec closed by hand, which is how to express a result shape 'auto'
-- does not build.
hsEncodeShapedByHand :: String -> Int -> Int -> Bool -> IO (Int, String)
hsEncodeShapedByHand = toHighLevel encodeShaped
                     $ input defaultIn
                     $ scratchArray @CChar 16
                     $ output defaultOut
                     $ input defaultIn $ input defaultIn $ input defaultIn
                     $ resultPure (\v ok -> (v, if ok /= CBool 0 then "ok" else "failed"))

-- | A @void@ return contributes nothing, so 'auto' closes to the outputs alone.
hsTwoOutVoid :: Int -> IO (Int, Int)
hsTwoOutVoid = toHighLevel twoOutVoid
             $ input defaultIn
             $ output defaultOut
             $ output defaultOut
             $ auto

-- | The wider arities of the 'AutoResult' table, which nothing else covers: the
-- assembler's arguments must arrive in spec order at every width.
hsThreeOutVoid :: IO (Int, Int, Int)
hsThreeOutVoid = toHighLevel threeOutVoid
               $ output defaultOut
               $ output defaultOut
               $ output defaultOut
               $ auto

hsFiveOutStatus :: IO (Int, Int, Int, Int, Int, Int)
hsFiveOutStatus = toHighLevel fiveOutStatus
                $ output defaultOut
                $ output defaultOut
                $ output defaultOut
                $ output defaultOut
                $ output defaultOut
                $ auto

-- | A non-void return becomes the last tuple component, converted by 'DefaultRes'.
hsTwoOutStatus :: Int -> IO (Int, Int, Int)
hsTwoOutStatus = toHighLevel twoOutStatus
               $ input defaultIn
               $ output defaultOut
               $ output defaultOut
               $ auto

-- | 'autoResult' on its own: the inputs carry explicit marshallers, the closer does not.
hsTwoOutExplicitIn :: Int -> IO (Int, Int, Int)
hsTwoOutExplicitIn = toHighLevel twoOutStatus
                   $ input (scalar (fromIntegral :: Int -> CInt))
                   $ output defaultOut
                   $ output defaultOut
                   $ autoResult

-- | The out-parameters use a marshaller polymorphic in its Haskell type. The signature
-- signature is what fixes it: 'AutoResult' keeps its type equalities in the instance
-- context precisely so information can flow this way.
hsTwoOutInferred :: Int -> IO (Int, Int, Int)
hsTwoOutInferred = toHighLevel twoOutStatus
                 $ input defaultIn
                 $ output (unmarshalOutPure fromIntegral)
                 $ output (unmarshalOutPure fromIntegral)
                 $ auto

hsDefaultOut :: IO (Int, Int)
hsDefaultOut = toHighLevel outAndStatus
             $ output defaultOut
             $ resultPure (\v c -> (v, fromIntegral (c :: CInt)))

-- | The same out-parameter kept at its C type. The high-level result type is the only
-- thing that decides between this and 'hsDefaultOut'.
hsDefaultOutRaw :: IO (CInt, Int)
hsDefaultOutRaw = toHighLevel outAndStatus
                $ output defaultOut
                $ resultPure (\v c -> (v, fromIntegral (c :: CInt)))

-- | One general instance covers the identity, so a type nothing enumerates still has
-- an output default.
hsDefaultOutWidgetPtr :: IO (Ptr Widget, Int)
hsDefaultOutWidgetPtr = toHighLevel outWidgetPtr
                      $ output defaultOut
                      $ resultPure (\v c -> (v, fromIntegral (c :: CInt)))

{-------------------------------------------------------------------------------
  autoInputs: the inputs from the signature, the rest by hand
-------------------------------------------------------------------------------}

-- | The motivating case: every input is a default, but @CInt -> Bool@ is not a
-- 'DefaultRes', so @auto@ cannot close it. Without 'autoInputs' both inputs would
-- have to be written out just to reach the closer.
hsAddIsZero :: Int -> Int -> IO Bool
hsAddIsZero = toHighLevel cAdd
            $ autoInputs
            $ resultPure (== 0)

-- | One high-level argument, two C arguments: 'autoInputs' hands the tail the callable
-- with /both/ already filled, so a multi-argument default is no obstacle.
hsBsLenIsFive :: ByteString -> IO Bool
hsBsLenIsFive = toHighLevel cBsLen
              $ autoInputs
              $ resultPure (== 5)

-- | The tail is a spec, not just a closer: 'autoInputs' stops at the first combinator
-- the high-level type takes no argument for, and the outputs below it are written as
-- usual.
hsTwoOutAutoIn :: Int -> IO (Int, Int)
hsTwoOutAutoIn = toHighLevel twoOutStatus
               $ autoInputs
               $ output defaultOut
               $ output defaultOut
               $ resultPure (\p q _ -> (p, q)) -- the status is dropped

-- | The degenerate case: the high-level type takes no arguments at all, so 'autoInputs'
-- fills nothing and is exactly its tail.
hsNoInputs :: IO (Int, Int)
hsNoInputs = toHighLevel outAndStatus
           $ autoInputs
           $ output defaultOut
           $ resultPure (\v c -> (v, fromIntegral (c :: CInt)))

-- | @autoInputs autoResult@ is what @auto@ does; this pins that they agree.
hsTwoOutViaAutoInputs :: Int -> IO (Int, Int, Int)
hsTwoOutViaAutoInputs = toHighLevel twoOutStatus
                      $ autoInputs
                      $ output defaultOut
                      $ output defaultOut
                      $ autoResult

{-------------------------------------------------------------------------------
  checkedResult: a status guarding the outputs
-------------------------------------------------------------------------------}

-- | The C return is a status, so the result is the outputs alone. Passing @0@ makes
-- 'twoOutStatus' return @0@, which 'rejectNonZero' accepts.
hsCheckedTwoOut :: Int -> IO (Int, Int)
hsCheckedTwoOut = toHighLevel twoOutStatus
                $ autoInputs
                $ output defaultOut
                $ output defaultOut
                $ checkedResult rejectNonZero

-- | The read-back is observable through @ref@, so a test can pin that a failed check
-- skips it. Only the first out-parameter is tracked; the second is discarded by the
-- single-output result shape.
hsCheckedTracking :: IORef Bool -> Int -> IO Int
hsCheckedTracking ref = toHighLevel twoOutStatus
                      $ autoInputs
                      $ output (trackingOut ref)
                      $ scratchArray 1
                      $ checkedResult rejectNonZero

{-------------------------------------------------------------------------------
  autoWith / autoChecked: the same specs written in one word

  Each of these is the corresponding spec above with @autoInputs $ closer@ collapsed.
  No annotations: the point is that the merged forms infer from the high-level
  signature alone, the same as the composed forms they stand for.
-------------------------------------------------------------------------------}

-- | 'hsAddIsZero' written with the merged combinator.
hsAddIsZeroWith :: Int -> Int -> IO Bool
hsAddIsZeroWith = toHighLevel cAdd (autoWith (== 0))

-- | 'autoWith' over a spec that has an output. The merged forms close as soon as the
-- high-level type runs out of arguments, so any 'output' has to be written /above/
-- them,
-- which means the C function must take its out-parameters first. That is the usual C
-- convention, and 'outThenIn' follows it.
hsOutThenInWith :: Int -> IO (Int, Int)
hsOutThenInWith = toHighLevel outThenIn
                $ output peekIntOut
                $ autoWith (\o c -> (o, fromIntegral (c :: CInt)))

-- | 'hsCheckedTwoOut' written with the merged combinator, over the out-parameters
-- first shape.
hsAutoCheckedTwoOut :: Int -> IO (Int, Int)
hsAutoCheckedTwoOut = toHighLevel twoOutThenIn
                    $ output defaultOut
                    $ output defaultOut
                    $ autoChecked rejectNonZero

-- | With no outputs at all the assembled result is @()@, so a status-only call is
-- one word. 'cAdd' returns its sum, so @3 + (-3)@ passes the check and @3 + 4@ does
-- not.
hsAutoCheckedUnit :: Int -> Int -> IO ()
hsAutoCheckedUnit = toHighLevel cAdd (autoChecked rejectNonZero)

-- | A check that never rejects, which reduces 'checkedResult' to \"assemble the
-- outputs and ignore the C return\": 'HsBindgen.HighLevel.discardResult'
-- generalised to a spec that has outputs.
hsIgnoredStatus :: Int -> IO (Int, Int)
hsIgnoredStatus = toHighLevel twoOutStatus
                $ autoInputs
                $ output defaultOut
                $ output defaultOut
                $ checkedResult (\_ -> pure ())

{-------------------------------------------------------------------------------
  DefaultRes: the conversions the closer resolves

  'DefaultRes' runs in 'IO', which is what lets the string defaults copy out of
  memory C owns. These pin one instance each; the borrowed-pointer ones point at a
  static buffer, since the defaults free nothing.
-------------------------------------------------------------------------------}

-- | A NUL-terminated @"borrowed"@ that outlives every call below, standing in for a
-- string a C library owns and hands back by pointer.
borrowedBuf :: Ptr CChar
borrowedBuf = unsafePerformIO (newCString "borrowed")
{-# NOINLINE borrowedBuf #-}

c_borrowedConst :: IO (PtrConst CChar)
c_borrowedConst = pure (PtrConst.unsafeFromPtr borrowedBuf)

c_borrowedMut :: IO (Ptr CChar)
c_borrowedMut = pure borrowedBuf

c_maybeBorrowed :: CBool -> IO (PtrConst CChar)
c_maybeBorrowed (CBool b)
  | b /= 0    = c_borrowedConst
  | otherwise = pure (PtrConst.unsafeFromPtr nullPtr)

c_maybePtr :: CBool -> IO (Ptr CChar)
c_maybePtr (CBool b)
  | b /= 0    = pure borrowedBuf
  | otherwise = pure nullPtr

c_long  :: IO CLong
c_long  = pure 1
c_llong :: IO CLLong
c_llong = pure 2
c_short :: IO CShort
c_short = pure 3
c_schar :: IO CSChar
c_schar = pure 4

c_ulong  :: IO CULong
c_ulong  = pure 1
c_ullong :: IO CULLong
c_ullong = pure 2
c_ushort :: IO CUShort
c_ushort = pure 3
c_uchar  :: IO CUChar
c_uchar  = pure 4

hsWidthsSigned :: IO (Int, Int, Int, Int)
hsWidthsSigned = (,,,) <$> toHighLevel c_long  auto
                       <*> toHighLevel c_llong auto
                       <*> toHighLevel c_short auto
                       <*> toHighLevel c_schar auto

hsWidthsUnsigned :: IO (Word, Word, Word, Word)
hsWidthsUnsigned = (,,,) <$> toHighLevel c_ulong  auto
                         <*> toHighLevel c_ullong auto
                         <*> toHighLevel c_ushort auto
                         <*> toHighLevel c_uchar  auto

hsBorrowedString :: IO String
hsBorrowedString = toHighLevel c_borrowedConst defaultRes

hsBorrowedBytes :: IO ByteString
hsBorrowedBytes = toHighLevel c_borrowedConst defaultRes

hsBorrowedMut :: IO String
hsBorrowedMut = toHighLevel c_borrowedMut defaultRes

-- | The same instance reached through @auto@ rather than named, which is the point:
-- a copy-out default needs no more from the call site than a scalar one.
hsBorrowedStringAuto :: IO String
hsBorrowedStringAuto = toHighLevel c_borrowedConst auto

hsMaybeString :: Bool -> IO (Maybe String)
hsMaybeString = toHighLevel c_maybeBorrowed auto

-- | The nullable rule over the identity default, so no string is involved: a
-- @char *@ return that is NULL for "absent".
hsMaybePtr :: Bool -> IO (Maybe (Ptr CChar))
hsMaybePtr = toHighLevel c_maybePtr auto

{-------------------------------------------------------------------------------
  maybeResult / eitherResult: a status that reports absence rather than failure
-------------------------------------------------------------------------------}

hsMaybeTwoOut :: Int -> IO (Maybe (Int, Int))
hsMaybeTwoOut = toHighLevel twoOutStatus
              $ autoInputs
              $ output defaultOut
              $ output defaultOut
              $ maybeResult (== 0)

hsEitherTwoOut :: Int -> IO (Either Int (Int, Int))
hsEitherTwoOut = toHighLevel twoOutStatus
               $ autoInputs
               $ output defaultOut
               $ output defaultOut
               $ eitherResult (\c -> if c == 0 then Nothing else Just (fromIntegral c))

-- | The read-back is observable, so a test can pin that a rejected status skips it.
hsMaybeTracking :: IORef Bool -> Int -> IO (Maybe Int)
hsMaybeTracking ref = toHighLevel twoOutStatus
                    $ autoInputs
                    $ output (trackingOut ref)
                    $ scratchArray 1
                    $ maybeResult (== 0)

-- | 'hsMaybeTwoOut' written with the merged combinator, over the out-parameters
-- first shape.
hsAutoMaybeTwoOut :: Int -> IO (Maybe (Int, Int))
hsAutoMaybeTwoOut = toHighLevel twoOutThenIn
                  $ output defaultOut
                  $ output defaultOut
                  $ autoMaybe (== 0)

hsAutoMaybeUnit :: Int -> Int -> IO (Maybe ())
hsAutoMaybeUnit = toHighLevel cAdd (autoMaybe (== 0))

{-------------------------------------------------------------------------------
  A closer named once, at more than one arity

  The shape a binding reaches for when a whole C library shares a status
  convention: @os@ and @hs@ stay polymorphic, so the one definition closes a call
  with no out-parameters and one with two. libgit2's @checkedStatus@ is this.
-------------------------------------------------------------------------------}

namedCloser :: AutoOutputs os hs => ToHighLevel os (IO CInt) (IO hs)
namedCloser = checkedResult rejectNonZero

hsNamedCloserUnit :: Int -> Int -> IO ()
hsNamedCloserUnit = toHighLevel cAdd (autoInputs namedCloser)

hsNamedCloserPair :: Int -> IO (Int, Int)
hsNamedCloserPair = toHighLevel twoOutThenIn
                  $ output defaultOut
                  $ output defaultOut
                  $ autoInputs
                  $ namedCloser

-- | Every combinator takes the same integral widths. The result side is covered above;
-- these two pin that an argument and an out-parameter of the same C type convert the
-- same way, which is what 'HsBindgen.HighLevel.Defaults' promises and what a binding
-- author will assume after seeing one of the three work.
c_widthArgs :: CLong -> CULong -> CShort -> CSize -> IO CInt
c_widthArgs a b c d = pure (fromIntegral (a + fromIntegral b + fromIntegral c + fromIntegral d))

c_widthOuts :: Ptr CLong -> Ptr CULong -> IO ()
c_widthOuts p q = poke p 1 >> poke q 2

hsWidthArgs :: Int -> Word -> Int -> Int -> IO Int
hsWidthArgs = toHighLevel c_widthArgs auto

hsWidthOuts :: IO (Int, Word)
hsWidthOuts = toHighLevel c_widthOuts
            $ output defaultOut
            $ output defaultOut
            $ auto
