{-# LANGUAGE OverloadedStrings #-}

-- | Smoke tests for the @HighLevel@ design.
--
-- Exercises the worked examples from @HIGH_LEVEL_DESIGN.md@ §2 against
-- @libc@, plus mixed-slot signatures, nullability, and arbitrary
-- arity. Compile + link is the strongest test of the elaboration.
module Test.HsBindgen.Runtime.HighLevel.Smoke (tests) where

import Data.ByteString (ByteString)
import Foreign.C.ConstPtr (ConstPtr (..))
import Foreign.C.Types (CBool (..), CChar (..), CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import Foreign.Ptr qualified as Ptr
import Foreign.Storable (peek, poke, pokeElemOff)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.Runtime.HighLevel.Prelude qualified as HL
import HsBindgen.Runtime.HighLevel.ToC (CSlot, ToC (..))
import HsBindgen.Runtime.IncompleteArray (IncompleteArray)
import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

{-------------------------------------------------------------------------------
  Foreign imports (against libc)
-------------------------------------------------------------------------------}

foreign import ccall unsafe "string.h strlen"
  c_strlen :: PtrConst CChar -> IO CSize

foreign import ccall unsafe "string.h memcmp"
  c_memcmp :: PtrConst CChar -> PtrConst CChar -> CSize -> IO CInt

foreign import ccall unsafe "string.h strncmp"
  c_strncmp :: PtrConst CChar -> PtrConst CChar -> CSize -> IO CInt

{-------------------------------------------------------------------------------
  High-level wrappers via hl
-------------------------------------------------------------------------------}

-- | §2.1: single-arg, single-slot, primitive result.
hsStrlen :: String -> IO Int
hsStrlen = HL.hl c_strlen

-- | §2.2: multi-arg, all single-slot (two 'String' + identity 'CSize').
hsMemcmpStrings :: String -> String -> CSize -> IO Int
hsMemcmpStrings = HL.hl c_memcmp

-- | Mixed slot counts in one signature: 'String' (1 slot) + 'ByteString'
-- (2 slots, via 'Spread') in a single recursive 'HighLevel' elaboration.
hsStrncmpMixed :: String -> ByteString -> IO Int
hsStrncmpMixed = HL.hl c_strncmp

{-------------------------------------------------------------------------------
  withOut around a custom out-parameter helper
-------------------------------------------------------------------------------}

-- | Writes 42 into the out parameter, returns 0 as status.
sampleOut :: Ptr CInt -> IO CInt
sampleOut p = do
  poke p 42
  pure 0

hsSampleOut :: IO Int
hsSampleOut = do
  (cval, _status) <- HL.withOut sampleOut
  pure (fromIntegral (cval :: CInt))

{-------------------------------------------------------------------------------
  withBuf around a custom buffer-output helper
-------------------------------------------------------------------------------}

-- | Writes @[0x41, 0x42, 0x43]@ ("ABC") into the buffer if there's
-- capacity, writes the actual length, returns 0. On insufficient
-- capacity writes 0 and returns -1.
sampleBuf :: Ptr CChar -> Ptr CSize -> IO CInt
sampleBuf pBuf pLen = do
  cap <- peek pLen
  if cap >= 3
    then do
      pokeElemOff pBuf 0 (CChar 0x41)
      pokeElemOff pBuf 1 (CChar 0x42)
      pokeElemOff pBuf 2 (CChar 0x43)
      poke pLen 3
      pure 0
    else do
      poke pLen 0
      pure (-1)

hsSampleBuf :: Int -> IO (IncompleteArray CChar, CInt)
hsSampleBuf cap = HL.withBuf cap sampleBuf

{-------------------------------------------------------------------------------
  Maybe (nullability) tests
-------------------------------------------------------------------------------}

-- | Returns 0 if the pointer is NULL, 1 otherwise.
sampleNullable :: PtrConst CChar -> IO CInt
sampleNullable p
  | PtrConst.unsafeToPtr p == Ptr.nullPtr = pure 0
  | otherwise                             = pure 1

hsCheckPresent :: Maybe String -> IO Int
hsCheckPresent = HL.hl sampleNullable

{-------------------------------------------------------------------------------
  High-arity wrapper + idiom-2 (domain newtype) demonstration
-------------------------------------------------------------------------------}

-- | Fake low-level function; we only check that 'hl' elaborates the wrapper.
fake5 :: PtrConst CChar -> CInt -> CBool -> CChar -> CSize -> IO CInt
fake5 _ _ _ _ _ = pure 0

-- | Idiom 2: a domain newtype around 'Int' with an explicit C width.
newtype Iters = Iters Int
  deriving stock (Eq, Show)

instance ToC Iters where
  type CSlot Iters = CInt
  withC (Iters n) k = k (fromIntegral n)

-- | 5-arg wrapper; no per-arity instance needed thanks to structural
-- recursion in 'HighLevel'.
hsFiveArg :: String -> Iters -> Bool -> CChar -> CSize -> IO Int
hsFiveArg = HL.hl fake5

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HighLevel.Smoke"
  [ testCase "hl strlen on \"hello\"" $ do
      n <- hsStrlen "hello"
      n @?= 5

  , testCase "hl strlen on \"\"" $ do
      n <- hsStrlen ""
      n @?= 0

  , testCase "hl strlen on 11 bytes" $ do
      n <- hsStrlen "hello world"
      n @?= 11

  , testCase "hl memcmp equal prefix returns 0" $ do
      r <- hsMemcmpStrings "abcdef" "abcxyz" 3
      r @?= 0

  , testCase "hl memcmp inequal returns nonzero" $ do
      r <- hsMemcmpStrings "abc" "abd" 3
      (r /= 0) @?= True

  , testCase "withOut peeks the value written by C" $ do
      v <- hsSampleOut
      v @?= 42

  , testCase "withBuf peeks buffer and length written by C" $ do
      (arr, status) <- hsSampleBuf 16
      status @?= 0
      IA.toList arr @?= [CChar 0x41, CChar 0x42, CChar 0x43]

  , testCase "withBuf reports 0-length on insufficient capacity" $ do
      (arr, status) <- hsSampleBuf 1
      status @?= (-1)
      IA.toList arr @?= []

  , testCase "Maybe String Nothing marshals to NULL" $ do
      r <- hsCheckPresent Nothing
      r @?= 0

  , testCase "Maybe String Just marshals to non-NULL" $ do
      r <- hsCheckPresent (Just "hello")
      r @?= 1

  , testCase "5-arg wrapper compiles + runs (no per-arity instance)" $ do
      r <- hsFiveArg "ignored" (Iters 0) False (CChar 0) 0
      r @?= 0

    -- Mixed-slot: String (1 slot) + ByteString (2 slots) in one signature.

  , testCase "hl strncmp [String + ByteString] equal prefix returns 0" $ do
      r <- hsStrncmpMixed "hello" "hello"
      r @?= 0

  , testCase "hl strncmp [String + ByteString] differing returns nonzero" $ do
      r <- hsStrncmpMixed "hello" "abcde"
      (r /= 0) @?= True

  , testCase "hl strncmp [String + ByteString] empty ByteString returns 0" $ do
      -- strncmp(_, _, 0) is always 0.
      r <- hsStrncmpMixed "anything" ""
      r @?= 0

  , testCase "hl strncmp [String + ByteString] prefix match" $ do
      -- First 5 chars of "hello world" are "hello"; match.
      r <- hsStrncmpMixed "hello world" "hello"
      r @?= 0
  ]
