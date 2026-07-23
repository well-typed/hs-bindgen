{-# LANGUAGE OverloadedStrings #-}

-- | The struct marshaller vocabulary: 'struct' \/ 'marshalNested' \/ 'withStruct'
-- on the write side, 'UnmarshalStruct' \/ 'unmarshalNested' \/ 'unmarshalField' \/
-- 'unmarshalOptional' on the read side, and the wrapper adapters 'asArgument' \/
-- 'asArgumentC' \/ 'asOutput' \/ 'asResult'. One nested fixture with a
-- 'ByteString' field and a nullable label exercises the whole vocabulary through
-- a round-trip, checked on fixed values and over generated ones.
module Test.HsBindgen.HighLevel.Struct (tests) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar, CDouble (..), CInt, CSize)
import Foreign.Ptr (Ptr, nullPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, choose, elements, ioProperty,
                              listOf, testProperty, (===))

import HsBindgen.Runtime.Marshal (ReadRaw (..), StaticSize (..), WriteRaw (..),
                                  readRawByteOff, writeRawByteOff)
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

import HsBindgen.HighLevel (asArgument, asArgumentC, asOutput, asResult, input,
                            output, resultPure, toHighLevel)
import HsBindgen.HighLevel.Marshaller (MarshalStruct, UnmarshalStruct, at,
                                       marshalNested, marshalOptional,
                                       runUnmarshalStruct, scalar, struct,
                                       unmarshalField, unmarshalFieldPure,
                                       unmarshalNested, unmarshalOptional,
                                       withStruct, (>>>))
import HsBindgen.HighLevel.Marshaller.Utils (nullConst, useAsByteStringLenIn,
                                             withCStringIn)

{-------------------------------------------------------------------------------
  The fixture (mirroring what hs-bindgen generates)

    struct Inner { int x; int y; };                              // size 8,  align 4
    struct Outer { struct Inner pos; const char* name;
                   size_t name_len; const char* label;           // label nullable
                   double weight; };                             // size 40, align 8
-------------------------------------------------------------------------------}

data Inner = Inner !CInt !CInt

instance StaticSize Inner where
  staticSizeOf    _ = 8
  staticAlignment _ = 4
instance ReadRaw Inner where
  readRaw p = Inner <$> readRawByteOff p 0 <*> readRawByteOff p 4
instance WriteRaw Inner where
  writeRaw p (Inner x y) = writeRawByteOff p 0 x >> writeRawByteOff p 4 y

data Outer = Outer !Inner !(PtrConst CChar) !CSize !(PtrConst CChar) !CDouble

instance StaticSize Outer where
  staticSizeOf    _ = 40
  staticAlignment _ = 8
instance ReadRaw Outer where
  readRaw p = Outer
    <$> readRawByteOff p 0
    <*> readRawByteOff p 8
    <*> readRawByteOff p 16
    <*> readRawByteOff p 24
    <*> readRawByteOff p 32
instance WriteRaw Outer where
  writeRaw p (Outer pos nm len lbl w) = do
    writeRawByteOff p 0  pos
    writeRawByteOff p 8  nm
    writeRawByteOff p 16 len
    writeRawByteOff p 24 lbl
    writeRawByteOff p 32 w

data InnerHi = InnerHi { ix :: Int, iy :: Int }
  deriving stock (Eq, Show)

data OuterHi = OuterHi
  { oPos :: InnerHi, oName :: ByteString, oLabel :: Maybe String, oWeight :: Double }
  deriving stock (Eq, Show)

-- | Write side: 'marshalNested' for the sub-struct, 'useAsByteStringLenIn' for
-- the @(name, name_len)@ pair, 'marshalOptional' for the nullable label.
innerIn :: MarshalStruct InnerHi Inner
innerIn = struct Inner (at ix (scalar fromIntegral) >>> at iy (scalar fromIntegral))

outerIn :: MarshalStruct OuterHi Outer
outerIn = struct Outer
    ( at oPos    (marshalNested innerIn)
  >>> at oName   useAsByteStringLenIn
  >>> at oLabel  (marshalOptional ($ nullConst) withCStringIn)
  >>> at oWeight (scalar realToFrac) )

-- | Read side: 'unmarshalField' packs the @(name, name_len)@ pair,
-- 'unmarshalOptional' turns a NULL label into 'Nothing'.
innerOut :: UnmarshalStruct Inner InnerHi
innerOut = InnerHi
    <$> unmarshalFieldPure (\(Inner x _) -> x) fromIntegral
    <*> unmarshalFieldPure (\(Inner _ y) -> y) fromIntegral

outerOut :: UnmarshalStruct Outer OuterHi
outerOut = OuterHi
    <$> unmarshalNested   (\(Outer pos _ _ _ _) -> pos) innerOut
    <*> unmarshalField    (\(Outer _ p n _ _) -> (p, n)) packName
    <*> unmarshalOptional (\(Outer _ _ _ lbl _) -> PtrConst.unsafeToPtr lbl) peekCString
    <*> unmarshalFieldPure (\(Outer _ _ _ _ w) -> w) (\(CDouble d) -> d)
  where
    packName (p, n) = BS.packCStringLen (PtrConst.unsafeToPtr p, fromIntegral n)

{-------------------------------------------------------------------------------
  Stand-in low-level callables
-------------------------------------------------------------------------------}

-- | Reads @pos.x + pos.y + name_len@ back out, plus @100@ for a non-NULL label,
-- so the status reflects what was marshalled in.
c_takesOuter :: Ptr Outer -> IO CInt
c_takesOuter p = do
    x   <- readRawByteOff p 0  :: IO CInt
    y   <- readRawByteOff p 4  :: IO CInt
    len <- readRawByteOff p 16 :: IO CSize
    lbl <- readRawByteOff p 24 :: IO (PtrConst CChar)
    let labelled = if PtrConst.unsafeToPtr lbl == nullPtr then 0 else 100
    pure (x + y + fromIntegral len + labelled)

c_takesOuterConst :: PtrConst Outer -> IO CInt
c_takesOuterConst = c_takesOuter . PtrConst.unsafeToPtr

c_fillInner :: Ptr Inner -> IO CInt
c_fillInner p = writeRaw p (Inner 11 22) >> pure 0

c_makeInner :: IO Inner
c_makeInner = pure (Inner 7 8)

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

-- | Write Hi -> C into a zeroed slot, then read C -> Hi back, all inside the field
-- brackets so a @Just@ label still points at live memory.
roundTrip :: OuterHi -> IO OuterHi
roundTrip hi = withStruct outerIn hi (readRaw >=> runUnmarshalStruct outerOut)

hsTakesOuter :: OuterHi -> IO Int
hsTakesOuter =
    toHighLevel (input (asArgument outerIn) $ resultPure fromIntegral) c_takesOuter

hsTakesOuterConst :: OuterHi -> IO Int
hsTakesOuterConst =
    toHighLevel (input (asArgumentC outerIn) $ resultPure fromIntegral) c_takesOuterConst

hsFillInner :: IO (InnerHi, Int)
hsFillInner =
    toHighLevel (output (asOutput innerOut) $ resultPure fromIntegral) c_fillInner

hsMakeInner :: IO InnerHi
hsMakeInner = toHighLevel (asResult innerOut) c_makeInner

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

instance Arbitrary InnerHi where
  arbitrary = InnerHi <$> genI <*> genI
    where genI = choose (-100000, 100000)   -- within CInt

instance Arbitrary OuterHi where
  arbitrary = OuterHi <$> arbitrary <*> genName <*> genLabel <*> genWeight
    where
      genName   = BS.pack <$> arbitrary                          -- any bytes; NULs kept
      genLabel  = do present <- arbitrary
                     if present then Just <$> listOf (elements ['a'..'z'])
                                else pure Nothing
      genWeight = choose (-1.0e6, 1.0e6) :: Gen Double            -- finite; exact via CDouble

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HighLevel.Struct"
    [ testGroup "round-trip Hi -> C -> Hi"
        [ testCase "nesting, a ByteString field, and a Just label survive" $ do
            let hi = OuterHi (InnerHi 1 2) "hello" (Just "tag") 2.5
            roundTrip hi >>= (@?= hi)
        , testCase "a Nothing label marshals to NULL and reads back Nothing" $ do
            let hi = OuterHi (InnerHi 1 2) "hello" Nothing 2.5
            roundTrip hi >>= (@?= hi)
        , testProperty "preserves an arbitrary value" $ \hi ->
            ioProperty ((=== hi) <$> roundTrip hi)
        ]

    , testGroup "wrapper adapters"
        [ testCase "asArgument: C sees the nested fields and a present label" $
            hsTakesOuter (OuterHi (InnerHi 3 4) "hello" (Just "x") 2.5) >>= (@?= 112)
        , testCase "asArgument: a Nothing label reaches C as NULL" $
            hsTakesOuter (OuterHi (InnerHi 3 4) "hello" Nothing 2.5) >>= (@?= 12)
        , testCase "asArgumentC: the same struct served as a const T *" $
            hsTakesOuterConst (OuterHi (InnerHi 3 4) "hello" (Just "x") 2.5) >>= (@?= 112)
        , testCase "asOutput: read a struct back from an out-parameter" $
            hsFillInner >>= (@?= (InnerHi 11 22, 0))
        , testCase "asResult: read a struct returned by value" $
            hsMakeInner >>= (@?= InnerHi 7 8)
        ]
    ]
