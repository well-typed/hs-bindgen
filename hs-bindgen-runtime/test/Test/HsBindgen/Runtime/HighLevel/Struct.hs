{-# LANGUAGE OverloadedStrings #-}

-- | Behavioural tests for the struct marshaller vocabulary: 'struct' \/ 'marshalNested' \/
-- 'withStruct' on the write side, 'UnmarshalStruct' \/ 'unmarshalNested' \/ 'unmarshalField' \/ 'unmarshalOptional'
-- on the read side, and the three wrapper adapters ('asArgument' \/ 'asOutput' \/
-- 'asResult'). One fixture nests a sub-struct, carries a 'ByteString' field and a
-- nullable label, so a single round-trip exercises the whole vocabulary.
module Test.HsBindgen.Runtime.HighLevel.Struct (tests) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar, CDouble (..), CInt, CSize)
import Foreign.Ptr (Ptr, nullPtr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.Runtime.HighLevel (asArgument, asOutput, asResult, input,
                                    output, resultPure, toHighLevel)
import HsBindgen.Runtime.HighLevel.Marshaller (MarshalStruct, UnmarshalStruct,
                                               at, marshalNested,
                                               marshalOptional,
                                               runUnmarshalStruct, scalar,
                                               struct, unmarshalField,
                                               unmarshalFieldPure,
                                               unmarshalNested,
                                               unmarshalOptional, withStruct,
                                               (>>>))
import HsBindgen.Runtime.HighLevel.Marshaller.Utils (useAsByteStringLenIn,
                                                     withCStringIn)
import HsBindgen.Runtime.Marshal (ReadRaw (..), StaticSize (..), WriteRaw (..),
                                  readRawByteOff, writeRawByteOff)
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

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

nullName :: PtrConst CChar
nullName = PtrConst.unsafeFromPtr nullPtr

-- | Write marshallers: 'marshalNested' for the sub-struct, 'useAsByteStringLenIn' for the
-- @(name, name_len)@ pair, 'marshalOptional' for the nullable label.
innerIn :: MarshalStruct InnerHi Inner
innerIn = struct Inner (at ix (scalar fromIntegral) >>> at iy (scalar fromIntegral))

outerIn :: MarshalStruct OuterHi Outer
outerIn = struct Outer
  ( at oPos    (marshalNested innerIn)
  >>> at oName   useAsByteStringLenIn
  >>> at oLabel  (marshalOptional ($ nullName) withCStringIn)
  >>> at oWeight (scalar realToFrac) )

-- | Read marshallers: 'unmarshalField' packs the @(name, name_len)@ pair, 'unmarshalOptional' turns a
-- NULL label into 'Nothing'.
innerOut :: UnmarshalStruct Inner InnerHi
innerOut = InnerHi
  <$> unmarshalFieldPure (\(Inner x _) -> x) fromIntegral
  <*> unmarshalFieldPure (\(Inner _ y) -> y) fromIntegral

outerOut :: UnmarshalStruct Outer OuterHi
outerOut = OuterHi
  <$> unmarshalNested  (\(Outer pos _ _ _ _) -> pos) innerOut
  <*> unmarshalField      (\(Outer _ p n _ _) -> (p, n)) packName
  <*> unmarshalOptional (\(Outer _ _ _ lbl _) -> PtrConst.unsafeToPtr lbl) peekCString
  <*> unmarshalFieldPure  (\(Outer _ _ _ _ w) -> w) (\(CDouble d) -> d)
  where
    packName (p, n) = BS.packCStringLen (PtrConst.unsafeToPtr p, fromIntegral n)

{-------------------------------------------------------------------------------
  Stand-in low-level callables
-------------------------------------------------------------------------------}

-- | Reads pos.x + pos.y + name_len back out, plus 100 if the label is non-NULL, so
-- the wrapper's status reflects what was marshalled in.
c_takesOuter :: Ptr Outer -> IO CInt
c_takesOuter p = do
  x   <- readRawByteOff p 0  :: IO CInt
  y   <- readRawByteOff p 4  :: IO CInt
  len <- readRawByteOff p 16 :: IO CSize
  lbl <- readRawByteOff p 24 :: IO (PtrConst CChar)
  let labelled = if PtrConst.unsafeToPtr lbl == nullPtr then 0 else 100
  pure (x + y + fromIntegral len + labelled)

-- | Fills the out-parameter with an 'Inner'.
c_fillInner :: Ptr Inner -> IO CInt
c_fillInner p = writeRaw p (Inner 11 22) >> pure 0

-- | Returns an 'Inner' by value.
c_makeInner :: IO Inner
c_makeInner = pure (Inner 7 8)

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

-- | Write Hi -> C into a zeroed slot, then read C -> Hi back, all inside the field
-- brackets so a @Just@ label still points at live memory.
roundTrip :: OuterHi -> IO OuterHi
roundTrip hi = withStruct outerIn hi (readRaw >=> runUnmarshalStruct outerOut)

-- | A struct as a by-value argument, via 'input' \/ 'asArgument'.
hsTakesOuter :: OuterHi -> IO Int
hsTakesOuter =
  toHighLevel (input (asArgument outerIn) $ resultPure fromIntegral) c_takesOuter

-- | A struct out-parameter, via 'output' \/ 'asOutput'.
hsFillInner :: IO (InnerHi, Int)
hsFillInner =
  toHighLevel (output (asOutput innerOut) $ resultPure fromIntegral) c_fillInner

-- | A struct by-value return, via 'asResult'.
hsMakeInner :: IO InnerHi
hsMakeInner = toHighLevel (asResult innerOut) c_makeInner

tests :: TestTree
tests = testGroup "HighLevel.Struct"
  [ testCase "round-trip: nesting, a ByteString field, and a Just label survive Hi -> C -> Hi" $ do
      let hi = OuterHi (InnerHi 1 2) "hello" (Just "tag") 2.5
      hi' <- roundTrip hi
      hi' @?= hi

  , testCase "round-trip: a Nothing label marshals to NULL and reads back Nothing" $ do
      let hi = OuterHi (InnerHi 1 2) "hello" Nothing 2.5
      hi' <- roundTrip hi
      hi' @?= hi

  , testCase "asArgument: C sees the nested fields and a present label" $ do
      status <- hsTakesOuter (OuterHi (InnerHi 3 4) "hello" (Just "x") 2.5)
      status @?= 112                    -- pos.x 3 + pos.y 4 + name_len 5 + label 100

  , testCase "asArgument: a Nothing label reaches C as NULL" $ do
      status <- hsTakesOuter (OuterHi (InnerHi 3 4) "hello" Nothing 2.5)
      status @?= 12                     -- pos.x 3 + pos.y 4 + name_len 5 + no label

  , testCase "asOutput: read a struct back from an out-parameter" $ do
      (inner, status) <- hsFillInner
      inner  @?= InnerHi 11 22
      status @?= 0

  , testCase "asResult: read a struct returned by value" $ do
      inner <- hsMakeInner
      inner @?= InnerHi 7 8
  ]
