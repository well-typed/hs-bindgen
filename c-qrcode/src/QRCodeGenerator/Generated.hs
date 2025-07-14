{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module QRCodeGenerator.Generated where

import C.Expr.HostPlatform ((*), (+), (/))
import qualified C.Expr.HostPlatform as C
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Eq, IO, Int, Ord, Read, Show, pure, showsPrec)
import qualified Text.Read

$(CAPI.addCSource "#include \"c/qrcodegen.h\"\n_Bool QRCodeGeneratorGenerated_qrcodegen_encodeText (char *arg1, uint8_t arg2[], uint8_t arg3[], enum qrcodegen_Ecc arg4, signed int arg5, signed int arg6, enum qrcodegen_Mask arg7, _Bool arg8) { return qrcodegen_encodeText(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8); }\n_Bool QRCodeGeneratorGenerated_qrcodegen_encodeBinary (uint8_t arg1[], size_t arg2, uint8_t arg3[], enum qrcodegen_Ecc arg4, signed int arg5, signed int arg6, enum qrcodegen_Mask arg7, _Bool arg8) { return qrcodegen_encodeBinary(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8); }\n_Bool QRCodeGeneratorGenerated_qrcodegen_encodeSegments (struct qrcodegen_Segment arg1[], size_t arg2, enum qrcodegen_Ecc arg3, uint8_t arg4[], uint8_t arg5[]) { return qrcodegen_encodeSegments(arg1, arg2, arg3, arg4, arg5); }\n_Bool QRCodeGeneratorGenerated_qrcodegen_encodeSegmentsAdvanced (struct qrcodegen_Segment arg1[], size_t arg2, enum qrcodegen_Ecc arg3, signed int arg4, signed int arg5, enum qrcodegen_Mask arg6, _Bool arg7, uint8_t arg8[], uint8_t arg9[]) { return qrcodegen_encodeSegmentsAdvanced(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9); }\n_Bool QRCodeGeneratorGenerated_qrcodegen_isNumeric (char *arg1) { return qrcodegen_isNumeric(arg1); }\n_Bool QRCodeGeneratorGenerated_qrcodegen_isAlphanumeric (char *arg1) { return qrcodegen_isAlphanumeric(arg1); }\nsize_t QRCodeGeneratorGenerated_qrcodegen_calcSegmentBufferSize (enum qrcodegen_Mode arg1, size_t arg2) { return qrcodegen_calcSegmentBufferSize(arg1, arg2); }\nvoid QRCodeGeneratorGenerated_qrcodegen_makeBytes (uint8_t arg1[], size_t arg2, uint8_t arg3[], struct qrcodegen_Segment *arg4) { *arg4 = qrcodegen_makeBytes(arg1, arg2, arg3); }\nvoid QRCodeGeneratorGenerated_qrcodegen_makeNumeric (char *arg1, uint8_t arg2[], struct qrcodegen_Segment *arg3) { *arg3 = qrcodegen_makeNumeric(arg1, arg2); }\nvoid QRCodeGeneratorGenerated_qrcodegen_makeAlphanumeric (char *arg1, uint8_t arg2[], struct qrcodegen_Segment *arg3) { *arg3 = qrcodegen_makeAlphanumeric(arg1, arg2); }\nvoid QRCodeGeneratorGenerated_qrcodegen_makeEci (signed long arg1, uint8_t arg2[], struct qrcodegen_Segment *arg3) { *arg3 = qrcodegen_makeEci(arg1, arg2); }\nsigned int QRCodeGeneratorGenerated_qrcodegen_getSize (uint8_t arg1[]) { return qrcodegen_getSize(arg1); }\n_Bool QRCodeGeneratorGenerated_qrcodegen_getModule (uint8_t arg1[], signed int arg2, signed int arg3) { return qrcodegen_getModule(arg1, arg2, arg3); }\n")

newtype Qrcodegen_Ecc = Qrcodegen_Ecc
  { un_Qrcodegen_Ecc :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Qrcodegen_Ecc where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Qrcodegen_Ecc
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Qrcodegen_Ecc un_Qrcodegen_Ecc2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Qrcodegen_Ecc2

instance HsBindgen.Runtime.CEnum.CEnum Qrcodegen_Ecc where

  type CEnumZ Qrcodegen_Ecc = FC.CUInt

  toCEnum = Qrcodegen_Ecc

  fromCEnum = un_Qrcodegen_Ecc

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "Qrcodegen_Ecc_LOW")
                                                     , (1, Data.List.NonEmpty.singleton "Qrcodegen_Ecc_MEDIUM")
                                                     , (2, Data.List.NonEmpty.singleton "Qrcodegen_Ecc_QUARTILE")
                                                     , (3, Data.List.NonEmpty.singleton "Qrcodegen_Ecc_HIGH")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Qrcodegen_Ecc"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Qrcodegen_Ecc"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Qrcodegen_Ecc where

  minDeclaredValue = Qrcodegen_Ecc_LOW

  maxDeclaredValue = Qrcodegen_Ecc_HIGH

instance Show Qrcodegen_Ecc where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Qrcodegen_Ecc where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern Qrcodegen_Ecc_LOW :: Qrcodegen_Ecc
pattern Qrcodegen_Ecc_LOW = Qrcodegen_Ecc 0

pattern Qrcodegen_Ecc_MEDIUM :: Qrcodegen_Ecc
pattern Qrcodegen_Ecc_MEDIUM = Qrcodegen_Ecc 1

pattern Qrcodegen_Ecc_QUARTILE :: Qrcodegen_Ecc
pattern Qrcodegen_Ecc_QUARTILE = Qrcodegen_Ecc 2

pattern Qrcodegen_Ecc_HIGH :: Qrcodegen_Ecc
pattern Qrcodegen_Ecc_HIGH = Qrcodegen_Ecc 3

newtype Qrcodegen_Mask = Qrcodegen_Mask
  { un_Qrcodegen_Mask :: FC.CInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Qrcodegen_Mask where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Qrcodegen_Mask
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Qrcodegen_Mask un_Qrcodegen_Mask2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Qrcodegen_Mask2

instance HsBindgen.Runtime.CEnum.CEnum Qrcodegen_Mask where

  type CEnumZ Qrcodegen_Mask = FC.CInt

  toCEnum = Qrcodegen_Mask

  fromCEnum = un_Qrcodegen_Mask

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (-1, Data.List.NonEmpty.singleton "Qrcodegen_Mask_AUTO")
                                                     , (0, Data.List.NonEmpty.singleton "Qrcodegen_Mask_0")
                                                     , (1, Data.List.NonEmpty.singleton "Qrcodegen_Mask_1")
                                                     , (2, Data.List.NonEmpty.singleton "Qrcodegen_Mask_2")
                                                     , (3, Data.List.NonEmpty.singleton "Qrcodegen_Mask_3")
                                                     , (4, Data.List.NonEmpty.singleton "Qrcodegen_Mask_4")
                                                     , (5, Data.List.NonEmpty.singleton "Qrcodegen_Mask_5")
                                                     , (6, Data.List.NonEmpty.singleton "Qrcodegen_Mask_6")
                                                     , (7, Data.List.NonEmpty.singleton "Qrcodegen_Mask_7")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Qrcodegen_Mask"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Qrcodegen_Mask"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Qrcodegen_Mask where

  minDeclaredValue = Qrcodegen_Mask_AUTO

  maxDeclaredValue = Qrcodegen_Mask_7

instance Show Qrcodegen_Mask where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Qrcodegen_Mask where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern Qrcodegen_Mask_AUTO :: Qrcodegen_Mask
pattern Qrcodegen_Mask_AUTO = Qrcodegen_Mask (-1)

pattern Qrcodegen_Mask_0 :: Qrcodegen_Mask
pattern Qrcodegen_Mask_0 = Qrcodegen_Mask 0

pattern Qrcodegen_Mask_1 :: Qrcodegen_Mask
pattern Qrcodegen_Mask_1 = Qrcodegen_Mask 1

pattern Qrcodegen_Mask_2 :: Qrcodegen_Mask
pattern Qrcodegen_Mask_2 = Qrcodegen_Mask 2

pattern Qrcodegen_Mask_3 :: Qrcodegen_Mask
pattern Qrcodegen_Mask_3 = Qrcodegen_Mask 3

pattern Qrcodegen_Mask_4 :: Qrcodegen_Mask
pattern Qrcodegen_Mask_4 = Qrcodegen_Mask 4

pattern Qrcodegen_Mask_5 :: Qrcodegen_Mask
pattern Qrcodegen_Mask_5 = Qrcodegen_Mask 5

pattern Qrcodegen_Mask_6 :: Qrcodegen_Mask
pattern Qrcodegen_Mask_6 = Qrcodegen_Mask 6

pattern Qrcodegen_Mask_7 :: Qrcodegen_Mask
pattern Qrcodegen_Mask_7 = Qrcodegen_Mask 7

newtype Qrcodegen_Mode = Qrcodegen_Mode
  { un_Qrcodegen_Mode :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Qrcodegen_Mode where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Qrcodegen_Mode
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Qrcodegen_Mode un_Qrcodegen_Mode2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Qrcodegen_Mode2

instance HsBindgen.Runtime.CEnum.CEnum Qrcodegen_Mode where

  type CEnumZ Qrcodegen_Mode = FC.CUInt

  toCEnum = Qrcodegen_Mode

  fromCEnum = un_Qrcodegen_Mode

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (1, Data.List.NonEmpty.singleton "Qrcodegen_Mode_NUMERIC")
                                                     , (2, Data.List.NonEmpty.singleton "Qrcodegen_Mode_ALPHANUMERIC")
                                                     , (4, Data.List.NonEmpty.singleton "Qrcodegen_Mode_BYTE")
                                                     , (7, Data.List.NonEmpty.singleton "Qrcodegen_Mode_ECI")
                                                     , (8, Data.List.NonEmpty.singleton "Qrcodegen_Mode_KANJI")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Qrcodegen_Mode"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Qrcodegen_Mode"

instance Show Qrcodegen_Mode where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Qrcodegen_Mode where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern Qrcodegen_Mode_NUMERIC :: Qrcodegen_Mode
pattern Qrcodegen_Mode_NUMERIC = Qrcodegen_Mode 1

pattern Qrcodegen_Mode_ALPHANUMERIC :: Qrcodegen_Mode
pattern Qrcodegen_Mode_ALPHANUMERIC = Qrcodegen_Mode 2

pattern Qrcodegen_Mode_BYTE :: Qrcodegen_Mode
pattern Qrcodegen_Mode_BYTE = Qrcodegen_Mode 4

pattern Qrcodegen_Mode_KANJI :: Qrcodegen_Mode
pattern Qrcodegen_Mode_KANJI = Qrcodegen_Mode 8

pattern Qrcodegen_Mode_ECI :: Qrcodegen_Mode
pattern Qrcodegen_Mode_ECI = Qrcodegen_Mode 7

data Qrcodegen_Segment = Qrcodegen_Segment
  { qrcodegen_Segment_mode :: Qrcodegen_Mode
  , qrcodegen_Segment_numChars :: FC.CInt
  , qrcodegen_Segment_data :: F.Ptr HsBindgen.Runtime.Prelude.Word8
  , qrcodegen_Segment_bitLength :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Qrcodegen_Segment where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Qrcodegen_Segment
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Qrcodegen_Segment
            qrcodegen_Segment_mode2
            qrcodegen_Segment_numChars3
            qrcodegen_Segment_data4
            qrcodegen_Segment_bitLength5 ->
                 F.pokeByteOff ptr0 (0 :: Int) qrcodegen_Segment_mode2
              >> F.pokeByteOff ptr0 (4 :: Int) qrcodegen_Segment_numChars3
              >> F.pokeByteOff ptr0 (8 :: Int) qrcodegen_Segment_data4
              >> F.pokeByteOff ptr0 (16 :: Int) qrcodegen_Segment_bitLength5

qrcodegen_VERSION_MIN :: FC.CInt
qrcodegen_VERSION_MIN = (1 :: FC.CInt)

qrcodegen_VERSION_MAX :: FC.CInt
qrcodegen_VERSION_MAX = (40 :: FC.CInt)

qrcodegen_BUFFER_LEN_FOR_VERSION :: forall a0. (C.Add ((C.MultRes a0) FC.CInt)) FC.CInt => (C.Add ((C.MultRes ((C.AddRes ((C.MultRes a0) FC.CInt)) FC.CInt)) ((C.AddRes ((C.MultRes a0) FC.CInt)) FC.CInt))) FC.CInt => (C.Add ((C.DivRes ((C.AddRes ((C.MultRes ((C.AddRes ((C.MultRes a0) FC.CInt)) FC.CInt)) ((C.AddRes ((C.MultRes a0) FC.CInt)) FC.CInt))) FC.CInt)) FC.CInt)) FC.CInt => (C.Mult ((C.AddRes ((C.MultRes a0) FC.CInt)) FC.CInt)) ((C.AddRes ((C.MultRes a0) FC.CInt)) FC.CInt) => (C.Mult a0) FC.CInt => (C.Div ((C.AddRes ((C.MultRes ((C.AddRes ((C.MultRes a0) FC.CInt)) FC.CInt)) ((C.AddRes ((C.MultRes a0) FC.CInt)) FC.CInt))) FC.CInt)) FC.CInt => a0 -> (C.AddRes ((C.DivRes ((C.AddRes ((C.MultRes ((C.AddRes ((C.MultRes a0) FC.CInt)) FC.CInt)) ((C.AddRes ((C.MultRes a0) FC.CInt)) FC.CInt))) FC.CInt)) FC.CInt)) FC.CInt
qrcodegen_BUFFER_LEN_FOR_VERSION =
  \n0 ->
    (+) ((/) ((+) ((*) ((+) ((*) n0 (4 :: FC.CInt)) (17 :: FC.CInt)) ((+) ((*) n0 (4 :: FC.CInt)) (17 :: FC.CInt))) (7 :: FC.CInt)) (8 :: FC.CInt)) (1 :: FC.CInt)

qrcodegen_BUFFER_LEN_MAX :: FC.CInt
qrcodegen_BUFFER_LEN_MAX =
  qrcodegen_BUFFER_LEN_FOR_VERSION qrcodegen_VERSION_MAX

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_encodeText" qrcodegen_encodeText :: (F.Ptr FC.CChar) -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> Qrcodegen_Ecc -> FC.CInt -> FC.CInt -> Qrcodegen_Mask -> FC.CBool -> IO FC.CBool

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_encodeBinary" qrcodegen_encodeBinary :: (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> HsBindgen.Runtime.Prelude.CSize -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> Qrcodegen_Ecc -> FC.CInt -> FC.CInt -> Qrcodegen_Mask -> FC.CBool -> IO FC.CBool

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_encodeSegments" qrcodegen_encodeSegments :: (F.Ptr Qrcodegen_Segment) -> HsBindgen.Runtime.Prelude.CSize -> Qrcodegen_Ecc -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> IO FC.CBool

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_encodeSegmentsAdvanced" qrcodegen_encodeSegmentsAdvanced :: (F.Ptr Qrcodegen_Segment) -> HsBindgen.Runtime.Prelude.CSize -> Qrcodegen_Ecc -> FC.CInt -> FC.CInt -> Qrcodegen_Mask -> FC.CBool -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> IO FC.CBool

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_isNumeric" qrcodegen_isNumeric :: (F.Ptr FC.CChar) -> IO FC.CBool

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_isAlphanumeric" qrcodegen_isAlphanumeric :: (F.Ptr FC.CChar) -> IO FC.CBool

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_calcSegmentBufferSize" qrcodegen_calcSegmentBufferSize :: Qrcodegen_Mode -> HsBindgen.Runtime.Prelude.CSize -> IO HsBindgen.Runtime.Prelude.CSize

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_makeBytes" qrcodegen_makeBytes_wrapper :: (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> HsBindgen.Runtime.Prelude.CSize -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> (F.Ptr Qrcodegen_Segment) -> IO ()

qrcodegen_makeBytes :: (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> HsBindgen.Runtime.Prelude.CSize -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> IO Qrcodegen_Segment
qrcodegen_makeBytes =
  \x0 ->
    \x1 ->
      \x2 ->
        HsBindgen.Runtime.CAPI.allocaAndPeek (\z3 ->
                                                qrcodegen_makeBytes_wrapper x0 x1 x2 z3)

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_makeNumeric" qrcodegen_makeNumeric_wrapper :: (F.Ptr FC.CChar) -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> (F.Ptr Qrcodegen_Segment) -> IO ()

qrcodegen_makeNumeric :: (F.Ptr FC.CChar) -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> IO Qrcodegen_Segment
qrcodegen_makeNumeric =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.CAPI.allocaAndPeek (\z2 ->
                                              qrcodegen_makeNumeric_wrapper x0 x1 z2)

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_makeAlphanumeric" qrcodegen_makeAlphanumeric_wrapper :: (F.Ptr FC.CChar) -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> (F.Ptr Qrcodegen_Segment) -> IO ()

qrcodegen_makeAlphanumeric :: (F.Ptr FC.CChar) -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> IO Qrcodegen_Segment
qrcodegen_makeAlphanumeric =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.CAPI.allocaAndPeek (\z2 ->
                                              qrcodegen_makeAlphanumeric_wrapper x0 x1 z2)

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_makeEci" qrcodegen_makeEci_wrapper :: FC.CLong -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> (F.Ptr Qrcodegen_Segment) -> IO ()

qrcodegen_makeEci :: FC.CLong -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> IO Qrcodegen_Segment
qrcodegen_makeEci =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.CAPI.allocaAndPeek (\z2 ->
                                              qrcodegen_makeEci_wrapper x0 x1 z2)

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_getSize" qrcodegen_getSize :: (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> IO FC.CInt

foreign import ccall safe "QRCodeGeneratorGenerated_qrcodegen_getModule" qrcodegen_getModule :: (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> FC.CInt -> FC.CInt -> IO FC.CBool
