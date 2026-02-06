{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.List.NonEmpty
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import qualified Text.Read
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)

{-| __C declaration:__ @enum first@

    __defined at:__ @types\/enums\/enums.h 4:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype First = First
  { unwrapFirst :: FC.CUInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize First where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw First where

  readRaw =
    \ptr0 ->
          pure First
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw First where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          First unwrapFirst2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapFirst2

deriving via HsBindgen.Runtime.Marshal.EquivStorable First instance F.Storable First

deriving via FC.CUInt instance Data.Primitive.Types.Prim First

instance HsBindgen.Runtime.CEnum.CEnum First where

  type CEnumZ First = FC.CUInt

  toCEnum = First

  fromCEnum = unwrapFirst

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "FIRST1")
                                                     , (1, Data.List.NonEmpty.singleton "FIRST2")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "First"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "First"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum First where

  minDeclaredValue = FIRST1

  maxDeclaredValue = FIRST2

instance Show First where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read First where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType First) "unwrapFirst")
         ) => GHC.Records.HasField "unwrapFirst" (Ptr.Ptr First) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFirst")

instance HsBindgen.Runtime.HasCField.HasCField First "unwrapFirst" where

  type CFieldType First "unwrapFirst" = FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FIRST1@

    __defined at:__ @types\/enums\/enums.h 5:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern FIRST1 :: First
pattern FIRST1 = First 0

{-| __C declaration:__ @FIRST2@

    __defined at:__ @types\/enums\/enums.h 6:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern FIRST2 :: First
pattern FIRST2 = First 1

{-| __C declaration:__ @enum second@

    __defined at:__ @types\/enums\/enums.h 9:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype Second = Second
  { unwrapSecond :: FC.CInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize Second where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Second where

  readRaw =
    \ptr0 ->
          pure Second
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw Second where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Second unwrapSecond2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapSecond2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Second instance F.Storable Second

deriving via FC.CInt instance Data.Primitive.Types.Prim Second

instance HsBindgen.Runtime.CEnum.CEnum Second where

  type CEnumZ Second = FC.CInt

  toCEnum = Second

  fromCEnum = unwrapSecond

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (-1, Data.List.NonEmpty.singleton "SECOND_A")
                                                     , (0, Data.List.NonEmpty.singleton "SECOND_B")
                                                     , (1, Data.List.NonEmpty.singleton "SECOND_C")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Second"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Second"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Second where

  minDeclaredValue = SECOND_A

  maxDeclaredValue = SECOND_C

instance Show Second where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read Second where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Second) "unwrapSecond")
         ) => GHC.Records.HasField "unwrapSecond" (Ptr.Ptr Second) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapSecond")

instance HsBindgen.Runtime.HasCField.HasCField Second "unwrapSecond" where

  type CFieldType Second "unwrapSecond" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @SECOND_A@

    __defined at:__ @types\/enums\/enums.h 10:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern SECOND_A :: Second
pattern SECOND_A = Second (-1)

{-| __C declaration:__ @SECOND_B@

    __defined at:__ @types\/enums\/enums.h 11:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern SECOND_B :: Second
pattern SECOND_B = Second 0

{-| __C declaration:__ @SECOND_C@

    __defined at:__ @types\/enums\/enums.h 12:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern SECOND_C :: Second
pattern SECOND_C = Second 1

{-| __C declaration:__ @enum same@

    __defined at:__ @types\/enums\/enums.h 15:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype Same = Same
  { unwrapSame :: FC.CUInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize Same where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Same where

  readRaw =
    \ptr0 ->
          pure Same
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw Same where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Same unwrapSame2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapSame2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Same instance F.Storable Same

deriving via FC.CUInt instance Data.Primitive.Types.Prim Same

instance HsBindgen.Runtime.CEnum.CEnum Same where

  type CEnumZ Same = FC.CUInt

  toCEnum = Same

  fromCEnum = unwrapSame

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(1, ("SAME_A" Data.List.NonEmpty.:| ["SAME_B"]))]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Same"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Same"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Same where

  minDeclaredValue = SAME_A

  maxDeclaredValue = SAME_A

instance Show Same where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read Same where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Same) "unwrapSame")
         ) => GHC.Records.HasField "unwrapSame" (Ptr.Ptr Same) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapSame")

instance HsBindgen.Runtime.HasCField.HasCField Same "unwrapSame" where

  type CFieldType Same "unwrapSame" = FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @SAME_A@

    __defined at:__ @types\/enums\/enums.h 16:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern SAME_A :: Same
pattern SAME_A = Same 1

{-| __C declaration:__ @SAME_B@

    __defined at:__ @types\/enums\/enums.h 17:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern SAME_B :: Same
pattern SAME_B = Same 1

{-| __C declaration:__ @enum nonseq@

    __defined at:__ @types\/enums\/enums.h 20:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype Nonseq = Nonseq
  { unwrapNonseq :: FC.CUInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize Nonseq where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Nonseq where

  readRaw =
    \ptr0 ->
          pure Nonseq
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw Nonseq where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Nonseq unwrapNonseq2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapNonseq2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Nonseq instance F.Storable Nonseq

deriving via FC.CUInt instance Data.Primitive.Types.Prim Nonseq

instance HsBindgen.Runtime.CEnum.CEnum Nonseq where

  type CEnumZ Nonseq = FC.CUInt

  toCEnum = Nonseq

  fromCEnum = unwrapNonseq

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (200, Data.List.NonEmpty.singleton "NONSEQ_A")
                                                     , (301, Data.List.NonEmpty.singleton "NONSEQ_B")
                                                     , (404, Data.List.NonEmpty.singleton "NONSEQ_C")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Nonseq"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Nonseq"

instance Show Nonseq where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read Nonseq where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Nonseq) "unwrapNonseq")
         ) => GHC.Records.HasField "unwrapNonseq" (Ptr.Ptr Nonseq) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapNonseq")

instance HsBindgen.Runtime.HasCField.HasCField Nonseq "unwrapNonseq" where

  type CFieldType Nonseq "unwrapNonseq" = FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @NONSEQ_A@

    __defined at:__ @types\/enums\/enums.h 21:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern NONSEQ_A :: Nonseq
pattern NONSEQ_A = Nonseq 200

{-| __C declaration:__ @NONSEQ_B@

    __defined at:__ @types\/enums\/enums.h 22:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern NONSEQ_B :: Nonseq
pattern NONSEQ_B = Nonseq 301

{-| __C declaration:__ @NONSEQ_C@

    __defined at:__ @types\/enums\/enums.h 23:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern NONSEQ_C :: Nonseq
pattern NONSEQ_C = Nonseq 404

{-| __C declaration:__ @enum packed@

    __defined at:__ @types\/enums\/enums.h 26:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype Packed = Packed
  { unwrapPacked :: FC.CUChar
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize Packed where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Packed where

  readRaw =
    \ptr0 ->
          pure Packed
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw Packed where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Packed unwrapPacked2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapPacked2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Packed instance F.Storable Packed

deriving via FC.CUChar instance Data.Primitive.Types.Prim Packed

instance HsBindgen.Runtime.CEnum.CEnum Packed where

  type CEnumZ Packed = FC.CUChar

  toCEnum = Packed

  fromCEnum = unwrapPacked

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "PACKED_A")
                                                     , (1, Data.List.NonEmpty.singleton "PACKED_B")
                                                     , (2, Data.List.NonEmpty.singleton "PACKED_C")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Packed"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Packed"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Packed where

  minDeclaredValue = PACKED_A

  maxDeclaredValue = PACKED_C

instance Show Packed where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read Packed where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Packed) "unwrapPacked")
         ) => GHC.Records.HasField "unwrapPacked" (Ptr.Ptr Packed) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPacked")

instance HsBindgen.Runtime.HasCField.HasCField Packed "unwrapPacked" where

  type CFieldType Packed "unwrapPacked" = FC.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @PACKED_A@

    __defined at:__ @types\/enums\/enums.h 27:5@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern PACKED_A :: Packed
pattern PACKED_A = Packed 0

{-| __C declaration:__ @PACKED_B@

    __defined at:__ @types\/enums\/enums.h 27:15@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern PACKED_B :: Packed
pattern PACKED_B = Packed 1

{-| __C declaration:__ @PACKED_C@

    __defined at:__ @types\/enums\/enums.h 27:25@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern PACKED_C :: Packed
pattern PACKED_C = Packed 2

{-| __C declaration:__ @enum enumA@

    __defined at:__ @types\/enums\/enums.h 30:9@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype EnumA = EnumA
  { unwrapEnumA :: FC.CUInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize EnumA where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw EnumA where

  readRaw =
    \ptr0 ->
          pure EnumA
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw EnumA where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumA unwrapEnumA2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapEnumA2

deriving via HsBindgen.Runtime.Marshal.EquivStorable EnumA instance F.Storable EnumA

deriving via FC.CUInt instance Data.Primitive.Types.Prim EnumA

instance HsBindgen.Runtime.CEnum.CEnum EnumA where

  type CEnumZ EnumA = FC.CUInt

  toCEnum = EnumA

  fromCEnum = unwrapEnumA

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "A_FOO")
                                                     , (1, Data.List.NonEmpty.singleton "A_BAR")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "EnumA"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "EnumA"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumA where

  minDeclaredValue = A_FOO

  maxDeclaredValue = A_BAR

instance Show EnumA where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read EnumA where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType EnumA) "unwrapEnumA")
         ) => GHC.Records.HasField "unwrapEnumA" (Ptr.Ptr EnumA) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapEnumA")

instance HsBindgen.Runtime.HasCField.HasCField EnumA "unwrapEnumA" where

  type CFieldType EnumA "unwrapEnumA" = FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A_FOO@

    __defined at:__ @types\/enums\/enums.h 30:16@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern A_FOO :: EnumA
pattern A_FOO = EnumA 0

{-| __C declaration:__ @A_BAR@

    __defined at:__ @types\/enums\/enums.h 30:23@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern A_BAR :: EnumA
pattern A_BAR = EnumA 1

{-| __C declaration:__ @enum enumB@

    __defined at:__ @types\/enums\/enums.h 32:14@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype EnumB = EnumB
  { unwrapEnumB :: FC.CUInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize EnumB where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw EnumB where

  readRaw =
    \ptr0 ->
          pure EnumB
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw EnumB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumB unwrapEnumB2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapEnumB2

deriving via HsBindgen.Runtime.Marshal.EquivStorable EnumB instance F.Storable EnumB

deriving via FC.CUInt instance Data.Primitive.Types.Prim EnumB

instance HsBindgen.Runtime.CEnum.CEnum EnumB where

  type CEnumZ EnumB = FC.CUInt

  toCEnum = EnumB

  fromCEnum = unwrapEnumB

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "B_FOO")
                                                     , (1, Data.List.NonEmpty.singleton "B_BAR")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "EnumB"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "EnumB"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumB where

  minDeclaredValue = B_FOO

  maxDeclaredValue = B_BAR

instance Show EnumB where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read EnumB where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType EnumB) "unwrapEnumB")
         ) => GHC.Records.HasField "unwrapEnumB" (Ptr.Ptr EnumB) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapEnumB")

instance HsBindgen.Runtime.HasCField.HasCField EnumB "unwrapEnumB" where

  type CFieldType EnumB "unwrapEnumB" = FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B_FOO@

    __defined at:__ @types\/enums\/enums.h 32:22@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern B_FOO :: EnumB
pattern B_FOO = EnumB 0

{-| __C declaration:__ @B_BAR@

    __defined at:__ @types\/enums\/enums.h 32:29@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern B_BAR :: EnumB
pattern B_BAR = EnumB 1

{-| __C declaration:__ @enum enumC@

    __defined at:__ @types\/enums\/enums.h 34:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype EnumC = EnumC
  { unwrapEnumC :: FC.CUInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize EnumC where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw EnumC where

  readRaw =
    \ptr0 ->
          pure EnumC
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw EnumC where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumC unwrapEnumC2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapEnumC2

deriving via HsBindgen.Runtime.Marshal.EquivStorable EnumC instance F.Storable EnumC

deriving via FC.CUInt instance Data.Primitive.Types.Prim EnumC

instance HsBindgen.Runtime.CEnum.CEnum EnumC where

  type CEnumZ EnumC = FC.CUInt

  toCEnum = EnumC

  fromCEnum = unwrapEnumC

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "C_FOO")
                                                     , (1, Data.List.NonEmpty.singleton "C_BAR")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "EnumC"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "EnumC"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumC where

  minDeclaredValue = C_FOO

  maxDeclaredValue = C_BAR

instance Show EnumC where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read EnumC where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType EnumC) "unwrapEnumC")
         ) => GHC.Records.HasField "unwrapEnumC" (Ptr.Ptr EnumC) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapEnumC")

instance HsBindgen.Runtime.HasCField.HasCField EnumC "unwrapEnumC" where

  type CFieldType EnumC "unwrapEnumC" = FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @C_FOO@

    __defined at:__ @types\/enums\/enums.h 34:14@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern C_FOO :: EnumC
pattern C_FOO = EnumC 0

{-| __C declaration:__ @C_BAR@

    __defined at:__ @types\/enums\/enums.h 34:21@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern C_BAR :: EnumC
pattern C_BAR = EnumC 1

{-| __C declaration:__ @enum enumD@

    __defined at:__ @types\/enums\/enums.h 37:6@

    __exported by:__ @types\/enums\/enums.h@
-}
newtype EnumD_t = EnumD_t
  { unwrapEnumD_t :: FC.CUInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize EnumD_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw EnumD_t where

  readRaw =
    \ptr0 ->
          pure EnumD_t
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw EnumD_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumD_t unwrapEnumD_t2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapEnumD_t2

deriving via HsBindgen.Runtime.Marshal.EquivStorable EnumD_t instance F.Storable EnumD_t

deriving via FC.CUInt instance Data.Primitive.Types.Prim EnumD_t

instance HsBindgen.Runtime.CEnum.CEnum EnumD_t where

  type CEnumZ EnumD_t = FC.CUInt

  toCEnum = EnumD_t

  fromCEnum = unwrapEnumD_t

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "D_FOO")
                                                     , (1, Data.List.NonEmpty.singleton "D_BAR")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "EnumD_t"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "EnumD_t"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumD_t where

  minDeclaredValue = D_FOO

  maxDeclaredValue = D_BAR

instance Show EnumD_t where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read EnumD_t where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType EnumD_t) "unwrapEnumD_t")
         ) => GHC.Records.HasField "unwrapEnumD_t" (Ptr.Ptr EnumD_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapEnumD_t")

instance HsBindgen.Runtime.HasCField.HasCField EnumD_t "unwrapEnumD_t" where

  type CFieldType EnumD_t "unwrapEnumD_t" = FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @D_FOO@

    __defined at:__ @types\/enums\/enums.h 37:14@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern D_FOO :: EnumD_t
pattern D_FOO = EnumD_t 0

{-| __C declaration:__ @D_BAR@

    __defined at:__ @types\/enums\/enums.h 37:21@

    __exported by:__ @types\/enums\/enums.h@
-}
pattern D_BAR :: EnumD_t
pattern D_BAR = EnumD_t 1
