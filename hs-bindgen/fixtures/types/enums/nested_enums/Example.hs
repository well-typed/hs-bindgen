{-# LANGUAGE DataKinds #-}
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
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import qualified Text.Read
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)

{-| __C declaration:__ @enum enumA@

    __defined at:__ @types\/enums\/nested_enums.h 2:14@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
newtype EnumA = EnumA
  { unwrapEnumA :: FC.CUInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

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
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "VALA_1")
                                                     , (1, Data.List.NonEmpty.singleton "VALA_2")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "EnumA"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "EnumA"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumA where

  minDeclaredValue = VALA_1

  maxDeclaredValue = VALA_2

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

{-| __C declaration:__ @VALA_1@

    __defined at:__ @types\/enums\/nested_enums.h 3:17@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
pattern VALA_1 :: EnumA
pattern VALA_1 = EnumA 0

{-| __C declaration:__ @VALA_2@

    __defined at:__ @types\/enums\/nested_enums.h 4:17@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
pattern VALA_2 :: EnumA
pattern VALA_2 = EnumA 1

{-| __C declaration:__ @struct exA@

    __defined at:__ @types\/enums\/nested_enums.h 1:8@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
data ExA = ExA
  { exA_fieldA1 :: EnumA
    {- ^ __C declaration:__ @fieldA1@

         __defined at:__ @types\/enums\/nested_enums.h 5:11@

         __exported by:__ @types\/enums\/nested_enums.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize ExA where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw ExA where

  readRaw =
    \ptr0 ->
          pure ExA
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"exA_fieldA1") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw ExA where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExA exA_fieldA12 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"exA_fieldA1") ptr0 exA_fieldA12

deriving via HsBindgen.Runtime.Marshal.EquivStorable ExA instance F.Storable ExA

instance Data.Primitive.Types.Prim ExA where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        ExA (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, ExA v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              ExA exA_fieldA14 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 exA_fieldA14 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        ExA (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, ExA v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              ExA exA_fieldA14 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 exA_fieldA14 s3

instance HsBindgen.Runtime.HasCField.HasCField ExA "exA_fieldA1" where

  type CFieldType ExA "exA_fieldA1" = EnumA

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExA) "exA_fieldA1")
         ) => GHC.Records.HasField "exA_fieldA1" (Ptr.Ptr ExA) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"exA_fieldA1")

{-| __C declaration:__ @enum \@exB_fieldB1@

    __defined at:__ @types\/enums\/nested_enums.h 9:9@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
newtype ExB_fieldB1 = ExB_fieldB1
  { unwrapExB_fieldB1 :: FC.CUInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize ExB_fieldB1 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw ExB_fieldB1 where

  readRaw =
    \ptr0 ->
          pure ExB_fieldB1
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw ExB_fieldB1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExB_fieldB1 unwrapExB_fieldB12 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapExB_fieldB12

deriving via HsBindgen.Runtime.Marshal.EquivStorable ExB_fieldB1 instance F.Storable ExB_fieldB1

deriving via FC.CUInt instance Data.Primitive.Types.Prim ExB_fieldB1

instance HsBindgen.Runtime.CEnum.CEnum ExB_fieldB1 where

  type CEnumZ ExB_fieldB1 = FC.CUInt

  toCEnum = ExB_fieldB1

  fromCEnum = unwrapExB_fieldB1

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "VALB_1")
                                                     , (1, Data.List.NonEmpty.singleton "VALB_2")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "ExB_fieldB1"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "ExB_fieldB1"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum ExB_fieldB1 where

  minDeclaredValue = VALB_1

  maxDeclaredValue = VALB_2

instance Show ExB_fieldB1 where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read ExB_fieldB1 where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExB_fieldB1) "unwrapExB_fieldB1")
         ) => GHC.Records.HasField "unwrapExB_fieldB1" (Ptr.Ptr ExB_fieldB1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapExB_fieldB1")

instance HsBindgen.Runtime.HasCField.HasCField ExB_fieldB1 "unwrapExB_fieldB1" where

  type CFieldType ExB_fieldB1 "unwrapExB_fieldB1" =
    FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @VALB_1@

    __defined at:__ @types\/enums\/nested_enums.h 10:17@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
pattern VALB_1 :: ExB_fieldB1
pattern VALB_1 = ExB_fieldB1 0

{-| __C declaration:__ @VALB_2@

    __defined at:__ @types\/enums\/nested_enums.h 11:17@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
pattern VALB_2 :: ExB_fieldB1
pattern VALB_2 = ExB_fieldB1 1

{-| __C declaration:__ @struct exB@

    __defined at:__ @types\/enums\/nested_enums.h 8:8@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
data ExB = ExB
  { exB_fieldB1 :: ExB_fieldB1
    {- ^ __C declaration:__ @fieldB1@

         __defined at:__ @types\/enums\/nested_enums.h 12:11@

         __exported by:__ @types\/enums\/nested_enums.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize ExB where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw ExB where

  readRaw =
    \ptr0 ->
          pure ExB
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"exB_fieldB1") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw ExB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExB exB_fieldB12 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"exB_fieldB1") ptr0 exB_fieldB12

deriving via HsBindgen.Runtime.Marshal.EquivStorable ExB instance F.Storable ExB

instance Data.Primitive.Types.Prim ExB where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        ExB (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, ExB v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              ExB exB_fieldB14 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 exB_fieldB14 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        ExB (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, ExB v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              ExB exB_fieldB14 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 exB_fieldB14 s3

instance HsBindgen.Runtime.HasCField.HasCField ExB "exB_fieldB1" where

  type CFieldType ExB "exB_fieldB1" = ExB_fieldB1

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExB) "exB_fieldB1")
         ) => GHC.Records.HasField "exB_fieldB1" (Ptr.Ptr ExB) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"exB_fieldB1")
