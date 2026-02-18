{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @enum enumA@

    __defined at:__ @types\/enums\/nested_enums.h 2:14@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
newtype EnumA = EnumA
  { unwrapEnumA :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize EnumA where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw EnumA where

  readRaw =
    \ptr0 ->
          pure EnumA
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw EnumA where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumA unwrapEnumA2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapEnumA2

deriving via Marshal.EquivStorable EnumA instance RIP.Storable EnumA

deriving via RIP.CUInt instance RIP.Prim EnumA

instance CEnum.CEnum EnumA where

  type CEnumZ EnumA = RIP.CUInt

  toCEnum = EnumA

  fromCEnum = unwrapEnumA

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "VALA_1"), (1, RIP.singleton "VALA_2")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "EnumA"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "EnumA"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum EnumA where

  minDeclaredValue = VALA_1

  maxDeclaredValue = VALA_2

instance Show EnumA where

  showsPrec = CEnum.shows

instance Read EnumA where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapEnumA" (RIP.Ptr EnumA) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapEnumA")

instance HasCField.HasCField EnumA "unwrapEnumA" where

  type CFieldType EnumA "unwrapEnumA" = RIP.CUInt

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize ExA where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw ExA where

  readRaw =
    \ptr0 ->
          pure ExA
      <*> HasCField.readRaw (RIP.Proxy @"exA_fieldA1") ptr0

instance Marshal.WriteRaw ExA where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExA exA_fieldA12 ->
            HasCField.writeRaw (RIP.Proxy @"exA_fieldA1") ptr0 exA_fieldA12

deriving via Marshal.EquivStorable ExA instance RIP.Storable ExA

instance HasCField.HasCField ExA "exA_fieldA1" where

  type CFieldType ExA "exA_fieldA1" = EnumA

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) EnumA
         ) => RIP.HasField "exA_fieldA1" (RIP.Ptr ExA) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"exA_fieldA1")

{-| __C declaration:__ @enum \@exB_fieldB1@

    __defined at:__ @types\/enums\/nested_enums.h 9:9@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
newtype ExB_fieldB1 = ExB_fieldB1
  { unwrapExB_fieldB1 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize ExB_fieldB1 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw ExB_fieldB1 where

  readRaw =
    \ptr0 ->
          pure ExB_fieldB1
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw ExB_fieldB1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExB_fieldB1 unwrapExB_fieldB12 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapExB_fieldB12

deriving via Marshal.EquivStorable ExB_fieldB1 instance RIP.Storable ExB_fieldB1

deriving via RIP.CUInt instance RIP.Prim ExB_fieldB1

instance CEnum.CEnum ExB_fieldB1 where

  type CEnumZ ExB_fieldB1 = RIP.CUInt

  toCEnum = ExB_fieldB1

  fromCEnum = unwrapExB_fieldB1

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "VALB_1"), (1, RIP.singleton "VALB_2")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "ExB_fieldB1"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "ExB_fieldB1"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum ExB_fieldB1 where

  minDeclaredValue = VALB_1

  maxDeclaredValue = VALB_2

instance Show ExB_fieldB1 where

  showsPrec = CEnum.shows

instance Read ExB_fieldB1 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapExB_fieldB1" (RIP.Ptr ExB_fieldB1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapExB_fieldB1")

instance HasCField.HasCField ExB_fieldB1 "unwrapExB_fieldB1" where

  type CFieldType ExB_fieldB1 "unwrapExB_fieldB1" =
    RIP.CUInt

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize ExB where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw ExB where

  readRaw =
    \ptr0 ->
          pure ExB
      <*> HasCField.readRaw (RIP.Proxy @"exB_fieldB1") ptr0

instance Marshal.WriteRaw ExB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExB exB_fieldB12 ->
            HasCField.writeRaw (RIP.Proxy @"exB_fieldB1") ptr0 exB_fieldB12

deriving via Marshal.EquivStorable ExB instance RIP.Storable ExB

instance HasCField.HasCField ExB "exB_fieldB1" where

  type CFieldType ExB "exB_fieldB1" = ExB_fieldB1

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) ExB_fieldB1
         ) => RIP.HasField "exB_fieldB1" (RIP.Ptr ExB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"exB_fieldB1")
