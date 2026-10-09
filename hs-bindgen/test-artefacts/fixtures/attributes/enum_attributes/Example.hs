{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Example
    ( Example.E0(..)
    , pattern Example.E0_A
    , pattern Example.E0_B
    , Example.E1(..)
    , pattern Example.E1_A
    , pattern Example.E1_B
    , Example.E2(..)
    , pattern Example.E2_A
    , pattern Example.E2_B
    , Example.E3(..)
    , pattern Example.E3_A
    , pattern Example.E3_B
    , Example.E4(..)
    , pattern Example.E4_A
    , pattern Example.E4_B
    , Example.E5(..)
    , pattern Example.E5_A
    , pattern Example.E5_B
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, type (~))

{-| __C declaration:__ @enum E0@

    __defined at:__ @attributes\/enum_attributes.h 13:38@

    __exported by:__ @attributes\/enum_attributes.h@
-}
newtype E0 = E0
  { unwrapE0 :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize E0 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E0 where

  readRaw =
    \ptr0 ->
          pure E0
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E0 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E0 unwrapE02 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE02

deriving via Marshal.EquivStorable E0 instance BG.Storable E0

deriving via BG.CUInt instance BG.Prim E0

instance CEnum.CEnum E0 where

  type CEnumZ E0 = BG.CUInt

  toCEnum = E0

  fromCEnum = BG.getField @"unwrapE0"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(1, BG.singleton "E0_A"), (2, BG.singleton "E0_B")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E0"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E0"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E0 where

  minDeclaredValue = E0_A

  maxDeclaredValue = E0_B

instance Show E0 where

  showsPrec = CEnum.shows

instance Read E0 where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrapE0" E0 ty where

  hasField =
    \x0 ->
      (\y1 ->
         E0 {unwrapE0 = y1}, BG.getField @"unwrapE0" x0)

instance (ty ~ BG.CUInt) => BG.HasField "unwrapE0" (BG.Ptr E0) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE0")

instance HasCField.HasCField E0 "unwrapE0" where

  type CFieldType E0 "unwrapE0" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E0_A@

    __defined at:__ @attributes\/enum_attributes.h 13:43@

    __exported by:__ @attributes\/enum_attributes.h@
-}
pattern E0_A :: E0
pattern E0_A = E0 1

{-| __C declaration:__ @E0_B@

    __defined at:__ @attributes\/enum_attributes.h 13:53@

    __exported by:__ @attributes\/enum_attributes.h@
-}
pattern E0_B :: E0
pattern E0_B = E0 2

{-| __C declaration:__ @enum E1@

    __defined at:__ @attributes\/enum_attributes.h 14:38@

    __exported by:__ @attributes\/enum_attributes.h@
-}
newtype E1 = E1
  { unwrapE1 :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize E1 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E1 where

  readRaw =
    \ptr0 ->
          pure E1
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E1 unwrapE12 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE12

deriving via Marshal.EquivStorable E1 instance BG.Storable E1

deriving via BG.CUInt instance BG.Prim E1

instance CEnum.CEnum E1 where

  type CEnumZ E1 = BG.CUInt

  toCEnum = E1

  fromCEnum = BG.getField @"unwrapE1"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(1, BG.singleton "E1_A"), (2, BG.singleton "E1_B")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E1"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E1"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E1 where

  minDeclaredValue = E1_A

  maxDeclaredValue = E1_B

instance Show E1 where

  showsPrec = CEnum.shows

instance Read E1 where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrapE1" E1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         E1 {unwrapE1 = y1}, BG.getField @"unwrapE1" x0)

instance (ty ~ BG.CUInt) => BG.HasField "unwrapE1" (BG.Ptr E1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE1")

instance HasCField.HasCField E1 "unwrapE1" where

  type CFieldType E1 "unwrapE1" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E1_A@

    __defined at:__ @attributes\/enum_attributes.h 14:43@

    __exported by:__ @attributes\/enum_attributes.h@
-}
pattern E1_A :: E1
pattern E1_A = E1 1

{-| __C declaration:__ @E1_B@

    __defined at:__ @attributes\/enum_attributes.h 14:53@

    __exported by:__ @attributes\/enum_attributes.h@
-}
pattern E1_B :: E1
pattern E1_B = E1 2

{-| __C declaration:__ @enum E2@

    __defined at:__ @attributes\/enum_attributes.h 17:52@

    __exported by:__ @attributes\/enum_attributes.h@
-}
newtype E2 = E2
  { unwrapE2 :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize E2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E2 where

  readRaw =
    \ptr0 ->
          pure E2
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E2 unwrapE22 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE22

deriving via Marshal.EquivStorable E2 instance BG.Storable E2

deriving via BG.CUInt instance BG.Prim E2

instance CEnum.CEnum E2 where

  type CEnumZ E2 = BG.CUInt

  toCEnum = E2

  fromCEnum = BG.getField @"unwrapE2"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "E2_A"), (1, BG.singleton "E2_B")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E2"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E2"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E2 where

  minDeclaredValue = E2_A

  maxDeclaredValue = E2_B

instance Show E2 where

  showsPrec = CEnum.shows

instance Read E2 where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrapE2" E2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         E2 {unwrapE2 = y1}, BG.getField @"unwrapE2" x0)

instance (ty ~ BG.CUInt) => BG.HasField "unwrapE2" (BG.Ptr E2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE2")

instance HasCField.HasCField E2 "unwrapE2" where

  type CFieldType E2 "unwrapE2" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E2_A@

    __defined at:__ @attributes\/enum_attributes.h 17:57@

    __exported by:__ @attributes\/enum_attributes.h@
-}
pattern E2_A :: E2
pattern E2_A = E2 0

{-| __C declaration:__ @E2_B@

    __defined at:__ @attributes\/enum_attributes.h 17:63@

    __exported by:__ @attributes\/enum_attributes.h@
-}
pattern E2_B :: E2
pattern E2_B = E2 1

{-| __C declaration:__ @enum E3@

    __defined at:__ @attributes\/enum_attributes.h 18:52@

    __exported by:__ @attributes\/enum_attributes.h@
-}
newtype E3 = E3
  { unwrapE3 :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize E3 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E3 where

  readRaw =
    \ptr0 ->
          pure E3
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E3 unwrapE32 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE32

deriving via Marshal.EquivStorable E3 instance BG.Storable E3

deriving via BG.CUInt instance BG.Prim E3

instance CEnum.CEnum E3 where

  type CEnumZ E3 = BG.CUInt

  toCEnum = E3

  fromCEnum = BG.getField @"unwrapE3"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "E3_A"), (1, BG.singleton "E3_B")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E3"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E3"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E3 where

  minDeclaredValue = E3_A

  maxDeclaredValue = E3_B

instance Show E3 where

  showsPrec = CEnum.shows

instance Read E3 where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrapE3" E3 ty where

  hasField =
    \x0 ->
      (\y1 ->
         E3 {unwrapE3 = y1}, BG.getField @"unwrapE3" x0)

instance (ty ~ BG.CUInt) => BG.HasField "unwrapE3" (BG.Ptr E3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE3")

instance HasCField.HasCField E3 "unwrapE3" where

  type CFieldType E3 "unwrapE3" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E3_A@

    __defined at:__ @attributes\/enum_attributes.h 18:57@

    __exported by:__ @attributes\/enum_attributes.h@
-}
pattern E3_A :: E3
pattern E3_A = E3 0

{-| __C declaration:__ @E3_B@

    __defined at:__ @attributes\/enum_attributes.h 18:63@

    __exported by:__ @attributes\/enum_attributes.h@
-}
pattern E3_B :: E3
pattern E3_B = E3 1

{-| __C declaration:__ @enum E4@

    __defined at:__ @attributes\/enum_attributes.h 21:63@

    __exported by:__ @attributes\/enum_attributes.h@
-}
newtype E4 = E4
  { unwrapE4 :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize E4 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E4 where

  readRaw =
    \ptr0 ->
          pure E4
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E4 unwrapE42 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE42

deriving via Marshal.EquivStorable E4 instance BG.Storable E4

deriving via BG.CUInt instance BG.Prim E4

instance CEnum.CEnum E4 where

  type CEnumZ E4 = BG.CUInt

  toCEnum = E4

  fromCEnum = BG.getField @"unwrapE4"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(1, BG.singleton "E4_A"), (2, BG.singleton "E4_B")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E4"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E4"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E4 where

  minDeclaredValue = E4_A

  maxDeclaredValue = E4_B

instance Show E4 where

  showsPrec = CEnum.shows

instance Read E4 where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrapE4" E4 ty where

  hasField =
    \x0 ->
      (\y1 ->
         E4 {unwrapE4 = y1}, BG.getField @"unwrapE4" x0)

instance (ty ~ BG.CUInt) => BG.HasField "unwrapE4" (BG.Ptr E4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE4")

instance HasCField.HasCField E4 "unwrapE4" where

  type CFieldType E4 "unwrapE4" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E4_A@

    __defined at:__ @attributes\/enum_attributes.h 21:68@

    __exported by:__ @attributes\/enum_attributes.h@
-}
pattern E4_A :: E4
pattern E4_A = E4 1

{-| __C declaration:__ @E4_B@

    __defined at:__ @attributes\/enum_attributes.h 21:78@

    __exported by:__ @attributes\/enum_attributes.h@
-}
pattern E4_B :: E4
pattern E4_B = E4 2

{-| __C declaration:__ @enum E5@

    __defined at:__ @attributes\/enum_attributes.h 22:63@

    __exported by:__ @attributes\/enum_attributes.h@
-}
newtype E5 = E5
  { unwrapE5 :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize E5 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E5 where

  readRaw =
    \ptr0 ->
          pure E5
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E5 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E5 unwrapE52 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE52

deriving via Marshal.EquivStorable E5 instance BG.Storable E5

deriving via BG.CUInt instance BG.Prim E5

instance CEnum.CEnum E5 where

  type CEnumZ E5 = BG.CUInt

  toCEnum = E5

  fromCEnum = BG.getField @"unwrapE5"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(1, BG.singleton "E5_A"), (2, BG.singleton "E5_B")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E5"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E5"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E5 where

  minDeclaredValue = E5_A

  maxDeclaredValue = E5_B

instance Show E5 where

  showsPrec = CEnum.shows

instance Read E5 where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrapE5" E5 ty where

  hasField =
    \x0 ->
      (\y1 ->
         E5 {unwrapE5 = y1}, BG.getField @"unwrapE5" x0)

instance (ty ~ BG.CUInt) => BG.HasField "unwrapE5" (BG.Ptr E5) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE5")

instance HasCField.HasCField E5 "unwrapE5" where

  type CFieldType E5 "unwrapE5" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E5_A@

    __defined at:__ @attributes\/enum_attributes.h 22:68@

    __exported by:__ @attributes\/enum_attributes.h@
-}
pattern E5_A :: E5
pattern E5_A = E5 1

{-| __C declaration:__ @E5_B@

    __defined at:__ @attributes\/enum_attributes.h 22:78@

    __exported by:__ @attributes\/enum_attributes.h@
-}
pattern E5_B :: E5
pattern E5_B = E5 2
