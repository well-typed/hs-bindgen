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
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, type (~))

{-| __C declaration:__ @enum E0@

    __defined at:__ @attributes\/unexposed_attributes.h 17:50@

    __exported by:__ @attributes\/unexposed_attributes.h@
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
      CEnum.declaredValuesFromList [(0, BG.singleton "E0_A"), (1, BG.singleton "E0_B")]

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

    __defined at:__ @attributes\/unexposed_attributes.h 17:55@

    __exported by:__ @attributes\/unexposed_attributes.h@
-}
pattern E0_A :: E0
pattern E0_A = E0 0

{-| __C declaration:__ @E0_B@

    __defined at:__ @attributes\/unexposed_attributes.h 17:61@

    __exported by:__ @attributes\/unexposed_attributes.h@
-}
pattern E0_B :: E0
pattern E0_B = E0 1

{-| __C declaration:__ @enum E1@

    __defined at:__ @attributes\/unexposed_attributes.h 45:64@

    __exported by:__ @attributes\/unexposed_attributes.h@
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
      CEnum.declaredValuesFromList [(0, BG.singleton "E1_A"), (1, BG.singleton "E1_B")]

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

    __defined at:__ @attributes\/unexposed_attributes.h 45:69@

    __exported by:__ @attributes\/unexposed_attributes.h@
-}
pattern E1_A :: E1
pattern E1_A = E1 0

{-| __C declaration:__ @E1_B@

    __defined at:__ @attributes\/unexposed_attributes.h 45:75@

    __exported by:__ @attributes\/unexposed_attributes.h@
-}
pattern E1_B :: E1
pattern E1_B = E1 1
