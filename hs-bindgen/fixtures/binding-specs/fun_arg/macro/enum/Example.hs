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

{-| __C declaration:__ @enum MyEnum@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
newtype MyEnum = MyEnum
  { unwrapMyEnum :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize MyEnum where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw MyEnum where

  readRaw =
    \ptr0 ->
          pure MyEnum
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw MyEnum where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyEnum unwrapMyEnum2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapMyEnum2

deriving via Marshal.EquivStorable MyEnum instance RIP.Storable MyEnum

deriving via RIP.CUInt instance RIP.Prim MyEnum

instance CEnum.CEnum MyEnum where

  type CEnumZ MyEnum = RIP.CUInt

  toCEnum = MyEnum

  fromCEnum = unwrapMyEnum

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "MyEnum"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "MyEnum"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum MyEnum where

  minDeclaredValue = X

  maxDeclaredValue = X

instance Show MyEnum where

  showsPrec = CEnum.shows

instance Read MyEnum where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapMyEnum" (RIP.Ptr MyEnum) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyEnum")

instance HasCField.HasCField MyEnum "unwrapMyEnum" where

  type CFieldType MyEnum "unwrapMyEnum" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 4:14@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
pattern X :: MyEnum
pattern X = MyEnum 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
newtype A = A
  { unwrapA :: MyEnum
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.HasFFIType
    , RIP.Prim
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) MyEnum
         ) => RIP.HasField "unwrapA" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyEnum

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.HasFFIType
    , RIP.Prim
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance (((~) ty) A) => RIP.HasField "unwrapB" (RIP.Ptr B) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0
