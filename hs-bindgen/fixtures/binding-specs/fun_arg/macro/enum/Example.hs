{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)

{-| __C declaration:__ @enum MyEnum@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
newtype MyEnum = MyEnum
  { unwrapMyEnum :: FC.CUInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize MyEnum where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw MyEnum where

  readRaw =
    \ptr0 ->
          pure MyEnum
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw MyEnum where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyEnum unwrapMyEnum2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapMyEnum2

deriving via HsBindgen.Runtime.Marshal.EquivStorable MyEnum instance F.Storable MyEnum

deriving via FC.CUInt instance Data.Primitive.Types.Prim MyEnum

instance HsBindgen.Runtime.CEnum.CEnum MyEnum where

  type CEnumZ MyEnum = FC.CUInt

  toCEnum = MyEnum

  fromCEnum = unwrapMyEnum

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "X")]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "MyEnum"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "MyEnum"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum MyEnum where

  minDeclaredValue = X

  maxDeclaredValue = X

instance Show MyEnum where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read MyEnum where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance GHC.Records.HasField "unwrapMyEnum" (Ptr.Ptr MyEnum) (Ptr.Ptr FC.CUInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMyEnum")

instance HsBindgen.Runtime.HasCField.HasCField MyEnum "unwrapMyEnum" where

  type CFieldType MyEnum "unwrapMyEnum" = FC.CUInt

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
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    )

instance GHC.Records.HasField "unwrapA" (Ptr.Ptr A) (Ptr.Ptr MyEnum) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapA")

instance HsBindgen.Runtime.HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyEnum

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    )

instance GHC.Records.HasField "unwrapB" (Ptr.Ptr B) (Ptr.Ptr A) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapB")

instance HsBindgen.Runtime.HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0
