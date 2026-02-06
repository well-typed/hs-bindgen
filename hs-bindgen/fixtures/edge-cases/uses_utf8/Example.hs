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

{-| __C declaration:__ @enum MyEnum@

    __defined at:__ @edge-cases\/uses_utf8.h 4:6@

    __exported by:__ @edge-cases\/uses_utf8.h@
-}
newtype MyEnum = MyEnum
  { unwrapMyEnum :: FC.CUInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord)
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
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "Say\20320\22909")
                                                     , (1, Data.List.NonEmpty.singleton "Say\25308\25308")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "MyEnum"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "MyEnum"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum MyEnum where

  minDeclaredValue = Say你好

  maxDeclaredValue = Say拜拜

instance Show MyEnum where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read MyEnum where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyEnum) "unwrapMyEnum")
         ) => GHC.Records.HasField "unwrapMyEnum" (Ptr.Ptr MyEnum) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMyEnum")

instance HsBindgen.Runtime.HasCField.HasCField MyEnum "unwrapMyEnum" where

  type CFieldType MyEnum "unwrapMyEnum" = FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @Say你好@

    __defined at:__ @edge-cases\/uses_utf8.h 5:9@

    __exported by:__ @edge-cases\/uses_utf8.h@
-}
pattern Say你好 :: MyEnum
pattern Say你好 = MyEnum 0

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/uses_utf8.h 6:9@

    __exported by:__ @edge-cases\/uses_utf8.h@
-}
pattern Say拜拜 :: MyEnum
pattern Say拜拜 = MyEnum 1
