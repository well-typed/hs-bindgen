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

    __defined at:__ @edge-cases\/uses_utf8.h 4:6@

    __exported by:__ @edge-cases\/uses_utf8.h@
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
      CEnum.declaredValuesFromList [(0, RIP.singleton "Say\20320\22909"), (1, RIP.singleton "Say\25308\25308")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "MyEnum"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "MyEnum"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum MyEnum where

  minDeclaredValue = Say你好

  maxDeclaredValue = Say拜拜

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
