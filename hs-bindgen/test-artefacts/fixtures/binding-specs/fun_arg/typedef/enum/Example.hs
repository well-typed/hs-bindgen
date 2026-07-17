{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Example
    ( Example.MyEnum(..)
    , pattern Example.X
    , Example.A(..)
    , Example.B(..)
    , Example.E(..)
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified M

{-| __C declaration:__ @enum MyEnum@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/enum.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/enum.h@
-}
newtype MyEnum = MyEnum
  { unwrapMyEnum :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

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

deriving via Marshal.EquivStorable MyEnum instance BG.Storable MyEnum

deriving via BG.CUInt instance BG.Prim MyEnum

instance CEnum.CEnum MyEnum where

  type CEnumZ MyEnum = BG.CUInt

  toCEnum = MyEnum

  fromCEnum = BG.getField @"unwrapMyEnum"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "X")]

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

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapMyEnum" MyEnum ty where

  hasField =
    \x0 ->
      (\y1 ->
         MyEnum {unwrapMyEnum = y1}, BG.getField @"unwrapMyEnum" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapMyEnum" (BG.Ptr MyEnum) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMyEnum")

instance HasCField.HasCField MyEnum "unwrapMyEnum" where

  type CFieldType MyEnum "unwrapMyEnum" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/enum.h 4:14@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/enum.h@
-}
pattern X :: MyEnum
pattern X = MyEnum 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/enum.h 7:21@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/enum.h@
-}
newtype A = A
  { unwrapA :: MyEnum
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.HasFFIType
    , BG.Prim
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ MyEnum) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance (ty ~ MyEnum) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyEnum

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/enum.h 8:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/enum.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.HasFFIType
    , BG.Prim
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ A) => BG.CompatHasField.HasField "unwrapB" B ty where

  hasField =
    \x0 ->
      (\y1 -> B {unwrapB = y1}, BG.getField @"unwrapB" x0)

instance (ty ~ A) => BG.HasField "unwrapB" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/enum.h 21:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/enum.h@
-}
newtype E = E
  { unwrapE :: M.C
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

instance (ty ~ M.C) => BG.CompatHasField.HasField "unwrapE" E ty where

  hasField =
    \x0 ->
      (\y1 -> E {unwrapE = y1}, BG.getField @"unwrapE" x0)

instance (ty ~ M.C) => BG.HasField "unwrapE" (BG.Ptr E) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE")

instance HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
