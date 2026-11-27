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
    ( Example.T(..)
    , Example.E(..)
    , pattern Example.E_A
    , pattern Example.E_B
    , pattern Example.U_A
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, type (~))

{-| __C declaration:__ @T@

    __defined at:__ @enum_selected_t.h 3:23@

    __exported by:__ @program-analysis\/program-slicing\/enum_selected.h@
-}
newtype T = T
  { unwrapT :: BG.CUChar
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CUChar) => BG.CompatHasField.HasField "unwrapT" T ty where

  hasField =
    \x0 ->
      (\y1 -> T {unwrapT = y1}, BG.getField @"unwrapT" x0)

instance (ty ~ BG.CUChar) => BG.HasField "unwrapT" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = BG.CUChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @enum E@

    __defined at:__ @program-analysis\/program-slicing\/enum_selected.h 7:6@

    __exported by:__ @program-analysis\/program-slicing\/enum_selected.h@
-}
newtype E = E
  { unwrapE :: T
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.Bitfield, BG.HasFFIType)

instance Marshal.StaticSize E where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw E where

  readRaw =
    \ptr0 ->
          pure E
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E unwrapE2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE2

deriving via Marshal.EquivStorable E instance BG.Storable E

deriving via T instance BG.Prim E

instance CEnum.CEnum E where

  type CEnumZ E = T

  toCEnum = E

  fromCEnum = BG.getField @"unwrapE"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "E_A"), (1, BG.singleton "E_B")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E where

  minDeclaredValue = E_A

  maxDeclaredValue = E_B

instance Show E where

  showsPrec = CEnum.shows

instance Read E where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ T) => BG.CompatHasField.HasField "unwrapE" E ty where

  hasField =
    \x0 ->
      (\y1 -> E {unwrapE = y1}, BG.getField @"unwrapE" x0)

instance (ty ~ T) => BG.HasField "unwrapE" (BG.Ptr E) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE")

instance HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = T

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E_A@

    __defined at:__ @program-analysis\/program-slicing\/enum_selected.h 7:14@

    __exported by:__ @program-analysis\/program-slicing\/enum_selected.h@
-}
pattern E_A :: E
pattern E_A = E 0

{-| __C declaration:__ @E_B@

    __defined at:__ @program-analysis\/program-slicing\/enum_selected.h 7:19@

    __exported by:__ @program-analysis\/program-slicing\/enum_selected.h@
-}
pattern E_B :: E
pattern E_B = E 1

{-| __C declaration:__ @U_A@

    __defined at:__ @program-analysis\/program-slicing\/enum_selected.h 8:1@

    __exported by:__ @program-analysis\/program-slicing\/enum_selected.h@
-}
pattern U_A :: T
pattern U_A = 0
