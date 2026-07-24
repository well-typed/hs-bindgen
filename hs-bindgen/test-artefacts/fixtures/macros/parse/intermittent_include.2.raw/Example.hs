{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.ma
    , Example.Ta(..)
    , Example.m1
    , Example.T1(..)
    , Example.m2
    , Example.m3
    , Example.T2(..)
    , Example.m4
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro Ma@

    __defined at:__ @intermittent_include_inner.h 1:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
ma :: [String]
ma = ["Ta"]

{-| __C declaration:__ @Ta@

    __defined at:__ @intermittent_include_inner.h 3:13@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype Ta = Ta
  { unwrapTa :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapTa" Ta ty where

  hasField =
    \x0 ->
      (\y1 ->
         Ta {unwrapTa = y1}, BG.getField @"unwrapTa" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapTa" (BG.Ptr Ta) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapTa")

instance HasCField.HasCField Ta "unwrapTa" where

  type CFieldType Ta "unwrapTa" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro M1@

    __defined at:__ @macros\/parse\/intermittent_include.h 7:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
m1 :: [String]
m1 = ["T1"]

{-| __C declaration:__ @T1@

    __defined at:__ @macros\/parse\/intermittent_include.h 9:13@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype T1 = T1
  { unwrapT1 :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapT1" T1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T1 {unwrapT1 = y1}, BG.getField @"unwrapT1" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapT1" (BG.Ptr T1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT1")

instance HasCField.HasCField T1 "unwrapT1" where

  type CFieldType T1 "unwrapT1" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro M2@

    __defined at:__ @macros\/parse\/intermittent_include.h 11:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
m2 :: [String]
m2 = ["T1"]

{-| __C declaration:__ @macro M3@

    __defined at:__ @macros\/parse\/intermittent_include.h 15:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
m3 :: [String]
m3 = ["T2"]

{-| __C declaration:__ @T2@

    __defined at:__ @macros\/parse\/intermittent_include.h 17:13@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype T2 = T2
  { unwrapT2 :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapT2" T2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T2 {unwrapT2 = y1}, BG.getField @"unwrapT2" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapT2" (BG.Ptr T2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT2")

instance HasCField.HasCField T2 "unwrapT2" where

  type CFieldType T2 "unwrapT2" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro M4@

    __defined at:__ @macros\/parse\/intermittent_include.h 19:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
m4 :: [String]
m4 = ["T2"]
