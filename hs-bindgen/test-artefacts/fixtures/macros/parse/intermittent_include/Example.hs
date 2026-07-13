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
    ( Example.Ta(..)
    , Example.Ma(..)
    , Example.T1(..)
    , Example.M1(..)
    , Example.M2(..)
    , Example.T2(..)
    , Example.M3(..)
    , Example.M4(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

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

{-| __C declaration:__ @macro Ma@

    __defined at:__ @intermittent_include_inner.h 1:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype Ma = Ma
  { unwrapMa :: Ta
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

instance (ty ~ Ta) => BG.CompatHasField.HasField "unwrapMa" Ma ty where

  hasField =
    \x0 ->
      (\y1 ->
         Ma {unwrapMa = y1}, BG.getField @"unwrapMa" x0)

instance (ty ~ Ta) => BG.HasField "unwrapMa" (BG.Ptr Ma) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapMa")

instance HasCField.HasCField Ma "unwrapMa" where

  type CFieldType Ma "unwrapMa" = Ta

  offset# = \_ -> \_ -> 0

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

{-| __C declaration:__ @macro M1@

    __defined at:__ @macros\/parse\/intermittent_include.h 7:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype M1 = M1
  { unwrapM1 :: T1
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

instance (ty ~ T1) => BG.CompatHasField.HasField "unwrapM1" M1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         M1 {unwrapM1 = y1}, BG.getField @"unwrapM1" x0)

instance (ty ~ T1) => BG.HasField "unwrapM1" (BG.Ptr M1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapM1")

instance HasCField.HasCField M1 "unwrapM1" where

  type CFieldType M1 "unwrapM1" = T1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro M2@

    __defined at:__ @macros\/parse\/intermittent_include.h 11:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype M2 = M2
  { unwrapM2 :: T1
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

instance (ty ~ T1) => BG.CompatHasField.HasField "unwrapM2" M2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         M2 {unwrapM2 = y1}, BG.getField @"unwrapM2" x0)

instance (ty ~ T1) => BG.HasField "unwrapM2" (BG.Ptr M2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapM2")

instance HasCField.HasCField M2 "unwrapM2" where

  type CFieldType M2 "unwrapM2" = T1

  offset# = \_ -> \_ -> 0

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

{-| __C declaration:__ @macro M3@

    __defined at:__ @macros\/parse\/intermittent_include.h 15:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype M3 = M3
  { unwrapM3 :: T2
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

instance (ty ~ T2) => BG.CompatHasField.HasField "unwrapM3" M3 ty where

  hasField =
    \x0 ->
      (\y1 ->
         M3 {unwrapM3 = y1}, BG.getField @"unwrapM3" x0)

instance (ty ~ T2) => BG.HasField "unwrapM3" (BG.Ptr M3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapM3")

instance HasCField.HasCField M3 "unwrapM3" where

  type CFieldType M3 "unwrapM3" = T2

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro M4@

    __defined at:__ @macros\/parse\/intermittent_include.h 19:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype M4 = M4
  { unwrapM4 :: T2
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

instance (ty ~ T2) => BG.CompatHasField.HasField "unwrapM4" M4 ty where

  hasField =
    \x0 ->
      (\y1 ->
         M4 {unwrapM4 = y1}, BG.getField @"unwrapM4" x0)

instance (ty ~ T2) => BG.HasField "unwrapM4" (BG.Ptr M4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapM4")

instance HasCField.HasCField M4 "unwrapM4" where

  type CFieldType M4 "unwrapM4" = T2

  offset# = \_ -> \_ -> 0
