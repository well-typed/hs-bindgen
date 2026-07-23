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
    ( Example.T2(..)
    , Example.T4(..)
    , Example.T1(..)
    , Example.T3(..)
    , Example.T5(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @T2@

    __defined at:__ @intermittent_include_conditional_inner.h 2:15@

    __exported by:__ @macros\/parse\/intermittent_include_conditional.h@
-}
newtype T2 = T2
  { unwrapT2 :: BG.CFloat
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , Floating
    , Fractional
    , BG.HasFFIType
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , RealFloat
    , RealFrac
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CFloat) => BG.CompatHasField.HasField "unwrapT2" T2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T2 {unwrapT2 = y1}, BG.getField @"unwrapT2" x0)

instance ( ty ~ BG.CFloat
         ) => BG.HasField "unwrapT2" (BG.Ptr T2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT2")

instance HasCField.HasCField T2 "unwrapT2" where

  type CFieldType T2 "unwrapT2" = BG.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T4@

    __defined at:__ @intermittent_include_conditional_inner.h 6:16@

    __exported by:__ @macros\/parse\/intermittent_include_conditional.h@
-}
newtype T4 = T4
  { unwrapT4 :: BG.CDouble
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , Floating
    , Fractional
    , BG.HasFFIType
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , RealFloat
    , RealFrac
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CDouble) => BG.CompatHasField.HasField "unwrapT4" T4 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T4 {unwrapT4 = y1}, BG.getField @"unwrapT4" x0)

instance ( ty ~ BG.CDouble
         ) => BG.HasField "unwrapT4" (BG.Ptr T4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT4")

instance HasCField.HasCField T4 "unwrapT4" where

  type CFieldType T4 "unwrapT4" = BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T1@

    __defined at:__ @macros\/parse\/intermittent_include_conditional.h 1:13@

    __exported by:__ @macros\/parse\/intermittent_include_conditional.h@
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

{-| __C declaration:__ @T3@

    __defined at:__ @macros\/parse\/intermittent_include_conditional.h 6:13@

    __exported by:__ @macros\/parse\/intermittent_include_conditional.h@
-}
newtype T3 = T3
  { unwrapT3 :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapT3" T3 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T3 {unwrapT3 = y1}, BG.getField @"unwrapT3" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapT3" (BG.Ptr T3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT3")

instance HasCField.HasCField T3 "unwrapT3" where

  type CFieldType T3 "unwrapT3" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T5@

    __defined at:__ @macros\/parse\/intermittent_include_conditional.h 11:13@

    __exported by:__ @macros\/parse\/intermittent_include_conditional.h@
-}
newtype T5 = T5
  { unwrapT5 :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapT5" T5 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T5 {unwrapT5 = y1}, BG.getField @"unwrapT5" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapT5" (BG.Ptr T5) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT5")

instance HasCField.HasCField T5 "unwrapT5" where

  type CFieldType T5 "unwrapT5" = BG.CInt

  offset# = \_ -> \_ -> 0
