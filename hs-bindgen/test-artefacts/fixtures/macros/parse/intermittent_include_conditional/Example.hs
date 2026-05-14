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
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @T2@

    __defined at:__ @intermittent_include_conditional_inner.h 2:15@

    __exported by:__ @macros\/parse\/intermittent_include_conditional.h@
-}
newtype T2 = T2
  { unwrapT2 :: RIP.CFloat
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , Floating
    , Fractional
    , RIP.HasFFIType
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , RealFloat
    , RealFrac
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.CFloat
         ) => RIP.HasField "unwrapT2" (RIP.Ptr T2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT2")

instance HasCField.HasCField T2 "unwrapT2" where

  type CFieldType T2 "unwrapT2" = RIP.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T4@

    __defined at:__ @intermittent_include_conditional_inner.h 6:16@

    __exported by:__ @macros\/parse\/intermittent_include_conditional.h@
-}
newtype T4 = T4
  { unwrapT4 :: RIP.CDouble
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , Floating
    , Fractional
    , RIP.HasFFIType
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , RealFloat
    , RealFrac
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.CDouble
         ) => RIP.HasField "unwrapT4" (RIP.Ptr T4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT4")

instance HasCField.HasCField T4 "unwrapT4" where

  type CFieldType T4 "unwrapT4" = RIP.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T1@

    __defined at:__ @macros\/parse\/intermittent_include_conditional.h 1:13@

    __exported by:__ @macros\/parse\/intermittent_include_conditional.h@
-}
newtype T1 = T1
  { unwrapT1 :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unwrapT1" (RIP.Ptr T1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT1")

instance HasCField.HasCField T1 "unwrapT1" where

  type CFieldType T1 "unwrapT1" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T3@

    __defined at:__ @macros\/parse\/intermittent_include_conditional.h 6:13@

    __exported by:__ @macros\/parse\/intermittent_include_conditional.h@
-}
newtype T3 = T3
  { unwrapT3 :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unwrapT3" (RIP.Ptr T3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT3")

instance HasCField.HasCField T3 "unwrapT3" where

  type CFieldType T3 "unwrapT3" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T5@

    __defined at:__ @macros\/parse\/intermittent_include_conditional.h 11:13@

    __exported by:__ @macros\/parse\/intermittent_include_conditional.h@
-}
newtype T5 = T5
  { unwrapT5 :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unwrapT5" (RIP.Ptr T5) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT5")

instance HasCField.HasCField T5 "unwrapT5" where

  type CFieldType T5 "unwrapT5" = RIP.CInt

  offset# = \_ -> \_ -> 0
