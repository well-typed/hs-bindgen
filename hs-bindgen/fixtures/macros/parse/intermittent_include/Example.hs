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
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @Ta@

    __defined at:__ @intermittent_include_inner.h 3:13@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype Ta = Ta
  { unwrapTa :: RIP.CInt
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

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapTa" (RIP.Ptr Ta) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapTa")

instance HasCField.HasCField Ta "unwrapTa" where

  type CFieldType Ta "unwrapTa" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro Ma@

    __defined at:__ @intermittent_include_inner.h 1:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype Ma = Ma
  { unwrapMa :: Ta
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

instance ( ((~) ty) Ta
         ) => RIP.HasField "unwrapMa" (RIP.Ptr Ma) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapMa")

instance HasCField.HasCField Ma "unwrapMa" where

  type CFieldType Ma "unwrapMa" = Ta

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T1@

    __defined at:__ @macros\/parse\/intermittent_include.h 9:13@

    __exported by:__ @macros\/parse\/intermittent_include.h@
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

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapT1" (RIP.Ptr T1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT1")

instance HasCField.HasCField T1 "unwrapT1" where

  type CFieldType T1 "unwrapT1" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro M1@

    __defined at:__ @macros\/parse\/intermittent_include.h 7:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype M1 = M1
  { unwrapM1 :: T1
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

instance ( ((~) ty) T1
         ) => RIP.HasField "unwrapM1" (RIP.Ptr M1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapM1")

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

instance ( ((~) ty) T1
         ) => RIP.HasField "unwrapM2" (RIP.Ptr M2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapM2")

instance HasCField.HasCField M2 "unwrapM2" where

  type CFieldType M2 "unwrapM2" = T1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T2@

    __defined at:__ @macros\/parse\/intermittent_include.h 17:13@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype T2 = T2
  { unwrapT2 :: RIP.CInt
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

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapT2" (RIP.Ptr T2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT2")

instance HasCField.HasCField T2 "unwrapT2" where

  type CFieldType T2 "unwrapT2" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro M3@

    __defined at:__ @macros\/parse\/intermittent_include.h 15:9@

    __exported by:__ @macros\/parse\/intermittent_include.h@
-}
newtype M3 = M3
  { unwrapM3 :: T2
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

instance ( ((~) ty) T2
         ) => RIP.HasField "unwrapM3" (RIP.Ptr M3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapM3")

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

instance ( ((~) ty) T2
         ) => RIP.HasField "unwrapM4" (RIP.Ptr M4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapM4")

instance HasCField.HasCField M4 "unwrapM4" where

  type CFieldType M4 "unwrapM4" = T2

  offset# = \_ -> \_ -> 0
