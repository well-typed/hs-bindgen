{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @adiós@

    __defined at:__ @edge-cases\/adios.h 7:13@

    __exported by:__ @edge-cases\/adios.h@
-}
newtype Adio'0301s = Adio'0301s
  { unwrapAdio'0301s :: RIP.CInt
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
         ) => RIP.HasField "unwrapAdio'0301s" (RIP.Ptr Adio'0301s) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapAdio'0301s")

instance HasCField.HasCField Adio'0301s "unwrapAdio'0301s" where

  type CFieldType Adio'0301s "unwrapAdio'0301s" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @数字@

    __defined at:__ @edge-cases\/adios.h 17:13@

    __exported by:__ @edge-cases\/adios.h@
-}
newtype C数字 = C数字
  { unwrapC数字 :: RIP.CInt
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
         ) => RIP.HasField "unwrapC\25968\23383" (RIP.Ptr C数字) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapC\25968\23383")

instance HasCField.HasCField C数字 "unwrapC\25968\23383" where

  type CFieldType C数字 "unwrapC\25968\23383" = RIP.CInt

  offset# = \_ -> \_ -> 0
