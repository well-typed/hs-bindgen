{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
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
    ( Example.valueA
    , Example.valueB
    , Example.TypeA(..)
    , Example.TypeB(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @macro ValueA@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 7:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
valueA :: RIP.CInt
valueA = (1 :: RIP.CInt)

{-| __C declaration:__ @macro ValueB@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 6:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
valueB :: RIP.CInt
valueB = valueA

{-| __C declaration:__ @macro TypeA@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 10:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
newtype TypeA = TypeA
  { unwrapTypeA :: RIP.CInt
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

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "unwrapTypeA" (RIP.Ptr TypeA) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTypeA")

instance HasCField.HasCField TypeA "unwrapTypeA" where

  type CFieldType TypeA "unwrapTypeA" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro TypeB@

    __defined at:__ @macros\/parse\/first_parse_then_typecheck.h 9:9@

    __exported by:__ @macros\/parse\/first_parse_then_typecheck.h@
-}
newtype TypeB = TypeB
  { unwrapTypeB :: TypeA
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

instance ( (~) ty TypeA
         ) => RIP.HasField "unwrapTypeB" (RIP.Ptr TypeB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTypeB")

instance HasCField.HasCField TypeB "unwrapTypeB" where

  type CFieldType TypeB "unwrapTypeB" = TypeA

  offset# = \_ -> \_ -> 0
