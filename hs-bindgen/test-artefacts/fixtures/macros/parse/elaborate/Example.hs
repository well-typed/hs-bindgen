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
    ( Example.oUTER_A
    , Example.iNNER_A
    , Example.iNNER_B
    , Example.Outer_int(..)
    , Example.Inner_int(..)
    , Example.oUTER_B
    , Example.oUTER_C
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @macro OUTER_A@

    __defined at:__ @macros\/parse\/elaborate.h 7:9@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
oUTER_A :: RIP.CInt
oUTER_A = (1 :: RIP.CInt)

{-| __C declaration:__ @macro INNER_A@

    __defined at:__ @elaborate_inner.h 1:9@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
iNNER_A :: RIP.CInt
iNNER_A = oUTER_A

{-| __C declaration:__ @macro INNER_B@

    __defined at:__ @elaborate_inner.h 2:9@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
iNNER_B :: RIP.CInt
iNNER_B = (2 :: RIP.CInt)

{-| __C declaration:__ @outer_int@

    __defined at:__ @macros\/parse\/elaborate.h 8:13@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
newtype Outer_int = Outer_int
  { unwrapOuter_int :: RIP.CInt
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
         ) => RIP.HasField "unwrapOuter_int" (RIP.Ptr Outer_int) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapOuter_int")

instance HasCField.HasCField Outer_int "unwrapOuter_int" where

  type CFieldType Outer_int "unwrapOuter_int" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @inner_int@

    __defined at:__ @elaborate_inner.h 3:19@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
newtype Inner_int = Inner_int
  { unwrapInner_int :: Outer_int
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

instance ( ty ~ Outer_int
         ) => RIP.HasField "unwrapInner_int" (RIP.Ptr Inner_int) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapInner_int")

instance HasCField.HasCField Inner_int "unwrapInner_int" where

  type CFieldType Inner_int "unwrapInner_int" =
    Outer_int

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro OUTER_B@

    __defined at:__ @macros\/parse\/elaborate.h 12:9@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
oUTER_B :: RIP.CInt
oUTER_B = iNNER_A

{-| __C declaration:__ @macro OUTER_C@

    __defined at:__ @macros\/parse\/elaborate.h 13:9@

    __exported by:__ @macros\/parse\/elaborate.h@
-}
oUTER_C :: RIP.CInt
oUTER_C = iNNER_B
