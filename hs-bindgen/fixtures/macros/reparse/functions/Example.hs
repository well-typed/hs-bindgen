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
    ( Example.iD
    , Example.MY_INT(..)
    , Example.My_int_t(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @macro ID@

    __defined at:__ @macros\/reparse\/functions.h 1:9@

    __exported by:__ @macros\/reparse\/functions.h@
-}
iD :: forall a0. a0 -> a0
iD = \x0 -> x0

{-| __C declaration:__ @macro MY_INT@

    __defined at:__ @macros\/reparse\/functions.h 2:9@

    __exported by:__ @macros\/reparse\/functions.h@
-}
newtype MY_INT = MY_INT
  { unwrapMY_INT :: RIP.CInt
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
         ) => RIP.HasField "unwrapMY_INT" (RIP.Ptr MY_INT) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMY_INT")

instance HasCField.HasCField MY_INT "unwrapMY_INT" where

  type CFieldType MY_INT "unwrapMY_INT" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @my_int_t@

    __defined at:__ @macros\/reparse\/functions.h 19:16@

    __exported by:__ @macros\/reparse\/functions.h@
-}
newtype My_int_t = My_int_t
  { unwrapMy_int_t :: MY_INT
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

instance ( ty ~ MY_INT
         ) => RIP.HasField "unwrapMy_int_t" (RIP.Ptr My_int_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMy_int_t")

instance HasCField.HasCField My_int_t "unwrapMy_int_t" where

  type CFieldType My_int_t "unwrapMy_int_t" = MY_INT

  offset# = \_ -> \_ -> 0
