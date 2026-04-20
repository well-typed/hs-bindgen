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
    ( Example.Stdlib_CBool(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @stdlib_CBool@

    __defined at:__ @binding-specs\/stdlib\/bool.h 1:14@

    __exported by:__ @binding-specs\/stdlib\/bool.h@
-}
newtype Stdlib_CBool = Stdlib_CBool
  { unwrapStdlib_CBool :: RIP.CBool
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

instance ( ((~) ty) RIP.CBool
         ) => RIP.HasField "unwrapStdlib_CBool" (RIP.Ptr Stdlib_CBool) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStdlib_CBool")

instance HasCField.HasCField Stdlib_CBool "unwrapStdlib_CBool" where

  type CFieldType Stdlib_CBool "unwrapStdlib_CBool" =
    RIP.CBool

  offset# = \_ -> \_ -> 0
