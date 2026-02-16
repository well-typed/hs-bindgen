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

{-| __C declaration:__ @ParsedAndSelected1@

    __defined at:__ @declarations\/select_scoping.h 6:13@

    __exported by:__ @declarations\/select_scoping.h@
-}
newtype ParsedAndSelected1 = ParsedAndSelected1
  { unwrapParsedAndSelected1 :: RIP.CInt
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
         ) => RIP.HasField "unwrapParsedAndSelected1" (RIP.Ptr ParsedAndSelected1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapParsedAndSelected1")

instance HasCField.HasCField ParsedAndSelected1 "unwrapParsedAndSelected1" where

  type CFieldType ParsedAndSelected1 "unwrapParsedAndSelected1" =
    RIP.CInt

  offset# = \_ -> \_ -> 0
