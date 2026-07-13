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
    ( Example.MySym(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @sym@

    __defined at:__ @binding-specs\/name\/type.h 1:14@

    __exported by:__ @binding-specs\/name\/type.h@
-}
newtype MySym = MySym
  { unwrapMySym :: BG.CChar
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

instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "unwrapMySym" MySym ty where

  hasField =
    \x0 ->
      (\y1 ->
         MySym {unwrapMySym = y1}, BG.getField @"unwrapMySym" x0)

instance ( ty ~ BG.CChar
         ) => BG.HasField "unwrapMySym" (BG.Ptr MySym) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMySym")

instance HasCField.HasCField MySym "unwrapMySym" where

  type CFieldType MySym "unwrapMySym" = BG.CChar

  offset# = \_ -> \_ -> 0
