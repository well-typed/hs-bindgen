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
    ( Example.Sym(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @sym@

    __defined at:__ @binding-specs\/omit_type.h 1:14@

    __exported by:__ @binding-specs\/omit_type.h@
-}
newtype Sym = Sym
  { unwrapSym :: BG.CChar
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

instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "unwrapSym" Sym ty where

  hasField =
    \x0 ->
      (\y1 ->
         Sym {unwrapSym = y1}, BG.getField @"unwrapSym" x0)

instance ( ty ~ BG.CChar
         ) => BG.HasField "unwrapSym" (BG.Ptr Sym) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapSym")

instance HasCField.HasCField Sym "unwrapSym" where

  type CFieldType Sym "unwrapSym" = BG.CChar

  offset# = \_ -> \_ -> 0
