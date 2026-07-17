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
    ( Example.BOOL(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro BOOL@

    __defined at:__ @macros\/reparse\/cref_attributes.h 2:9@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
newtype BOOL = BOOL
  { unwrapBOOL :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapBOOL" BOOL ty where

  hasField =
    \x0 ->
      (\y1 ->
         BOOL {unwrapBOOL = y1}, BG.getField @"unwrapBOOL" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapBOOL" (BG.Ptr BOOL) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapBOOL")

instance HasCField.HasCField BOOL "unwrapBOOL" where

  type CFieldType BOOL "unwrapBOOL" = BG.CInt

  offset# = \_ -> \_ -> 0
