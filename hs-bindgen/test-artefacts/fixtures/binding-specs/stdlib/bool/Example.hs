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
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @stdlib_CBool@

    __defined at:__ @binding-specs\/stdlib\/bool.h 1:14@

    __exported by:__ @binding-specs\/stdlib\/bool.h@
-}
newtype Stdlib_CBool = Stdlib_CBool
  { unwrapStdlib_CBool :: BG.CBool
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

instance ( ty ~ BG.CBool
         ) => BG.CompatHasField.HasField "unwrapStdlib_CBool" Stdlib_CBool ty where

  hasField =
    \x0 ->
      ( \y1 -> Stdlib_CBool {unwrapStdlib_CBool = y1}
      , BG.getField @"unwrapStdlib_CBool" x0
      )

instance ( ty ~ BG.CBool
         ) => BG.HasField "unwrapStdlib_CBool" (BG.Ptr Stdlib_CBool) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStdlib_CBool")

instance HasCField.HasCField Stdlib_CBool "unwrapStdlib_CBool" where

  type CFieldType Stdlib_CBool "unwrapStdlib_CBool" =
    BG.CBool

  offset# = \_ -> \_ -> 0
