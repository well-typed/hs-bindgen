{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module M.Types
    ( M.Types.Lib_status(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, type (~))

{-| __C declaration:__ @lib_status@

    __defined at:__ @types.h 9:13@

    __exported by:__ @forward_typedef\/root.h@
-}
newtype Lib_status = Lib_status
  { unwrapLib_status :: BG.CInt
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

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapLib_status" Lib_status ty where

  hasField =
    \x0 ->
      ( \y1 -> Lib_status {unwrapLib_status = y1}
      , BG.getField @"unwrapLib_status" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapLib_status" (BG.Ptr Lib_status) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapLib_status")

instance HasCField.HasCField Lib_status "unwrapLib_status" where

  type CFieldType Lib_status "unwrapLib_status" =
    BG.CInt

  offset# = \_ -> \_ -> 0
