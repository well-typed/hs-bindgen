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
    ( Example.Adio'0301s(..)
    , Example.C数字(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @adiós@

    __defined at:__ @edge-cases\/adios.h 7:13@

    __exported by:__ @edge-cases\/adios.h@
-}
newtype Adio'0301s = Adio'0301s
  { unwrapAdio'0301s :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapAdio'0301s" Adio'0301s ty where

  hasField =
    \x0 ->
      ( \y1 -> Adio'0301s {unwrapAdio'0301s = y1}
      , BG.getField @"unwrapAdio'0301s" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapAdio'0301s" (BG.Ptr Adio'0301s) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapAdio'0301s")

instance HasCField.HasCField Adio'0301s "unwrapAdio'0301s" where

  type CFieldType Adio'0301s "unwrapAdio'0301s" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @数字@

    __defined at:__ @edge-cases\/adios.h 17:13@

    __exported by:__ @edge-cases\/adios.h@
-}
newtype C数字 = C数字
  { unwrapC数字 :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapC\25968\23383" C数字 ty where

  hasField =
    \x0 ->
      (\y1 ->
         C数字 {unwrapC数字 = y1}, BG.getField @"unwrapC\25968\23383" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapC\25968\23383" (BG.Ptr C数字) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapC\25968\23383")

instance HasCField.HasCField C数字 "unwrapC\25968\23383" where

  type CFieldType C数字 "unwrapC\25968\23383" = BG.CInt

  offset# = \_ -> \_ -> 0
