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

module Example
    ( Example.T(..)
    , Example.P(..)
    , Example.Z_PTR(..)
    , Example.A_T(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, type (~))

{-| __C declaration:__ @macro T@

    __defined at:__ @macros\/macro_type_ptr_order.h 2:9@

    __exported by:__ @macros\/macro_type_ptr_order.h@
-}
newtype T = T
  { unwrapT :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapT" T ty where

  hasField =
    \x0 ->
      (\y1 -> T {unwrapT = y1}, BG.getField @"unwrapT" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapT" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro P@

    __defined at:__ @macros\/macro_type_ptr_order.h 3:9@

    __exported by:__ @macros\/macro_type_ptr_order.h@
-}
newtype P = P
  { unwrapP :: BG.Ptr T
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.Ptr T) => BG.CompatHasField.HasField "unwrapP" P ty where

  hasField =
    \x0 ->
      (\y1 -> P {unwrapP = y1}, BG.getField @"unwrapP" x0)

instance (ty ~ BG.Ptr T) => BG.HasField "unwrapP" (BG.Ptr P) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapP")

instance HasCField.HasCField P "unwrapP" where

  type CFieldType P "unwrapP" = BG.Ptr T

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro Z_PTR@

    __defined at:__ @macros\/macro_type_ptr_order.h 6:9@

    __exported by:__ @macros\/macro_type_ptr_order.h@
-}
newtype Z_PTR = Z_PTR
  { unwrapZ_PTR :: BG.Ptr A_T
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr A_T
         ) => BG.CompatHasField.HasField "unwrapZ_PTR" Z_PTR ty where

  hasField =
    \x0 ->
      (\y1 ->
         Z_PTR {unwrapZ_PTR = y1}, BG.getField @"unwrapZ_PTR" x0)

instance ( ty ~ BG.Ptr A_T
         ) => BG.HasField "unwrapZ_PTR" (BG.Ptr Z_PTR) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapZ_PTR")

instance HasCField.HasCField Z_PTR "unwrapZ_PTR" where

  type CFieldType Z_PTR "unwrapZ_PTR" = BG.Ptr A_T

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro A_T@

    __defined at:__ @macros\/macro_type_ptr_order.h 7:9@

    __exported by:__ @macros\/macro_type_ptr_order.h@
-}
newtype A_T = A_T
  { unwrapA_T :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapA_T" A_T ty where

  hasField =
    \x0 ->
      (\y1 ->
         A_T {unwrapA_T = y1}, BG.getField @"unwrapA_T" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapA_T" (BG.Ptr A_T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA_T")

instance HasCField.HasCField A_T "unwrapA_T" where

  type CFieldType A_T "unwrapA_T" = BG.CInt

  offset# = \_ -> \_ -> 0
