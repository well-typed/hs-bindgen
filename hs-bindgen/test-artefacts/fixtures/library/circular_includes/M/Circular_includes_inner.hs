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

module M.Circular_includes_inner
    ( M.Circular_includes_inner.INNER_BEFORE_IFNDEF(..)
    , M.Circular_includes_inner.INNER_BEFORE_CIRCULAR_INCLUDE(..)
    , M.Circular_includes_inner.INNER_AFTER_CIRCULAR_INCLUDE(..)
    , M.Circular_includes_inner.INNER_AFTER_IFNDEF(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, type (~))

{-| __C declaration:__ @INNER_BEFORE_IFNDEF@

    __defined at:__ @circular_includes_inner.h 1:13@

    __exported by:__ @golden\/program-analysis\/circular_includes.h@
-}
newtype INNER_BEFORE_IFNDEF = INNER_BEFORE_IFNDEF
  { unwrapINNER_BEFORE_IFNDEF :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapINNER_BEFORE_IFNDEF" INNER_BEFORE_IFNDEF ty where

  hasField =
    \x0 ->
      ( \y1 ->
          INNER_BEFORE_IFNDEF {unwrapINNER_BEFORE_IFNDEF = y1}
      , BG.getField @"unwrapINNER_BEFORE_IFNDEF" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapINNER_BEFORE_IFNDEF" (BG.Ptr INNER_BEFORE_IFNDEF) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapINNER_BEFORE_IFNDEF")

instance HasCField.HasCField INNER_BEFORE_IFNDEF "unwrapINNER_BEFORE_IFNDEF" where

  type CFieldType INNER_BEFORE_IFNDEF "unwrapINNER_BEFORE_IFNDEF" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @INNER_BEFORE_CIRCULAR_INCLUDE@

    __defined at:__ @circular_includes_inner.h 4:13@

    __exported by:__ @golden\/program-analysis\/circular_includes.h@
-}
newtype INNER_BEFORE_CIRCULAR_INCLUDE = INNER_BEFORE_CIRCULAR_INCLUDE
  { unwrapINNER_BEFORE_CIRCULAR_INCLUDE :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapINNER_BEFORE_CIRCULAR_INCLUDE" INNER_BEFORE_CIRCULAR_INCLUDE ty where

  hasField =
    \x0 ->
      ( \y1 ->
          INNER_BEFORE_CIRCULAR_INCLUDE {unwrapINNER_BEFORE_CIRCULAR_INCLUDE = y1}
      , BG.getField @"unwrapINNER_BEFORE_CIRCULAR_INCLUDE" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapINNER_BEFORE_CIRCULAR_INCLUDE" (BG.Ptr INNER_BEFORE_CIRCULAR_INCLUDE) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapINNER_BEFORE_CIRCULAR_INCLUDE")

instance HasCField.HasCField INNER_BEFORE_CIRCULAR_INCLUDE "unwrapINNER_BEFORE_CIRCULAR_INCLUDE" where

  type CFieldType INNER_BEFORE_CIRCULAR_INCLUDE "unwrapINNER_BEFORE_CIRCULAR_INCLUDE" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @INNER_AFTER_CIRCULAR_INCLUDE@

    __defined at:__ @circular_includes_inner.h 6:13@

    __exported by:__ @golden\/program-analysis\/circular_includes.h@
-}
newtype INNER_AFTER_CIRCULAR_INCLUDE = INNER_AFTER_CIRCULAR_INCLUDE
  { unwrapINNER_AFTER_CIRCULAR_INCLUDE :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapINNER_AFTER_CIRCULAR_INCLUDE" INNER_AFTER_CIRCULAR_INCLUDE ty where

  hasField =
    \x0 ->
      ( \y1 ->
          INNER_AFTER_CIRCULAR_INCLUDE {unwrapINNER_AFTER_CIRCULAR_INCLUDE = y1}
      , BG.getField @"unwrapINNER_AFTER_CIRCULAR_INCLUDE" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapINNER_AFTER_CIRCULAR_INCLUDE" (BG.Ptr INNER_AFTER_CIRCULAR_INCLUDE) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapINNER_AFTER_CIRCULAR_INCLUDE")

instance HasCField.HasCField INNER_AFTER_CIRCULAR_INCLUDE "unwrapINNER_AFTER_CIRCULAR_INCLUDE" where

  type CFieldType INNER_AFTER_CIRCULAR_INCLUDE "unwrapINNER_AFTER_CIRCULAR_INCLUDE" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @INNER_AFTER_IFNDEF@

    __defined at:__ @circular_includes_inner.h 8:13@

    __exported by:__ @golden\/program-analysis\/circular_includes.h@
-}
newtype INNER_AFTER_IFNDEF = INNER_AFTER_IFNDEF
  { unwrapINNER_AFTER_IFNDEF :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapINNER_AFTER_IFNDEF" INNER_AFTER_IFNDEF ty where

  hasField =
    \x0 ->
      ( \y1 ->
          INNER_AFTER_IFNDEF {unwrapINNER_AFTER_IFNDEF = y1}
      , BG.getField @"unwrapINNER_AFTER_IFNDEF" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapINNER_AFTER_IFNDEF" (BG.Ptr INNER_AFTER_IFNDEF) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapINNER_AFTER_IFNDEF")

instance HasCField.HasCField INNER_AFTER_IFNDEF "unwrapINNER_AFTER_IFNDEF" where

  type CFieldType INNER_AFTER_IFNDEF "unwrapINNER_AFTER_IFNDEF" =
    BG.CInt

  offset# = \_ -> \_ -> 0
