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

module M.Macro_typedef_scope_multiple_inner1
    ( M.Macro_typedef_scope_multiple_inner1.M1(..)
    , M.Macro_typedef_scope_multiple_inner1.T2(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, type (~))

{-| __C declaration:__ @macro M1@

    __defined at:__ @macro_typedef_scope_multiple_inner1.h 1:9@

    __exported by:__ @golden\/macros\/parse\/macro_typedef_scope_multiple.h@
-}
newtype M1 = M1
  { unwrapM1 :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapM1" M1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         M1 {unwrapM1 = y1}, BG.getField @"unwrapM1" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapM1" (BG.Ptr M1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapM1")

instance HasCField.HasCField M1 "unwrapM1" where

  type CFieldType M1 "unwrapM1" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T2@

    __defined at:__ @macro_typedef_scope_multiple_inner1.h 2:12@

    __exported by:__ @golden\/macros\/parse\/macro_typedef_scope_multiple.h@
-}
newtype T2 = T2
  { unwrapT2 :: M1
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

instance (ty ~ M1) => BG.CompatHasField.HasField "unwrapT2" T2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T2 {unwrapT2 = y1}, BG.getField @"unwrapT2" x0)

instance (ty ~ M1) => BG.HasField "unwrapT2" (BG.Ptr T2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT2")

instance HasCField.HasCField T2 "unwrapT2" where

  type CFieldType T2 "unwrapT2" = M1

  offset# = \_ -> \_ -> 0
