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
    ( Example.m1
    , Example.T2(..)
    , Example.m3
    , Example.T4(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro M1@

    __defined at:__ @macro_typedef_scope_multiple_inner1.h 1:9@

    __exported by:__ @macros\/parse\/macro_typedef_scope_multiple.h@
-}
m1 :: [String]
m1 = ["int"]

{-| __C declaration:__ @T2@

    __defined at:__ @macro_typedef_scope_multiple_inner1.h 2:12@

    __exported by:__ @macros\/parse\/macro_typedef_scope_multiple.h@
-}
newtype T2 = T2
  { unwrapT2 :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapT2" T2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T2 {unwrapT2 = y1}, BG.getField @"unwrapT2" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapT2" (BG.Ptr T2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT2")

instance HasCField.HasCField T2 "unwrapT2" where

  type CFieldType T2 "unwrapT2" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro M3@

    __defined at:__ @macro_typedef_scope_multiple_inner2.h 1:9@

    __exported by:__ @macros\/parse\/macro_typedef_scope_multiple.h@
-}
m3 :: [String]
m3 = ["T2"]

{-| __C declaration:__ @T4@

    __defined at:__ @macro_typedef_scope_multiple_inner2.h 2:12@

    __exported by:__ @macros\/parse\/macro_typedef_scope_multiple.h@
-}
newtype T4 = T4
  { unwrapT4 :: T2
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

instance (ty ~ T2) => BG.CompatHasField.HasField "unwrapT4" T4 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T4 {unwrapT4 = y1}, BG.getField @"unwrapT4" x0)

instance (ty ~ T2) => BG.HasField "unwrapT4" (BG.Ptr T4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT4")

instance HasCField.HasCField T4 "unwrapT4" where

  type CFieldType T4 "unwrapT4" = T2

  offset# = \_ -> \_ -> 0
