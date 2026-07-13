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
    ( Example.M1(..)
    , Example.T2(..)
    , Example.M3(..)
    , Example.T4(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro M1@

    __defined at:__ @macros\/parse\/macro_typedef_scope.h 4:9@

    __exported by:__ @macros\/parse\/macro_typedef_scope.h@
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

    __defined at:__ @macros\/parse\/macro_typedef_scope.h 5:12@

    __exported by:__ @macros\/parse\/macro_typedef_scope.h@
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

{-| __C declaration:__ @macro M3@

    __defined at:__ @macros\/parse\/macro_typedef_scope.h 6:9@

    __exported by:__ @macros\/parse\/macro_typedef_scope.h@
-}
newtype M3 = M3
  { unwrapM3 :: T2
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

instance (ty ~ T2) => BG.CompatHasField.HasField "unwrapM3" M3 ty where

  hasField =
    \x0 ->
      (\y1 ->
         M3 {unwrapM3 = y1}, BG.getField @"unwrapM3" x0)

instance (ty ~ T2) => BG.HasField "unwrapM3" (BG.Ptr M3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapM3")

instance HasCField.HasCField M3 "unwrapM3" where

  type CFieldType M3 "unwrapM3" = T2

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T4@

    __defined at:__ @macros\/parse\/macro_typedef_scope.h 7:12@

    __exported by:__ @macros\/parse\/macro_typedef_scope.h@
-}
newtype T4 = T4
  { unwrapT4 :: M3
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

instance (ty ~ M3) => BG.CompatHasField.HasField "unwrapT4" T4 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T4 {unwrapT4 = y1}, BG.getField @"unwrapT4" x0)

instance (ty ~ M3) => BG.HasField "unwrapT4" (BG.Ptr T4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT4")

instance HasCField.HasCField T4 "unwrapT4" where

  type CFieldType T4 "unwrapT4" = M3

  offset# = \_ -> \_ -> 0
