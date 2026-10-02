{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.T(..)
    , Example.v
    , Example.f
    , Example.S(..)
    , Example.w
    )
  where

import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro T@

    __defined at:__ @command line@
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

{-| __C declaration:__ @macro V@

    __defined at:__ @command line@
-}
v :: BG.CInt
v = (1 :: BG.CInt)

{-| __C declaration:__ @macro F@

    __defined at:__ @command line@
-}
f :: forall a0. C.Expr.HostPlatform.Add a0 BG.CInt => a0 -> C.Expr.HostPlatform.AddRes a0 BG.CInt
f = \x0 -> (C.Expr.HostPlatform.+) x0 (1 :: BG.CInt)

{-| __C declaration:__ @struct S@

    __defined at:__ @macros\/command_line\/uses.h 7:8@

    __exported by:__ @macros\/command_line\/uses.h@
-}
data S = S
  { s_x :: T
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/command_line\/uses.h 7:14@

         __exported by:__ @macros\/command_line\/uses.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_x") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_x2 ->
            HasCField.writeRaw (BG.Proxy @"s_x") ptr0 s_x2

deriving via Marshal.EquivStorable S instance BG.Storable S

deriving via Struct.IsStructViaReadRaw S instance Struct.IsStruct S

{-| __C declaration:__ @x@

    __defined at:__ @macros\/command_line\/uses.h 7:14@

    __exported by:__ @macros\/command_line\/uses.h@
-}
instance (ty ~ T) => BG.CompatHasField.HasField "s_x" S ty where

  hasField =
    \x0 -> (\y1 -> S {s_x = y1}, BG.getField @"s_x" x0)

instance (ty ~ T) => BG.HasField "s_x" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_x")

instance HasCField.HasCField S "s_x" where

  type CFieldType S "s_x" = T

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro W@

    __defined at:__ @macros\/command_line\/uses.h 9:9@

    __exported by:__ @macros\/command_line\/uses.h@
-}
w :: BG.CInt
w = f v
