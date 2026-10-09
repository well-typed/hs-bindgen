{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.S0(..)
    , Example.S1(..)
    , Example.U0(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct S0@

    __defined at:__ @attributes\/objc_boxable.h 12:39@

    __exported by:__ @attributes\/objc_boxable.h@
-}
data S0 = S0
  { s0_x :: BG.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/objc_boxable.h 12:51@

         __exported by:__ @attributes\/objc_boxable.h@
    -}
  , s0_y :: BG.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @attributes\/objc_boxable.h 12:61@

         __exported by:__ @attributes\/objc_boxable.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S0 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw S0 where

  readRaw =
    \ptr0 ->
          pure S0
      <*> HasCField.readRaw (BG.Proxy @"s0_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s0_y") ptr0

instance Marshal.WriteRaw S0 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S0 s0_x2 s0_y3 ->
               HasCField.writeRaw (BG.Proxy @"s0_x") ptr0 s0_x2
            >> HasCField.writeRaw (BG.Proxy @"s0_y") ptr0 s0_y3

deriving via Marshal.EquivStorable S0 instance BG.Storable S0

deriving via Struct.IsStructViaReadRaw S0 instance Struct.IsStruct S0

{-| __C declaration:__ @x@

    __defined at:__ @attributes\/objc_boxable.h 12:51@

    __exported by:__ @attributes\/objc_boxable.h@
-}
instance (ty ~ BG.CDouble) => BG.CompatHasField.HasField "s0_x" S0 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S0 {s0_x = y1, s0_y = BG.getField @"s0_y" x0}
      , BG.getField @"s0_x" x0
      )

instance (ty ~ BG.CDouble) => BG.HasField "s0_x" (BG.Ptr S0) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s0_x")

instance HasCField.HasCField S0 "s0_x" where

  type CFieldType S0 "s0_x" = BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @attributes\/objc_boxable.h 12:61@

    __exported by:__ @attributes\/objc_boxable.h@
-}
instance (ty ~ BG.CDouble) => BG.CompatHasField.HasField "s0_y" S0 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S0 {s0_y = y1, s0_x = BG.getField @"s0_x" x0}
      , BG.getField @"s0_y" x0
      )

instance (ty ~ BG.CDouble) => BG.HasField "s0_y" (BG.Ptr S0) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s0_y")

instance HasCField.HasCField S0 "s0_y" where

  type CFieldType S0 "s0_y" = BG.CDouble

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct S1@

    __defined at:__ @attributes\/objc_boxable.h 15:8@

    __exported by:__ @attributes\/objc_boxable.h@
-}
data S1 = S1
  { s1_x :: BG.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/objc_boxable.h 15:20@

         __exported by:__ @attributes\/objc_boxable.h@
    -}
  , s1_y :: BG.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @attributes\/objc_boxable.h 15:30@

         __exported by:__ @attributes\/objc_boxable.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S1 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw S1 where

  readRaw =
    \ptr0 ->
          pure S1
      <*> HasCField.readRaw (BG.Proxy @"s1_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s1_y") ptr0

instance Marshal.WriteRaw S1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_x2 s1_y3 ->
               HasCField.writeRaw (BG.Proxy @"s1_x") ptr0 s1_x2
            >> HasCField.writeRaw (BG.Proxy @"s1_y") ptr0 s1_y3

deriving via Marshal.EquivStorable S1 instance BG.Storable S1

deriving via Struct.IsStructViaReadRaw S1 instance Struct.IsStruct S1

{-| __C declaration:__ @x@

    __defined at:__ @attributes\/objc_boxable.h 15:20@

    __exported by:__ @attributes\/objc_boxable.h@
-}
instance (ty ~ BG.CDouble) => BG.CompatHasField.HasField "s1_x" S1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S1 {s1_x = y1, s1_y = BG.getField @"s1_y" x0}
      , BG.getField @"s1_x" x0
      )

instance (ty ~ BG.CDouble) => BG.HasField "s1_x" (BG.Ptr S1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s1_x")

instance HasCField.HasCField S1 "s1_x" where

  type CFieldType S1 "s1_x" = BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @attributes\/objc_boxable.h 15:30@

    __exported by:__ @attributes\/objc_boxable.h@
-}
instance (ty ~ BG.CDouble) => BG.CompatHasField.HasField "s1_y" S1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S1 {s1_y = y1, s1_x = BG.getField @"s1_x" x0}
      , BG.getField @"s1_y" x0
      )

instance (ty ~ BG.CDouble) => BG.HasField "s1_y" (BG.Ptr S1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s1_y")

instance HasCField.HasCField S1 "s1_y" where

  type CFieldType S1 "s1_y" = BG.CDouble

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @union U0@

    __defined at:__ @attributes\/objc_boxable.h 19:38@

    __exported by:__ @attributes\/objc_boxable.h@
-}
newtype U0 = U0
  { unwrapU0 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize U0

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw U0

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw U0

deriving via Marshal.EquivStorable U0 instance BG.Storable U0

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion U0

{-| __C declaration:__ @i@

    __defined at:__ @attributes\/objc_boxable.h 19:47@

    __exported by:__ @attributes\/objc_boxable.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "u0_i" U0 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @i@

    __defined at:__ @attributes\/objc_boxable.h 19:47@

    __exported by:__ @attributes\/objc_boxable.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "u0_i" U0 ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"u0_i" x0)

instance (ty ~ BG.CInt) => BG.HasField "u0_i" (BG.Ptr U0) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"u0_i")

instance HasCField.HasCField U0 "u0_i" where

  type CFieldType U0 "u0_i" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f@

    __defined at:__ @attributes\/objc_boxable.h 19:56@

    __exported by:__ @attributes\/objc_boxable.h@
-}
instance (ty ~ BG.CFloat) => BG.HasField "u0_f" U0 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @f@

    __defined at:__ @attributes\/objc_boxable.h 19:56@

    __exported by:__ @attributes\/objc_boxable.h@
-}
instance (ty ~ BG.CFloat) => BG.CompatHasField.HasField "u0_f" U0 ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"u0_f" x0)

instance (ty ~ BG.CFloat) => BG.HasField "u0_f" (BG.Ptr U0) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"u0_f")

instance HasCField.HasCField U0 "u0_f" where

  type CFieldType U0 "u0_f" = BG.CFloat

  offset# = \_ -> \_ -> 0
