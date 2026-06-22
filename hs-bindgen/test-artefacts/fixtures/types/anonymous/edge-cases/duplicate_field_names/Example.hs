{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.S_y_anon'x(..)
    , Example.S_y(..)
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct \@S_y_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h 16:5@

    __exported by:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h@
-}
data S_y_anon'x = S_y_anon'x
  { s_y_anon'x_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h 17:11@

         __exported by:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S_y_anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S_y_anon'x where

  readRaw =
    \ptr0 ->
          pure S_y_anon'x
      <*> HasCField.readRaw (RIP.Proxy @"s_y_anon'x_x") ptr0

instance Marshal.WriteRaw S_y_anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S_y_anon'x s_y_anon'x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s_y_anon'x_x") ptr0 s_y_anon'x_x2

deriving via Marshal.EquivStorable S_y_anon'x instance RIP.Storable S_y_anon'x

instance HasCField.HasCField S_y_anon'x "s_y_anon'x_x" where

  type CFieldType S_y_anon'x "s_y_anon'x_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "s_y_anon'x_x" (RIP.Ptr S_y_anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"s_y_anon'x_x")

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "s_y_anon'x_x" S_y_anon'x ty where

  hasField =
    \x0 ->
      (\y1 ->
         S_y_anon'x {s_y_anon'x_x = y1}, RIP.getField @"s_y_anon'x_x" x0)

{-| __C declaration:__ @struct \@S_y@

    __defined at:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h 15:3@

    __exported by:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h@
-}
data S_y = S_y
  { s_y_anon'x :: S_y_anon'x
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h 16:5@

         __exported by:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S_y where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S_y where

  readRaw =
    \ptr0 ->
          pure S_y
      <*> HasCField.readRaw (RIP.Proxy @"s_y_anon'x") ptr0

instance Marshal.WriteRaw S_y where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S_y s_y_anon'x2 ->
            HasCField.writeRaw (RIP.Proxy @"s_y_anon'x") ptr0 s_y_anon'x2

deriving via Marshal.EquivStorable S_y instance RIP.Storable S_y

instance HasCField.HasCField S_y "s_y_anon'x" where

  type CFieldType S_y "s_y_anon'x" = S_y_anon'x

  offset# = \_ -> \_ -> 0

instance ( ty ~ S_y_anon'x
         ) => RIP.HasField "s_y_anon'x" (RIP.Ptr S_y) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"s_y_anon'x")

instance ( ty ~ S_y_anon'x
         ) => RIP.CompatHasField.HasField "s_y_anon'x" S_y ty where

  hasField =
    \x0 ->
      (\y1 ->
         S_y {s_y_anon'x = y1}, RIP.getField @"s_y_anon'x" x0)

{-| __C declaration:__ @struct S@

    __defined at:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h 13:8@

    __exported by:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h@
-}
data S = S
  { s_x :: RIP.CULLong
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h 14:22@

         __exported by:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h@
    -}
  , s_y :: S_y
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h 19:5@

         __exported by:__ @types\/anonymous\/edge-cases\/duplicate_field_names.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (RIP.Proxy @"s_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s_y") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_x2 s_y3 ->
               HasCField.writeRaw (RIP.Proxy @"s_x") ptr0 s_x2
            >> HasCField.writeRaw (RIP.Proxy @"s_y") ptr0 s_y3

deriving via Marshal.EquivStorable S instance RIP.Storable S

instance HasCField.HasCField S "s_x" where

  type CFieldType S "s_x" = RIP.CULLong

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CULLong) => RIP.HasField "s_x" (RIP.Ptr S) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s_x")

instance (ty ~ RIP.CULLong) => RIP.CompatHasField.HasField "s_x" S ty where

  hasField =
    \x0 ->
      (\y1 ->
         S {s_x = y1, s_y = RIP.getField @"s_y" x0}, RIP.getField @"s_x" x0)

instance HasCField.HasCField S "s_y" where

  type CFieldType S "s_y" = S_y

  offset# = \_ -> \_ -> 8

instance (ty ~ S_y) => RIP.HasField "s_y" (RIP.Ptr S) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s_y")

instance (ty ~ S_y) => RIP.CompatHasField.HasField "s_y" S ty where

  hasField =
    \x0 ->
      (\y1 ->
         S {s_y = y1, s_x = RIP.getField @"s_x" x0}, RIP.getField @"s_y" x0)
