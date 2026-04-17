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
    ( Example.S_y_x(..)
    , Example.S_y(..)
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct \@S_y_x@

    __defined at:__ @types\/anonymous\/edge-cases\/anon_in_nonanon.h 15:5@

    __exported by:__ @types\/anonymous\/edge-cases\/anon_in_nonanon.h@
-}
data S_y_x = S_y_x
  { s_y_x_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/anon_in_nonanon.h 16:11@

         __exported by:__ @types\/anonymous\/edge-cases\/anon_in_nonanon.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S_y_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S_y_x where

  readRaw =
    \ptr0 ->
          pure S_y_x
      <*> HasCField.readRaw (RIP.Proxy @"s_y_x_x") ptr0

instance Marshal.WriteRaw S_y_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S_y_x s_y_x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s_y_x_x") ptr0 s_y_x_x2

deriving via Marshal.EquivStorable S_y_x instance RIP.Storable S_y_x

instance HasCField.HasCField S_y_x "s_y_x_x" where

  type CFieldType S_y_x "s_y_x_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "s_y_x_x" (RIP.Ptr S_y_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s_y_x_x")

{-| __C declaration:__ @struct \@S_y@

    __defined at:__ @types\/anonymous\/edge-cases\/anon_in_nonanon.h 14:3@

    __exported by:__ @types\/anonymous\/edge-cases\/anon_in_nonanon.h@
-}
data S_y = S_y
  { s_y_x :: S_y_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/anon_in_nonanon.h 15:5@

         __exported by:__ @types\/anonymous\/edge-cases\/anon_in_nonanon.h@
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
      <*> HasCField.readRaw (RIP.Proxy @"s_y_x") ptr0

instance Marshal.WriteRaw S_y where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S_y s_y_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s_y_x") ptr0 s_y_x2

deriving via Marshal.EquivStorable S_y instance RIP.Storable S_y

instance HasCField.HasCField S_y "s_y_x" where

  type CFieldType S_y "s_y_x" = S_y_x

  offset# = \_ -> \_ -> 0

instance ((~) ty S_y_x) => RIP.HasField "s_y_x" (RIP.Ptr S_y) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s_y_x")

{-| __C declaration:__ @struct S@

    __defined at:__ @types\/anonymous\/edge-cases\/anon_in_nonanon.h 13:8@

    __exported by:__ @types\/anonymous\/edge-cases\/anon_in_nonanon.h@
-}
data S = S
  { s_y :: S_y
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/anon_in_nonanon.h 18:5@

         __exported by:__ @types\/anonymous\/edge-cases\/anon_in_nonanon.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (RIP.Proxy @"s_y") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_y2 ->
            HasCField.writeRaw (RIP.Proxy @"s_y") ptr0 s_y2

deriving via Marshal.EquivStorable S instance RIP.Storable S

instance HasCField.HasCField S "s_y" where

  type CFieldType S "s_y" = S_y

  offset# = \_ -> \_ -> 0

instance ((~) ty S_y) => RIP.HasField "s_y" (RIP.Ptr S) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s_y")
