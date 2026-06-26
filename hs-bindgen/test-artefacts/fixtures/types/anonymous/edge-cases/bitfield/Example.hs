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
    ( Example.S1_anon'y(..)
    , Example.S1(..)
    , Example.S2_anon'anon'y_anon'y(..)
    , Example.S2_anon'anon'y(..)
    , Example.S2(..)
    )
  where

import qualified HsBindgen.Runtime.BitfieldPtr as BitfieldPtr
import qualified HsBindgen.Runtime.HasCBitfield as HasCBitfield
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct \@S1_anon\'y@

    __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 13:3@

    __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
-}
data S1_anon'y = S1_anon'y
  { s1_anon'y_y :: RIP.CChar
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 14:10@

         __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S1_anon'y where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw S1_anon'y where

  readRaw =
    \ptr0 ->
          pure S1_anon'y
      <*> HasCBitfield.peek (RIP.Proxy @"s1_anon'y_y") ptr0

instance Marshal.WriteRaw S1_anon'y where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1_anon'y s1_anon'y_y2 ->
            HasCBitfield.poke (RIP.Proxy @"s1_anon'y_y") ptr0 s1_anon'y_y2

deriving via Marshal.EquivStorable S1_anon'y instance RIP.Storable S1_anon'y

instance ( ty ~ RIP.CChar
         ) => RIP.CompatHasField.HasField "s1_anon'y_y" S1_anon'y ty where

  hasField =
    \x0 ->
      (\y1 ->
         S1_anon'y {s1_anon'y_y = y1}, RIP.getField @"s1_anon'y_y" x0)

instance ( ty ~ RIP.CChar
         ) => RIP.HasField "s1_anon'y_y" (RIP.Ptr S1_anon'y) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"s1_anon'y_y")

instance HasCBitfield.HasCBitfield S1_anon'y "s1_anon'y_y" where

  type CBitfieldType S1_anon'y "s1_anon'y_y" =
    RIP.CChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 3

{-| __C declaration:__ @struct S1@

    __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 12:8@

    __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
-}
data S1 = S1
  { s1_anon'y :: S1_anon'y
    {- ^ __C declaration:__ @anon\'y@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 13:3@

         __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
    -}
  , s1_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 16:7@

         __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S1 where

  readRaw =
    \ptr0 ->
          pure S1
      <*> HasCField.readRaw (RIP.Proxy @"s1_anon'y") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s1_x") ptr0

instance Marshal.WriteRaw S1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_anon'y2 s1_x3 ->
               HasCField.writeRaw (RIP.Proxy @"s1_anon'y") ptr0 s1_anon'y2
            >> HasCField.writeRaw (RIP.Proxy @"s1_x") ptr0 s1_x3

deriving via Marshal.EquivStorable S1 instance RIP.Storable S1

instance (ty ~ S1_anon'y) => RIP.CompatHasField.HasField "s1_anon'y" S1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S1 {s1_anon'y = y1, s1_x = RIP.getField @"s1_x" x0}
      , RIP.getField @"s1_anon'y" x0
      )

instance ( ty ~ S1_anon'y
         ) => RIP.HasField "s1_anon'y" (RIP.Ptr S1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s1_anon'y")

instance HasCField.HasCField S1 "s1_anon'y" where

  type CFieldType S1 "s1_anon'y" = S1_anon'y

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.CompatHasField.HasField "s1_x" S1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S1 {s1_x = y1, s1_anon'y = RIP.getField @"s1_anon'y" x0}
      , RIP.getField @"s1_x" x0
      )

instance (ty ~ RIP.CInt) => RIP.HasField "s1_x" (RIP.Ptr S1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s1_x")

instance HasCField.HasCField S1 "s1_x" where

  type CFieldType S1 "s1_x" = RIP.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct \@S2_anon\'anon\'y_anon\'y@

    __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 21:5@

    __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
-}
data S2_anon'anon'y_anon'y = S2_anon'anon'y_anon'y
  { s2_anon'anon'y_anon'y_y :: RIP.CChar
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 22:12@

         __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S2_anon'anon'y_anon'y where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw S2_anon'anon'y_anon'y where

  readRaw =
    \ptr0 ->
          pure S2_anon'anon'y_anon'y
      <*> HasCBitfield.peek (RIP.Proxy @"s2_anon'anon'y_anon'y_y") ptr0

instance Marshal.WriteRaw S2_anon'anon'y_anon'y where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_anon'anon'y_anon'y s2_anon'anon'y_anon'y_y2 ->
            HasCBitfield.poke (RIP.Proxy @"s2_anon'anon'y_anon'y_y") ptr0 s2_anon'anon'y_anon'y_y2

deriving via Marshal.EquivStorable S2_anon'anon'y_anon'y instance RIP.Storable S2_anon'anon'y_anon'y

instance ( ty ~ RIP.CChar
         ) => RIP.CompatHasField.HasField "s2_anon'anon'y_anon'y_y" S2_anon'anon'y_anon'y ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2_anon'anon'y_anon'y {s2_anon'anon'y_anon'y_y = y1}
      , RIP.getField @"s2_anon'anon'y_anon'y_y" x0
      )

instance ( ty ~ RIP.CChar
         ) => RIP.HasField "s2_anon'anon'y_anon'y_y" (RIP.Ptr S2_anon'anon'y_anon'y) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"s2_anon'anon'y_anon'y_y")

instance HasCBitfield.HasCBitfield S2_anon'anon'y_anon'y "s2_anon'anon'y_anon'y_y" where

  type CBitfieldType S2_anon'anon'y_anon'y "s2_anon'anon'y_anon'y_y" =
    RIP.CChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 3

{-| __C declaration:__ @struct \@S2_anon\'anon\'y@

    __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 20:3@

    __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
-}
data S2_anon'anon'y = S2_anon'anon'y
  { s2_anon'anon'y_anon'y :: S2_anon'anon'y_anon'y
    {- ^ __C declaration:__ @anon\'y@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 21:5@

         __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
    -}
  , s2_anon'anon'y_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 24:9@

         __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S2_anon'anon'y where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2_anon'anon'y where

  readRaw =
    \ptr0 ->
          pure S2_anon'anon'y
      <*> HasCField.readRaw (RIP.Proxy @"s2_anon'anon'y_anon'y") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s2_anon'anon'y_x") ptr0

instance Marshal.WriteRaw S2_anon'anon'y where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_anon'anon'y s2_anon'anon'y_anon'y2 s2_anon'anon'y_x3 ->
               HasCField.writeRaw (RIP.Proxy @"s2_anon'anon'y_anon'y") ptr0 s2_anon'anon'y_anon'y2
            >> HasCField.writeRaw (RIP.Proxy @"s2_anon'anon'y_x") ptr0 s2_anon'anon'y_x3

deriving via Marshal.EquivStorable S2_anon'anon'y instance RIP.Storable S2_anon'anon'y

instance ( ty ~ S2_anon'anon'y_anon'y
         ) => RIP.CompatHasField.HasField "s2_anon'anon'y_anon'y" S2_anon'anon'y ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2_anon'anon'y { s2_anon'anon'y_anon'y = y1
                         , s2_anon'anon'y_x = RIP.getField @"s2_anon'anon'y_x" x0
                         }
      , RIP.getField @"s2_anon'anon'y_anon'y" x0
      )

instance ( ty ~ S2_anon'anon'y_anon'y
         ) => RIP.HasField "s2_anon'anon'y_anon'y" (RIP.Ptr S2_anon'anon'y) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"s2_anon'anon'y_anon'y")

instance HasCField.HasCField S2_anon'anon'y "s2_anon'anon'y_anon'y" where

  type CFieldType S2_anon'anon'y "s2_anon'anon'y_anon'y" =
    S2_anon'anon'y_anon'y

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "s2_anon'anon'y_x" S2_anon'anon'y ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2_anon'anon'y { s2_anon'anon'y_x = y1
                         , s2_anon'anon'y_anon'y = RIP.getField @"s2_anon'anon'y_anon'y" x0
                         }
      , RIP.getField @"s2_anon'anon'y_x" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "s2_anon'anon'y_x" (RIP.Ptr S2_anon'anon'y) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"s2_anon'anon'y_x")

instance HasCField.HasCField S2_anon'anon'y "s2_anon'anon'y_x" where

  type CFieldType S2_anon'anon'y "s2_anon'anon'y_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct S2@

    __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 19:8@

    __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
-}
data S2 = S2
  { s2_anon'anon'y :: S2_anon'anon'y
    {- ^ __C declaration:__ @anon\'anon\'y@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 20:3@

         __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S2 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2 where

  readRaw =
    \ptr0 ->
          pure S2
      <*> HasCField.readRaw (RIP.Proxy @"s2_anon'anon'y") ptr0

instance Marshal.WriteRaw S2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_anon'anon'y2 ->
            HasCField.writeRaw (RIP.Proxy @"s2_anon'anon'y") ptr0 s2_anon'anon'y2

deriving via Marshal.EquivStorable S2 instance RIP.Storable S2

instance ( ty ~ S2_anon'anon'y
         ) => RIP.CompatHasField.HasField "s2_anon'anon'y" S2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         S2 {s2_anon'anon'y = y1}, RIP.getField @"s2_anon'anon'y" x0)

instance ( ty ~ S2_anon'anon'y
         ) => RIP.HasField "s2_anon'anon'y" (RIP.Ptr S2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"s2_anon'anon'y")

instance HasCField.HasCField S2 "s2_anon'anon'y" where

  type CFieldType S2 "s2_anon'anon'y" = S2_anon'anon'y

  offset# = \_ -> \_ -> 0
