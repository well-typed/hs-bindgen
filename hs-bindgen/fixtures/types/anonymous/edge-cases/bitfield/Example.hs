{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.S1_y(..)
    , Example.S1(..)
    , Example.S2_y_y(..)
    , Example.S2_y(..)
    , Example.S2(..)
    )
  where

import qualified HsBindgen.Runtime.BitfieldPtr as BitfieldPtr
import qualified HsBindgen.Runtime.HasCBitfield as HasCBitfield
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct \@S1_y@

    __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 14:3@

    __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
-}
data S1_y = S1_y
  { s1_y_y :: RIP.CChar
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 15:10@

         __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S1_y where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw S1_y where

  readRaw =
    \ptr0 ->
          pure S1_y
      <*> HasCBitfield.peek (RIP.Proxy @"s1_y_y") ptr0

instance Marshal.WriteRaw S1_y where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1_y s1_y_y2 ->
            HasCBitfield.poke (RIP.Proxy @"s1_y_y") ptr0 s1_y_y2

deriving via Marshal.EquivStorable S1_y instance RIP.Storable S1_y

instance HasCBitfield.HasCBitfield S1_y "s1_y_y" where

  type CBitfieldType S1_y "s1_y_y" = RIP.CChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 3

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "s1_y_y" (RIP.Ptr S1_y) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"s1_y_y")

{-| __C declaration:__ @struct S1@

    __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 13:8@

    __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
-}
data S1 = S1
  { s1_y :: S1_y
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 14:3@

         __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
    -}
  , s1_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 17:7@

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
      <*> HasCField.readRaw (RIP.Proxy @"s1_y") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s1_x") ptr0

instance Marshal.WriteRaw S1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_y2 s1_x3 ->
               HasCField.writeRaw (RIP.Proxy @"s1_y") ptr0 s1_y2
            >> HasCField.writeRaw (RIP.Proxy @"s1_x") ptr0 s1_x3

deriving via Marshal.EquivStorable S1 instance RIP.Storable S1

instance HasCField.HasCField S1 "s1_y" where

  type CFieldType S1 "s1_y" = S1_y

  offset# = \_ -> \_ -> 0

instance (((~) ty) S1_y) => RIP.HasField "s1_y" (RIP.Ptr S1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s1_y")

instance HasCField.HasCField S1 "s1_x" where

  type CFieldType S1 "s1_x" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s1_x" (RIP.Ptr S1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s1_x")

{-| __C declaration:__ @struct \@S2_y_y@

    __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 22:5@

    __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
-}
data S2_y_y = S2_y_y
  { s2_y_y_y :: RIP.CChar
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 23:12@

         __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S2_y_y where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw S2_y_y where

  readRaw =
    \ptr0 ->
          pure S2_y_y
      <*> HasCBitfield.peek (RIP.Proxy @"s2_y_y_y") ptr0

instance Marshal.WriteRaw S2_y_y where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_y_y s2_y_y_y2 ->
            HasCBitfield.poke (RIP.Proxy @"s2_y_y_y") ptr0 s2_y_y_y2

deriving via Marshal.EquivStorable S2_y_y instance RIP.Storable S2_y_y

instance HasCBitfield.HasCBitfield S2_y_y "s2_y_y_y" where

  type CBitfieldType S2_y_y "s2_y_y_y" = RIP.CChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 3

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "s2_y_y_y" (RIP.Ptr S2_y_y) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"s2_y_y_y")

{-| __C declaration:__ @struct \@S2_y@

    __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 21:3@

    __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
-}
data S2_y = S2_y
  { s2_y_y :: S2_y_y
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 22:5@

         __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
    -}
  , s2_y_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 25:9@

         __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S2_y where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2_y where

  readRaw =
    \ptr0 ->
          pure S2_y
      <*> HasCField.readRaw (RIP.Proxy @"s2_y_y") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s2_y_x") ptr0

instance Marshal.WriteRaw S2_y where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_y s2_y_y2 s2_y_x3 ->
               HasCField.writeRaw (RIP.Proxy @"s2_y_y") ptr0 s2_y_y2
            >> HasCField.writeRaw (RIP.Proxy @"s2_y_x") ptr0 s2_y_x3

deriving via Marshal.EquivStorable S2_y instance RIP.Storable S2_y

instance HasCField.HasCField S2_y "s2_y_y" where

  type CFieldType S2_y "s2_y_y" = S2_y_y

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) S2_y_y
         ) => RIP.HasField "s2_y_y" (RIP.Ptr S2_y) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s2_y_y")

instance HasCField.HasCField S2_y "s2_y_x" where

  type CFieldType S2_y "s2_y_x" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s2_y_x" (RIP.Ptr S2_y) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s2_y_x")

{-| __C declaration:__ @struct S2@

    __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 20:8@

    __exported by:__ @types\/anonymous\/edge-cases\/bitfield.h@
-}
data S2 = S2
  { s2_y :: S2_y
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/bitfield.h 21:3@

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
      <*> HasCField.readRaw (RIP.Proxy @"s2_y") ptr0

instance Marshal.WriteRaw S2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_y2 ->
            HasCField.writeRaw (RIP.Proxy @"s2_y") ptr0 s2_y2

deriving via Marshal.EquivStorable S2 instance RIP.Storable S2

instance HasCField.HasCField S2 "s2_y" where

  type CFieldType S2 "s2_y" = S2_y

  offset# = \_ -> \_ -> 0

instance (((~) ty) S2_y) => RIP.HasField "s2_y" (RIP.Ptr S2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s2_y")
