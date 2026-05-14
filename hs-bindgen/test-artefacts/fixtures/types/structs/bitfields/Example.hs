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
    ( Example.Foo_8(..)
    , Example.Foo_16(..)
    , Example.Foo_32(..)
    , Example.Foo_64(..)
    , Example.Bar_8_8(..)
    , Example.Bar_8_16(..)
    , Example.Bar_8_32(..)
    , Example.Bar_8_64(..)
    , Example.Bar_16_16(..)
    , Example.Bar_16_32(..)
    , Example.Bar_16_64(..)
    , Example.Bar_32_32(..)
    , Example.Bar_32_64(..)
    , Example.Bar_64_64(..)
    )
  where

import qualified HsBindgen.Runtime.BitfieldPtr as BitfieldPtr
import qualified HsBindgen.Runtime.HasCBitfield as HasCBitfield
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct foo_8@

    __defined at:__ @types\/structs\/bitfields.h 5:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Foo_8 = Foo_8
  { foo_8_a :: RIP.CSChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 6:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_8_b :: RIP.CSChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 7:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_8_c :: RIP.CSChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/bitfields.h 8:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_8_d :: RIP.CSChar
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/bitfields.h 9:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_8_e :: RIP.CSChar
    {- ^ __C declaration:__ @e@

         __defined at:__ @types\/structs\/bitfields.h 10:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_8_f :: RIP.CSChar
    {- ^ __C declaration:__ @f@

         __defined at:__ @types\/structs\/bitfields.h 11:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo_8 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Foo_8 where

  readRaw =
    \ptr0 ->
          pure Foo_8
      <*> HasCBitfield.peek (RIP.Proxy @"foo_8_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_8_b") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_8_c") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_8_d") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_8_e") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_8_f") ptr0

instance Marshal.WriteRaw Foo_8 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_8 foo_8_a2 foo_8_b3 foo_8_c4 foo_8_d5 foo_8_e6 foo_8_f7 ->
               HasCBitfield.poke (RIP.Proxy @"foo_8_a") ptr0 foo_8_a2
            >> HasCBitfield.poke (RIP.Proxy @"foo_8_b") ptr0 foo_8_b3
            >> HasCBitfield.poke (RIP.Proxy @"foo_8_c") ptr0 foo_8_c4
            >> HasCBitfield.poke (RIP.Proxy @"foo_8_d") ptr0 foo_8_d5
            >> HasCBitfield.poke (RIP.Proxy @"foo_8_e") ptr0 foo_8_e6
            >> HasCBitfield.poke (RIP.Proxy @"foo_8_f") ptr0 foo_8_f7

deriving via Marshal.EquivStorable Foo_8 instance RIP.Storable Foo_8

instance HasCBitfield.HasCBitfield Foo_8 "foo_8_a" where

  type CBitfieldType Foo_8 "foo_8_a" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 3

instance ( ty ~ RIP.CSChar
         ) => RIP.HasField "foo_8_a" (RIP.Ptr Foo_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_8_a")

instance HasCBitfield.HasCBitfield Foo_8 "foo_8_b" where

  type CBitfieldType Foo_8 "foo_8_b" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 3

  bitfieldWidth# = \_ -> \_ -> 3

instance ( ty ~ RIP.CSChar
         ) => RIP.HasField "foo_8_b" (RIP.Ptr Foo_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_8_b")

instance HasCBitfield.HasCBitfield Foo_8 "foo_8_c" where

  type CBitfieldType Foo_8 "foo_8_c" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 6

  bitfieldWidth# = \_ -> \_ -> 2

instance ( ty ~ RIP.CSChar
         ) => RIP.HasField "foo_8_c" (RIP.Ptr Foo_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_8_c")

instance HasCBitfield.HasCBitfield Foo_8 "foo_8_d" where

  type CBitfieldType Foo_8 "foo_8_d" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 8

  bitfieldWidth# = \_ -> \_ -> 3

instance ( ty ~ RIP.CSChar
         ) => RIP.HasField "foo_8_d" (RIP.Ptr Foo_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_8_d")

instance HasCBitfield.HasCBitfield Foo_8 "foo_8_e" where

  type CBitfieldType Foo_8 "foo_8_e" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 16

  bitfieldWidth# = \_ -> \_ -> 8

instance ( ty ~ RIP.CSChar
         ) => RIP.HasField "foo_8_e" (RIP.Ptr Foo_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_8_e")

instance HasCBitfield.HasCBitfield Foo_8 "foo_8_f" where

  type CBitfieldType Foo_8 "foo_8_f" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 24

  bitfieldWidth# = \_ -> \_ -> 5

instance ( ty ~ RIP.CSChar
         ) => RIP.HasField "foo_8_f" (RIP.Ptr Foo_8) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_8_f")

{-| __C declaration:__ @struct foo_16@

    __defined at:__ @types\/structs\/bitfields.h 15:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Foo_16 = Foo_16
  { foo_16_a :: RIP.CSChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 16:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_16_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 17:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_16_c :: RIP.CInt
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/bitfields.h 18:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_16_d :: RIP.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/bitfields.h 19:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_16_e :: RIP.CInt
    {- ^ __C declaration:__ @e@

         __defined at:__ @types\/structs\/bitfields.h 20:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_16_f :: RIP.CInt
    {- ^ __C declaration:__ @f@

         __defined at:__ @types\/structs\/bitfields.h 21:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo_16 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo_16 where

  readRaw =
    \ptr0 ->
          pure Foo_16
      <*> HasCBitfield.peek (RIP.Proxy @"foo_16_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_16_b") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_16_c") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_16_d") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_16_e") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_16_f") ptr0

instance Marshal.WriteRaw Foo_16 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_16 foo_16_a2 foo_16_b3 foo_16_c4 foo_16_d5 foo_16_e6 foo_16_f7 ->
               HasCBitfield.poke (RIP.Proxy @"foo_16_a") ptr0 foo_16_a2
            >> HasCBitfield.poke (RIP.Proxy @"foo_16_b") ptr0 foo_16_b3
            >> HasCBitfield.poke (RIP.Proxy @"foo_16_c") ptr0 foo_16_c4
            >> HasCBitfield.poke (RIP.Proxy @"foo_16_d") ptr0 foo_16_d5
            >> HasCBitfield.poke (RIP.Proxy @"foo_16_e") ptr0 foo_16_e6
            >> HasCBitfield.poke (RIP.Proxy @"foo_16_f") ptr0 foo_16_f7

deriving via Marshal.EquivStorable Foo_16 instance RIP.Storable Foo_16

instance HasCBitfield.HasCBitfield Foo_16 "foo_16_a" where

  type CBitfieldType Foo_16 "foo_16_a" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 6

instance ( ty ~ RIP.CSChar
         ) => RIP.HasField "foo_16_a" (RIP.Ptr Foo_16) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_16_a")

instance HasCBitfield.HasCBitfield Foo_16 "foo_16_b" where

  type CBitfieldType Foo_16 "foo_16_b" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 6

  bitfieldWidth# = \_ -> \_ -> 10

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "foo_16_b" (RIP.Ptr Foo_16) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_16_b")

instance HasCBitfield.HasCBitfield Foo_16 "foo_16_c" where

  type CBitfieldType Foo_16 "foo_16_c" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 16

  bitfieldWidth# = \_ -> \_ -> 16

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "foo_16_c" (RIP.Ptr Foo_16) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_16_c")

instance HasCBitfield.HasCBitfield Foo_16 "foo_16_d" where

  type CBitfieldType Foo_16 "foo_16_d" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 32

  bitfieldWidth# = \_ -> \_ -> 16

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "foo_16_d" (RIP.Ptr Foo_16) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_16_d")

instance HasCBitfield.HasCBitfield Foo_16 "foo_16_e" where

  type CBitfieldType Foo_16 "foo_16_e" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 48

  bitfieldWidth# = \_ -> \_ -> 12

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "foo_16_e" (RIP.Ptr Foo_16) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_16_e")

instance HasCBitfield.HasCBitfield Foo_16 "foo_16_f" where

  type CBitfieldType Foo_16 "foo_16_f" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 64

  bitfieldWidth# = \_ -> \_ -> 12

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "foo_16_f" (RIP.Ptr Foo_16) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_16_f")

{-| __C declaration:__ @struct foo_32@

    __defined at:__ @types\/structs\/bitfields.h 25:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Foo_32 = Foo_32
  { foo_32_a :: RIP.CSChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 26:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_32_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 27:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_32_c :: RIP.CInt
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/bitfields.h 28:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_32_d :: RIP.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/bitfields.h 29:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_32_e :: RIP.CLong
    {- ^ __C declaration:__ @e@

         __defined at:__ @types\/structs\/bitfields.h 30:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_32_f :: RIP.CInt
    {- ^ __C declaration:__ @f@

         __defined at:__ @types\/structs\/bitfields.h 31:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_32_g :: RIP.CLong
    {- ^ __C declaration:__ @g@

         __defined at:__ @types\/structs\/bitfields.h 32:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo_32 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Foo_32 where

  readRaw =
    \ptr0 ->
          pure Foo_32
      <*> HasCBitfield.peek (RIP.Proxy @"foo_32_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_32_b") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_32_c") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_32_d") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_32_e") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_32_f") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_32_g") ptr0

instance Marshal.WriteRaw Foo_32 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_32 foo_32_a2 foo_32_b3 foo_32_c4 foo_32_d5 foo_32_e6 foo_32_f7 foo_32_g8 ->
               HasCBitfield.poke (RIP.Proxy @"foo_32_a") ptr0 foo_32_a2
            >> HasCBitfield.poke (RIP.Proxy @"foo_32_b") ptr0 foo_32_b3
            >> HasCBitfield.poke (RIP.Proxy @"foo_32_c") ptr0 foo_32_c4
            >> HasCBitfield.poke (RIP.Proxy @"foo_32_d") ptr0 foo_32_d5
            >> HasCBitfield.poke (RIP.Proxy @"foo_32_e") ptr0 foo_32_e6
            >> HasCBitfield.poke (RIP.Proxy @"foo_32_f") ptr0 foo_32_f7
            >> HasCBitfield.poke (RIP.Proxy @"foo_32_g") ptr0 foo_32_g8

deriving via Marshal.EquivStorable Foo_32 instance RIP.Storable Foo_32

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_a" where

  type CBitfieldType Foo_32 "foo_32_a" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 6

instance ( ty ~ RIP.CSChar
         ) => RIP.HasField "foo_32_a" (RIP.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_32_a")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_b" where

  type CBitfieldType Foo_32 "foo_32_b" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 6

  bitfieldWidth# = \_ -> \_ -> 12

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "foo_32_b" (RIP.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_32_b")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_c" where

  type CBitfieldType Foo_32 "foo_32_c" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 18

  bitfieldWidth# = \_ -> \_ -> 14

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "foo_32_c" (RIP.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_32_c")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_d" where

  type CBitfieldType Foo_32 "foo_32_d" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 32

  bitfieldWidth# = \_ -> \_ -> 10

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "foo_32_d" (RIP.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_32_d")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_e" where

  type CBitfieldType Foo_32 "foo_32_e" = RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 64

  bitfieldWidth# = \_ -> \_ -> 32

instance ( ty ~ RIP.CLong
         ) => RIP.HasField "foo_32_e" (RIP.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_32_e")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_f" where

  type CBitfieldType Foo_32 "foo_32_f" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 96

  bitfieldWidth# = \_ -> \_ -> 6

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "foo_32_f" (RIP.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_32_f")

instance HasCBitfield.HasCBitfield Foo_32 "foo_32_g" where

  type CBitfieldType Foo_32 "foo_32_g" = RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 102

  bitfieldWidth# = \_ -> \_ -> 24

instance ( ty ~ RIP.CLong
         ) => RIP.HasField "foo_32_g" (RIP.Ptr Foo_32) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_32_g")

{-| __C declaration:__ @struct foo_64@

    __defined at:__ @types\/structs\/bitfields.h 36:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Foo_64 = Foo_64
  { foo_64_a :: RIP.CLong
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 37:20@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_64_b :: RIP.CLLong
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 38:20@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_64_c :: RIP.CLLong
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/bitfields.h 39:20@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , foo_64_d :: RIP.CLLong
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/bitfields.h 40:20@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo_64 where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Foo_64 where

  readRaw =
    \ptr0 ->
          pure Foo_64
      <*> HasCBitfield.peek (RIP.Proxy @"foo_64_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_64_b") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_64_c") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_64_d") ptr0

instance Marshal.WriteRaw Foo_64 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_64 foo_64_a2 foo_64_b3 foo_64_c4 foo_64_d5 ->
               HasCBitfield.poke (RIP.Proxy @"foo_64_a") ptr0 foo_64_a2
            >> HasCBitfield.poke (RIP.Proxy @"foo_64_b") ptr0 foo_64_b3
            >> HasCBitfield.poke (RIP.Proxy @"foo_64_c") ptr0 foo_64_c4
            >> HasCBitfield.poke (RIP.Proxy @"foo_64_d") ptr0 foo_64_d5

deriving via Marshal.EquivStorable Foo_64 instance RIP.Storable Foo_64

instance HasCBitfield.HasCBitfield Foo_64 "foo_64_a" where

  type CBitfieldType Foo_64 "foo_64_a" = RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 24

instance ( ty ~ RIP.CLong
         ) => RIP.HasField "foo_64_a" (RIP.Ptr Foo_64) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_64_a")

instance HasCBitfield.HasCBitfield Foo_64 "foo_64_b" where

  type CBitfieldType Foo_64 "foo_64_b" = RIP.CLLong

  bitfieldOffset# = \_ -> \_ -> 24

  bitfieldWidth# = \_ -> \_ -> 40

instance ( ty ~ RIP.CLLong
         ) => RIP.HasField "foo_64_b" (RIP.Ptr Foo_64) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_64_b")

instance HasCBitfield.HasCBitfield Foo_64 "foo_64_c" where

  type CBitfieldType Foo_64 "foo_64_c" = RIP.CLLong

  bitfieldOffset# = \_ -> \_ -> 64

  bitfieldWidth# = \_ -> \_ -> 64

instance ( ty ~ RIP.CLLong
         ) => RIP.HasField "foo_64_c" (RIP.Ptr Foo_64) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_64_c")

instance HasCBitfield.HasCBitfield Foo_64 "foo_64_d" where

  type CBitfieldType Foo_64 "foo_64_d" = RIP.CLLong

  bitfieldOffset# = \_ -> \_ -> 128

  bitfieldWidth# = \_ -> \_ -> 36

instance ( ty ~ RIP.CLLong
         ) => RIP.HasField "foo_64_d" (RIP.Ptr Foo_64) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_64_d")

{-| __C declaration:__ @struct bar_8_8@

    __defined at:__ @types\/structs\/bitfields.h 44:32@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Bar_8_8 = Bar_8_8
  { bar_8_8_a :: RIP.CSChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 45:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , bar_8_8_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 46:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar_8_8 where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar_8_8 where

  readRaw =
    \ptr0 ->
          pure Bar_8_8
      <*> HasCBitfield.peek (RIP.Proxy @"bar_8_8_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bar_8_8_b") ptr0

instance Marshal.WriteRaw Bar_8_8 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar_8_8 bar_8_8_a2 bar_8_8_b3 ->
               HasCBitfield.poke (RIP.Proxy @"bar_8_8_a") ptr0 bar_8_8_a2
            >> HasCBitfield.poke (RIP.Proxy @"bar_8_8_b") ptr0 bar_8_8_b3

deriving via Marshal.EquivStorable Bar_8_8 instance RIP.Storable Bar_8_8

instance HasCBitfield.HasCBitfield Bar_8_8 "bar_8_8_a" where

  type CBitfieldType Bar_8_8 "bar_8_8_a" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 6

instance ( ty ~ RIP.CSChar
         ) => RIP.HasField "bar_8_8_a" (RIP.Ptr Bar_8_8) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_8_8_a")

instance HasCBitfield.HasCBitfield Bar_8_8 "bar_8_8_b" where

  type CBitfieldType Bar_8_8 "bar_8_8_b" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 6

  bitfieldWidth# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "bar_8_8_b" (RIP.Ptr Bar_8_8) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_8_8_b")

{-| __C declaration:__ @struct bar_8_16@

    __defined at:__ @types\/structs\/bitfields.h 50:32@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Bar_8_16 = Bar_8_16
  { bar_8_16_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 51:14@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , bar_8_16_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 52:14@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar_8_16 where

  staticSizeOf = \_ -> (3 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar_8_16 where

  readRaw =
    \ptr0 ->
          pure Bar_8_16
      <*> HasCBitfield.peek (RIP.Proxy @"bar_8_16_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bar_8_16_b") ptr0

instance Marshal.WriteRaw Bar_8_16 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar_8_16 bar_8_16_a2 bar_8_16_b3 ->
               HasCBitfield.poke (RIP.Proxy @"bar_8_16_a") ptr0 bar_8_16_a2
            >> HasCBitfield.poke (RIP.Proxy @"bar_8_16_b") ptr0 bar_8_16_b3

deriving via Marshal.EquivStorable Bar_8_16 instance RIP.Storable Bar_8_16

instance HasCBitfield.HasCBitfield Bar_8_16 "bar_8_16_a" where

  type CBitfieldType Bar_8_16 "bar_8_16_a" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 14

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "bar_8_16_a" (RIP.Ptr Bar_8_16) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_8_16_a")

instance HasCBitfield.HasCBitfield Bar_8_16 "bar_8_16_b" where

  type CBitfieldType Bar_8_16 "bar_8_16_b" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 14

  bitfieldWidth# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "bar_8_16_b" (RIP.Ptr Bar_8_16) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_8_16_b")

{-| __C declaration:__ @struct bar_8_32@

    __defined at:__ @types\/structs\/bitfields.h 56:32@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Bar_8_32 = Bar_8_32
  { bar_8_32_a :: RIP.CLong
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 57:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , bar_8_32_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 58:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar_8_32 where

  staticSizeOf = \_ -> (5 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar_8_32 where

  readRaw =
    \ptr0 ->
          pure Bar_8_32
      <*> HasCBitfield.peek (RIP.Proxy @"bar_8_32_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bar_8_32_b") ptr0

instance Marshal.WriteRaw Bar_8_32 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar_8_32 bar_8_32_a2 bar_8_32_b3 ->
               HasCBitfield.poke (RIP.Proxy @"bar_8_32_a") ptr0 bar_8_32_a2
            >> HasCBitfield.poke (RIP.Proxy @"bar_8_32_b") ptr0 bar_8_32_b3

deriving via Marshal.EquivStorable Bar_8_32 instance RIP.Storable Bar_8_32

instance HasCBitfield.HasCBitfield Bar_8_32 "bar_8_32_a" where

  type CBitfieldType Bar_8_32 "bar_8_32_a" = RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 30

instance ( ty ~ RIP.CLong
         ) => RIP.HasField "bar_8_32_a" (RIP.Ptr Bar_8_32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_8_32_a")

instance HasCBitfield.HasCBitfield Bar_8_32 "bar_8_32_b" where

  type CBitfieldType Bar_8_32 "bar_8_32_b" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 30

  bitfieldWidth# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "bar_8_32_b" (RIP.Ptr Bar_8_32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_8_32_b")

{-| __C declaration:__ @struct bar_8_64@

    __defined at:__ @types\/structs\/bitfields.h 62:32@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Bar_8_64 = Bar_8_64
  { bar_8_64_a :: RIP.CLLong
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 63:20@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , bar_8_64_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 64:20@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar_8_64 where

  staticSizeOf = \_ -> (9 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar_8_64 where

  readRaw =
    \ptr0 ->
          pure Bar_8_64
      <*> HasCBitfield.peek (RIP.Proxy @"bar_8_64_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bar_8_64_b") ptr0

instance Marshal.WriteRaw Bar_8_64 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar_8_64 bar_8_64_a2 bar_8_64_b3 ->
               HasCBitfield.poke (RIP.Proxy @"bar_8_64_a") ptr0 bar_8_64_a2
            >> HasCBitfield.poke (RIP.Proxy @"bar_8_64_b") ptr0 bar_8_64_b3

deriving via Marshal.EquivStorable Bar_8_64 instance RIP.Storable Bar_8_64

instance HasCBitfield.HasCBitfield Bar_8_64 "bar_8_64_a" where

  type CBitfieldType Bar_8_64 "bar_8_64_a" = RIP.CLLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 62

instance ( ty ~ RIP.CLLong
         ) => RIP.HasField "bar_8_64_a" (RIP.Ptr Bar_8_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_8_64_a")

instance HasCBitfield.HasCBitfield Bar_8_64 "bar_8_64_b" where

  type CBitfieldType Bar_8_64 "bar_8_64_b" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 62

  bitfieldWidth# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "bar_8_64_b" (RIP.Ptr Bar_8_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_8_64_b")

{-| __C declaration:__ @struct bar_16_16@

    __defined at:__ @types\/structs\/bitfields.h 68:32@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Bar_16_16 = Bar_16_16
  { bar_16_16_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 69:14@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , bar_16_16_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 70:14@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar_16_16 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar_16_16 where

  readRaw =
    \ptr0 ->
          pure Bar_16_16
      <*> HasCBitfield.peek (RIP.Proxy @"bar_16_16_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bar_16_16_b") ptr0

instance Marshal.WriteRaw Bar_16_16 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar_16_16 bar_16_16_a2 bar_16_16_b3 ->
               HasCBitfield.poke (RIP.Proxy @"bar_16_16_a") ptr0 bar_16_16_a2
            >> HasCBitfield.poke (RIP.Proxy @"bar_16_16_b") ptr0 bar_16_16_b3

deriving via Marshal.EquivStorable Bar_16_16 instance RIP.Storable Bar_16_16

instance HasCBitfield.HasCBitfield Bar_16_16 "bar_16_16_a" where

  type CBitfieldType Bar_16_16 "bar_16_16_a" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 14

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "bar_16_16_a" (RIP.Ptr Bar_16_16) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_16_16_a")

instance HasCBitfield.HasCBitfield Bar_16_16 "bar_16_16_b" where

  type CBitfieldType Bar_16_16 "bar_16_16_b" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 14

  bitfieldWidth# = \_ -> \_ -> 14

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "bar_16_16_b" (RIP.Ptr Bar_16_16) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_16_16_b")

{-| __C declaration:__ @struct bar_16_32@

    __defined at:__ @types\/structs\/bitfields.h 74:32@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Bar_16_32 = Bar_16_32
  { bar_16_32_a :: RIP.CLong
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 75:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , bar_16_32_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 76:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar_16_32 where

  staticSizeOf = \_ -> (5 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar_16_32 where

  readRaw =
    \ptr0 ->
          pure Bar_16_32
      <*> HasCBitfield.peek (RIP.Proxy @"bar_16_32_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bar_16_32_b") ptr0

instance Marshal.WriteRaw Bar_16_32 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar_16_32 bar_16_32_a2 bar_16_32_b3 ->
               HasCBitfield.poke (RIP.Proxy @"bar_16_32_a") ptr0 bar_16_32_a2
            >> HasCBitfield.poke (RIP.Proxy @"bar_16_32_b") ptr0 bar_16_32_b3

deriving via Marshal.EquivStorable Bar_16_32 instance RIP.Storable Bar_16_32

instance HasCBitfield.HasCBitfield Bar_16_32 "bar_16_32_a" where

  type CBitfieldType Bar_16_32 "bar_16_32_a" =
    RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 24

instance ( ty ~ RIP.CLong
         ) => RIP.HasField "bar_16_32_a" (RIP.Ptr Bar_16_32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_16_32_a")

instance HasCBitfield.HasCBitfield Bar_16_32 "bar_16_32_b" where

  type CBitfieldType Bar_16_32 "bar_16_32_b" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 24

  bitfieldWidth# = \_ -> \_ -> 14

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "bar_16_32_b" (RIP.Ptr Bar_16_32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_16_32_b")

{-| __C declaration:__ @struct bar_16_64@

    __defined at:__ @types\/structs\/bitfields.h 80:32@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Bar_16_64 = Bar_16_64
  { bar_16_64_a :: RIP.CLLong
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 81:20@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , bar_16_64_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 82:20@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar_16_64 where

  staticSizeOf = \_ -> (9 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar_16_64 where

  readRaw =
    \ptr0 ->
          pure Bar_16_64
      <*> HasCBitfield.peek (RIP.Proxy @"bar_16_64_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bar_16_64_b") ptr0

instance Marshal.WriteRaw Bar_16_64 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar_16_64 bar_16_64_a2 bar_16_64_b3 ->
               HasCBitfield.poke (RIP.Proxy @"bar_16_64_a") ptr0 bar_16_64_a2
            >> HasCBitfield.poke (RIP.Proxy @"bar_16_64_b") ptr0 bar_16_64_b3

deriving via Marshal.EquivStorable Bar_16_64 instance RIP.Storable Bar_16_64

instance HasCBitfield.HasCBitfield Bar_16_64 "bar_16_64_a" where

  type CBitfieldType Bar_16_64 "bar_16_64_a" =
    RIP.CLLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 56

instance ( ty ~ RIP.CLLong
         ) => RIP.HasField "bar_16_64_a" (RIP.Ptr Bar_16_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_16_64_a")

instance HasCBitfield.HasCBitfield Bar_16_64 "bar_16_64_b" where

  type CBitfieldType Bar_16_64 "bar_16_64_b" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 56

  bitfieldWidth# = \_ -> \_ -> 14

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "bar_16_64_b" (RIP.Ptr Bar_16_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_16_64_b")

{-| __C declaration:__ @struct bar_32_32@

    __defined at:__ @types\/structs\/bitfields.h 86:32@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Bar_32_32 = Bar_32_32
  { bar_32_32_a :: RIP.CLong
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 87:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , bar_32_32_b :: RIP.CLong
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 88:15@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar_32_32 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar_32_32 where

  readRaw =
    \ptr0 ->
          pure Bar_32_32
      <*> HasCBitfield.peek (RIP.Proxy @"bar_32_32_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bar_32_32_b") ptr0

instance Marshal.WriteRaw Bar_32_32 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar_32_32 bar_32_32_a2 bar_32_32_b3 ->
               HasCBitfield.poke (RIP.Proxy @"bar_32_32_a") ptr0 bar_32_32_a2
            >> HasCBitfield.poke (RIP.Proxy @"bar_32_32_b") ptr0 bar_32_32_b3

deriving via Marshal.EquivStorable Bar_32_32 instance RIP.Storable Bar_32_32

instance HasCBitfield.HasCBitfield Bar_32_32 "bar_32_32_a" where

  type CBitfieldType Bar_32_32 "bar_32_32_a" =
    RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 30

instance ( ty ~ RIP.CLong
         ) => RIP.HasField "bar_32_32_a" (RIP.Ptr Bar_32_32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_32_32_a")

instance HasCBitfield.HasCBitfield Bar_32_32 "bar_32_32_b" where

  type CBitfieldType Bar_32_32 "bar_32_32_b" =
    RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 30

  bitfieldWidth# = \_ -> \_ -> 30

instance ( ty ~ RIP.CLong
         ) => RIP.HasField "bar_32_32_b" (RIP.Ptr Bar_32_32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_32_32_b")

{-| __C declaration:__ @struct bar_32_64@

    __defined at:__ @types\/structs\/bitfields.h 92:32@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Bar_32_64 = Bar_32_64
  { bar_32_64_a :: RIP.CLLong
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 93:20@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , bar_32_64_b :: RIP.CLong
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 94:20@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar_32_64 where

  staticSizeOf = \_ -> (11 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar_32_64 where

  readRaw =
    \ptr0 ->
          pure Bar_32_64
      <*> HasCBitfield.peek (RIP.Proxy @"bar_32_64_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bar_32_64_b") ptr0

instance Marshal.WriteRaw Bar_32_64 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar_32_64 bar_32_64_a2 bar_32_64_b3 ->
               HasCBitfield.poke (RIP.Proxy @"bar_32_64_a") ptr0 bar_32_64_a2
            >> HasCBitfield.poke (RIP.Proxy @"bar_32_64_b") ptr0 bar_32_64_b3

deriving via Marshal.EquivStorable Bar_32_64 instance RIP.Storable Bar_32_64

instance HasCBitfield.HasCBitfield Bar_32_64 "bar_32_64_a" where

  type CBitfieldType Bar_32_64 "bar_32_64_a" =
    RIP.CLLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 56

instance ( ty ~ RIP.CLLong
         ) => RIP.HasField "bar_32_64_a" (RIP.Ptr Bar_32_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_32_64_a")

instance HasCBitfield.HasCBitfield Bar_32_64 "bar_32_64_b" where

  type CBitfieldType Bar_32_64 "bar_32_64_b" =
    RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 56

  bitfieldWidth# = \_ -> \_ -> 30

instance ( ty ~ RIP.CLong
         ) => RIP.HasField "bar_32_64_b" (RIP.Ptr Bar_32_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_32_64_b")

{-| __C declaration:__ @struct bar_64_64@

    __defined at:__ @types\/structs\/bitfields.h 98:32@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Bar_64_64 = Bar_64_64
  { bar_64_64_a :: RIP.CLLong
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/bitfields.h 99:20@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , bar_64_64_b :: RIP.CLLong
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/bitfields.h 100:20@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar_64_64 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar_64_64 where

  readRaw =
    \ptr0 ->
          pure Bar_64_64
      <*> HasCBitfield.peek (RIP.Proxy @"bar_64_64_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bar_64_64_b") ptr0

instance Marshal.WriteRaw Bar_64_64 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar_64_64 bar_64_64_a2 bar_64_64_b3 ->
               HasCBitfield.poke (RIP.Proxy @"bar_64_64_a") ptr0 bar_64_64_a2
            >> HasCBitfield.poke (RIP.Proxy @"bar_64_64_b") ptr0 bar_64_64_b3

deriving via Marshal.EquivStorable Bar_64_64 instance RIP.Storable Bar_64_64

instance HasCBitfield.HasCBitfield Bar_64_64 "bar_64_64_a" where

  type CBitfieldType Bar_64_64 "bar_64_64_a" =
    RIP.CLLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 56

instance ( ty ~ RIP.CLLong
         ) => RIP.HasField "bar_64_64_a" (RIP.Ptr Bar_64_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_64_64_a")

instance HasCBitfield.HasCBitfield Bar_64_64 "bar_64_64_b" where

  type CBitfieldType Bar_64_64 "bar_64_64_b" =
    RIP.CLLong

  bitfieldOffset# = \_ -> \_ -> 56

  bitfieldWidth# = \_ -> \_ -> 40

instance ( ty ~ RIP.CLLong
         ) => RIP.HasField "bar_64_64_b" (RIP.Ptr Bar_64_64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bar_64_64_b")
