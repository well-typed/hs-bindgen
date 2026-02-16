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

module Example where

import qualified HsBindgen.Runtime.BitfieldPtr as BitfieldPtr
import qualified HsBindgen.Runtime.HasCBitfield as HasCBitfield
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct flags@

    __defined at:__ @types\/structs\/bitfields.h 1:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Flags = Flags
  { flags_fieldX :: RIP.CChar
    {- ^ __C declaration:__ @fieldX@

         __defined at:__ @types\/structs\/bitfields.h 2:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , flags_flagA :: RIP.CInt
    {- ^ __C declaration:__ @flagA@

         __defined at:__ @types\/structs\/bitfields.h 3:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , flags_flagB :: RIP.CInt
    {- ^ __C declaration:__ @flagB@

         __defined at:__ @types\/structs\/bitfields.h 4:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , flags_flagC :: RIP.CInt
    {- ^ __C declaration:__ @flagC@

         __defined at:__ @types\/structs\/bitfields.h 5:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , flags_fieldY :: RIP.CChar
    {- ^ __C declaration:__ @fieldY@

         __defined at:__ @types\/structs\/bitfields.h 6:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , flags_bits :: RIP.CInt
    {- ^ __C declaration:__ @bits@

         __defined at:__ @types\/structs\/bitfields.h 7:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Flags where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Flags where

  readRaw =
    \ptr0 ->
          pure Flags
      <*> HasCField.readRaw (RIP.Proxy @"flags_fieldX") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"flags_flagA") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"flags_flagB") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"flags_flagC") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"flags_fieldY") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"flags_bits") ptr0

instance Marshal.WriteRaw Flags where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Flags
            flags_fieldX2
            flags_flagA3
            flags_flagB4
            flags_flagC5
            flags_fieldY6
            flags_bits7 ->
                 HasCField.writeRaw (RIP.Proxy @"flags_fieldX") ptr0 flags_fieldX2
              >> HasCBitfield.poke (RIP.Proxy @"flags_flagA") ptr0 flags_flagA3
              >> HasCBitfield.poke (RIP.Proxy @"flags_flagB") ptr0 flags_flagB4
              >> HasCBitfield.poke (RIP.Proxy @"flags_flagC") ptr0 flags_flagC5
              >> HasCField.writeRaw (RIP.Proxy @"flags_fieldY") ptr0 flags_fieldY6
              >> HasCBitfield.poke (RIP.Proxy @"flags_bits") ptr0 flags_bits7

deriving via Marshal.EquivStorable Flags instance RIP.Storable Flags

instance HasCField.HasCField Flags "flags_fieldX" where

  type CFieldType Flags "flags_fieldX" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "flags_fieldX" (RIP.Ptr Flags) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"flags_fieldX")

instance HasCBitfield.HasCBitfield Flags "flags_flagA" where

  type CBitfieldType Flags "flags_flagA" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 8

  bitfieldWidth# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "flags_flagA" (RIP.Ptr Flags) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"flags_flagA")

instance HasCBitfield.HasCBitfield Flags "flags_flagB" where

  type CBitfieldType Flags "flags_flagB" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 9

  bitfieldWidth# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "flags_flagB" (RIP.Ptr Flags) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"flags_flagB")

instance HasCBitfield.HasCBitfield Flags "flags_flagC" where

  type CBitfieldType Flags "flags_flagC" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 10

  bitfieldWidth# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "flags_flagC" (RIP.Ptr Flags) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"flags_flagC")

instance HasCField.HasCField Flags "flags_fieldY" where

  type CFieldType Flags "flags_fieldY" = RIP.CChar

  offset# = \_ -> \_ -> 2

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "flags_fieldY" (RIP.Ptr Flags) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"flags_fieldY")

instance HasCBitfield.HasCBitfield Flags "flags_bits" where

  type CBitfieldType Flags "flags_bits" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 24

  bitfieldWidth# = \_ -> \_ -> 2

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "flags_bits" (RIP.Ptr Flags) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"flags_bits")

{-| __C declaration:__ @struct overflow32@

    __defined at:__ @types\/structs\/bitfields.h 12:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Overflow32 = Overflow32
  { overflow32_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/bitfields.h 13:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow32_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/bitfields.h 14:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow32_z :: RIP.CInt
    {- ^ __C declaration:__ @z@

         __defined at:__ @types\/structs\/bitfields.h 15:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Overflow32 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Overflow32 where

  readRaw =
    \ptr0 ->
          pure Overflow32
      <*> HasCBitfield.peek (RIP.Proxy @"overflow32_x") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"overflow32_y") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"overflow32_z") ptr0

instance Marshal.WriteRaw Overflow32 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32 overflow32_x2 overflow32_y3 overflow32_z4 ->
               HasCBitfield.poke (RIP.Proxy @"overflow32_x") ptr0 overflow32_x2
            >> HasCBitfield.poke (RIP.Proxy @"overflow32_y") ptr0 overflow32_y3
            >> HasCBitfield.poke (RIP.Proxy @"overflow32_z") ptr0 overflow32_z4

deriving via Marshal.EquivStorable Overflow32 instance RIP.Storable Overflow32

instance HasCBitfield.HasCBitfield Overflow32 "overflow32_x" where

  type CBitfieldType Overflow32 "overflow32_x" =
    RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 17

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "overflow32_x" (RIP.Ptr Overflow32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"overflow32_x")

instance HasCBitfield.HasCBitfield Overflow32 "overflow32_y" where

  type CBitfieldType Overflow32 "overflow32_y" =
    RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 32

  bitfieldWidth# = \_ -> \_ -> 17

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "overflow32_y" (RIP.Ptr Overflow32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"overflow32_y")

instance HasCBitfield.HasCBitfield Overflow32 "overflow32_z" where

  type CBitfieldType Overflow32 "overflow32_z" =
    RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 64

  bitfieldWidth# = \_ -> \_ -> 17

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "overflow32_z" (RIP.Ptr Overflow32) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"overflow32_z")

{-| __C declaration:__ @struct overflow32b@

    __defined at:__ @types\/structs\/bitfields.h 18:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Overflow32b = Overflow32b
  { overflow32b_x :: RIP.CLong
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/bitfields.h 19:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow32b_y :: RIP.CLong
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/bitfields.h 20:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow32b_z :: RIP.CLong
    {- ^ __C declaration:__ @z@

         __defined at:__ @types\/structs\/bitfields.h 21:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Overflow32b where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Overflow32b where

  readRaw =
    \ptr0 ->
          pure Overflow32b
      <*> HasCBitfield.peek (RIP.Proxy @"overflow32b_x") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"overflow32b_y") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"overflow32b_z") ptr0

instance Marshal.WriteRaw Overflow32b where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32b overflow32b_x2 overflow32b_y3 overflow32b_z4 ->
               HasCBitfield.poke (RIP.Proxy @"overflow32b_x") ptr0 overflow32b_x2
            >> HasCBitfield.poke (RIP.Proxy @"overflow32b_y") ptr0 overflow32b_y3
            >> HasCBitfield.poke (RIP.Proxy @"overflow32b_z") ptr0 overflow32b_z4

deriving via Marshal.EquivStorable Overflow32b instance RIP.Storable Overflow32b

instance HasCBitfield.HasCBitfield Overflow32b "overflow32b_x" where

  type CBitfieldType Overflow32b "overflow32b_x" =
    RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 17

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "overflow32b_x" (RIP.Ptr Overflow32b) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"overflow32b_x")

instance HasCBitfield.HasCBitfield Overflow32b "overflow32b_y" where

  type CBitfieldType Overflow32b "overflow32b_y" =
    RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 17

  bitfieldWidth# = \_ -> \_ -> 17

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "overflow32b_y" (RIP.Ptr Overflow32b) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"overflow32b_y")

instance HasCBitfield.HasCBitfield Overflow32b "overflow32b_z" where

  type CBitfieldType Overflow32b "overflow32b_z" =
    RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 34

  bitfieldWidth# = \_ -> \_ -> 17

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "overflow32b_z" (RIP.Ptr Overflow32b) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"overflow32b_z")

{-| __C declaration:__ @struct overflow32c@

    __defined at:__ @types\/structs\/bitfields.h 24:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Overflow32c = Overflow32c
  { overflow32c_x :: RIP.CLong
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/bitfields.h 25:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow32c_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/bitfields.h 26:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow32c_z :: RIP.CLong
    {- ^ __C declaration:__ @z@

         __defined at:__ @types\/structs\/bitfields.h 27:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Overflow32c where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Overflow32c where

  readRaw =
    \ptr0 ->
          pure Overflow32c
      <*> HasCBitfield.peek (RIP.Proxy @"overflow32c_x") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"overflow32c_y") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"overflow32c_z") ptr0

instance Marshal.WriteRaw Overflow32c where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32c overflow32c_x2 overflow32c_y3 overflow32c_z4 ->
               HasCBitfield.poke (RIP.Proxy @"overflow32c_x") ptr0 overflow32c_x2
            >> HasCBitfield.poke (RIP.Proxy @"overflow32c_y") ptr0 overflow32c_y3
            >> HasCBitfield.poke (RIP.Proxy @"overflow32c_z") ptr0 overflow32c_z4

deriving via Marshal.EquivStorable Overflow32c instance RIP.Storable Overflow32c

instance HasCBitfield.HasCBitfield Overflow32c "overflow32c_x" where

  type CBitfieldType Overflow32c "overflow32c_x" =
    RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 17

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "overflow32c_x" (RIP.Ptr Overflow32c) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"overflow32c_x")

instance HasCBitfield.HasCBitfield Overflow32c "overflow32c_y" where

  type CBitfieldType Overflow32c "overflow32c_y" =
    RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 32

  bitfieldWidth# = \_ -> \_ -> 17

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "overflow32c_y" (RIP.Ptr Overflow32c) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"overflow32c_y")

instance HasCBitfield.HasCBitfield Overflow32c "overflow32c_z" where

  type CBitfieldType Overflow32c "overflow32c_z" =
    RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 64

  bitfieldWidth# = \_ -> \_ -> 17

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "overflow32c_z" (RIP.Ptr Overflow32c) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"overflow32c_z")

{-| __C declaration:__ @struct overflow64@

    __defined at:__ @types\/structs\/bitfields.h 30:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Overflow64 = Overflow64
  { overflow64_x :: RIP.CLong
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/bitfields.h 31:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow64_y :: RIP.CLong
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/bitfields.h 32:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Overflow64 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Overflow64 where

  readRaw =
    \ptr0 ->
          pure Overflow64
      <*> HasCBitfield.peek (RIP.Proxy @"overflow64_x") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"overflow64_y") ptr0

instance Marshal.WriteRaw Overflow64 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow64 overflow64_x2 overflow64_y3 ->
               HasCBitfield.poke (RIP.Proxy @"overflow64_x") ptr0 overflow64_x2
            >> HasCBitfield.poke (RIP.Proxy @"overflow64_y") ptr0 overflow64_y3

deriving via Marshal.EquivStorable Overflow64 instance RIP.Storable Overflow64

instance HasCBitfield.HasCBitfield Overflow64 "overflow64_x" where

  type CBitfieldType Overflow64 "overflow64_x" =
    RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 33

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "overflow64_x" (RIP.Ptr Overflow64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"overflow64_x")

instance HasCBitfield.HasCBitfield Overflow64 "overflow64_y" where

  type CBitfieldType Overflow64 "overflow64_y" =
    RIP.CLong

  bitfieldOffset# = \_ -> \_ -> 64

  bitfieldWidth# = \_ -> \_ -> 33

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "overflow64_y" (RIP.Ptr Overflow64) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"overflow64_y")

{-| __C declaration:__ @struct alignA@

    __defined at:__ @types\/structs\/bitfields.h 36:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data AlignA = AlignA
  { alignA_x :: RIP.CUChar
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/bitfields.h 37:16@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , alignA_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/bitfields.h 38:6@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize AlignA where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw AlignA where

  readRaw =
    \ptr0 ->
          pure AlignA
      <*> HasCBitfield.peek (RIP.Proxy @"alignA_x") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"alignA_y") ptr0

instance Marshal.WriteRaw AlignA where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AlignA alignA_x2 alignA_y3 ->
               HasCBitfield.poke (RIP.Proxy @"alignA_x") ptr0 alignA_x2
            >> HasCBitfield.poke (RIP.Proxy @"alignA_y") ptr0 alignA_y3

deriving via Marshal.EquivStorable AlignA instance RIP.Storable AlignA

instance HasCBitfield.HasCBitfield AlignA "alignA_x" where

  type CBitfieldType AlignA "alignA_x" = RIP.CUChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CUChar
         ) => RIP.HasField "alignA_x" (RIP.Ptr AlignA) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"alignA_x")

instance HasCBitfield.HasCBitfield AlignA "alignA_y" where

  type CBitfieldType AlignA "alignA_y" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 1

  bitfieldWidth# = \_ -> \_ -> 10

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "alignA_y" (RIP.Ptr AlignA) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"alignA_y")

{-| __C declaration:__ @struct alignB@

    __defined at:__ @types\/structs\/bitfields.h 41:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data AlignB = AlignB
  { alignB_x :: RIP.CUChar
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/bitfields.h 42:16@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , alignB_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/bitfields.h 43:6@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize AlignB where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw AlignB where

  readRaw =
    \ptr0 ->
          pure AlignB
      <*> HasCBitfield.peek (RIP.Proxy @"alignB_x") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"alignB_y") ptr0

instance Marshal.WriteRaw AlignB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AlignB alignB_x2 alignB_y3 ->
               HasCBitfield.poke (RIP.Proxy @"alignB_x") ptr0 alignB_x2
            >> HasCBitfield.poke (RIP.Proxy @"alignB_y") ptr0 alignB_y3

deriving via Marshal.EquivStorable AlignB instance RIP.Storable AlignB

instance HasCBitfield.HasCBitfield AlignB "alignB_x" where

  type CBitfieldType AlignB "alignB_x" = RIP.CUChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 7

instance ( ((~) ty) RIP.CUChar
         ) => RIP.HasField "alignB_x" (RIP.Ptr AlignB) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"alignB_x")

instance HasCBitfield.HasCBitfield AlignB "alignB_y" where

  type CBitfieldType AlignB "alignB_y" = RIP.CInt

  bitfieldOffset# = \_ -> \_ -> 32

  bitfieldWidth# = \_ -> \_ -> 31

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "alignB_y" (RIP.Ptr AlignB) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"alignB_y")
