{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.BitfieldPtr
import qualified HsBindgen.Runtime.HasCBitfield
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct flags@

    __defined at:__ @types\/structs\/bitfields.h 1:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Flags = Flags
  { flags_fieldX :: FC.CChar
    {- ^ __C declaration:__ @fieldX@

         __defined at:__ @types\/structs\/bitfields.h 2:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , flags_flagA :: FC.CInt
    {- ^ __C declaration:__ @flagA@

         __defined at:__ @types\/structs\/bitfields.h 3:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , flags_flagB :: FC.CInt
    {- ^ __C declaration:__ @flagB@

         __defined at:__ @types\/structs\/bitfields.h 4:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , flags_flagC :: FC.CInt
    {- ^ __C declaration:__ @flagC@

         __defined at:__ @types\/structs\/bitfields.h 5:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , flags_fieldY :: FC.CChar
    {- ^ __C declaration:__ @fieldY@

         __defined at:__ @types\/structs\/bitfields.h 6:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , flags_bits :: FC.CInt
    {- ^ __C declaration:__ @bits@

         __defined at:__ @types\/structs\/bitfields.h 7:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Flags where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Flags where

  readRaw =
    \ptr0 ->
          pure Flags
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"flags_fieldX") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"flags_flagA") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"flags_flagB") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"flags_flagC") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"flags_fieldY") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"flags_bits") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Flags where

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
                 HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"flags_fieldX") ptr0 flags_fieldX2
              >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"flags_flagA") ptr0 flags_flagA3
              >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"flags_flagB") ptr0 flags_flagB4
              >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"flags_flagC") ptr0 flags_flagC5
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"flags_fieldY") ptr0 flags_fieldY6
              >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"flags_bits") ptr0 flags_bits7

deriving via HsBindgen.Runtime.Marshal.EquivStorable Flags instance F.Storable Flags

instance HsBindgen.Runtime.HasCField.HasCField Flags "flags_fieldX" where

  type CFieldType Flags "flags_fieldX" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Flags) "flags_fieldX")
         ) => GHC.Records.HasField "flags_fieldX" (Ptr.Ptr Flags) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"flags_fieldX")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Flags "flags_flagA" where

  type CBitfieldType Flags "flags_flagA" = FC.CInt

  bitfieldOffset# = \_ -> \_ -> 8

  bitfieldWidth# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Flags) "flags_flagA")
         ) => GHC.Records.HasField "flags_flagA" (Ptr.Ptr Flags) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"flags_flagA")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Flags "flags_flagB" where

  type CBitfieldType Flags "flags_flagB" = FC.CInt

  bitfieldOffset# = \_ -> \_ -> 9

  bitfieldWidth# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Flags) "flags_flagB")
         ) => GHC.Records.HasField "flags_flagB" (Ptr.Ptr Flags) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"flags_flagB")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Flags "flags_flagC" where

  type CBitfieldType Flags "flags_flagC" = FC.CInt

  bitfieldOffset# = \_ -> \_ -> 10

  bitfieldWidth# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Flags) "flags_flagC")
         ) => GHC.Records.HasField "flags_flagC" (Ptr.Ptr Flags) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"flags_flagC")

instance HsBindgen.Runtime.HasCField.HasCField Flags "flags_fieldY" where

  type CFieldType Flags "flags_fieldY" = FC.CChar

  offset# = \_ -> \_ -> 2

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Flags) "flags_fieldY")
         ) => GHC.Records.HasField "flags_fieldY" (Ptr.Ptr Flags) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"flags_fieldY")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Flags "flags_bits" where

  type CBitfieldType Flags "flags_bits" = FC.CInt

  bitfieldOffset# = \_ -> \_ -> 24

  bitfieldWidth# = \_ -> \_ -> 2

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Flags) "flags_bits")
         ) => GHC.Records.HasField "flags_bits" (Ptr.Ptr Flags) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"flags_bits")

{-| __C declaration:__ @struct overflow32@

    __defined at:__ @types\/structs\/bitfields.h 12:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Overflow32 = Overflow32
  { overflow32_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/bitfields.h 13:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow32_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/bitfields.h 14:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow32_z :: FC.CInt
    {- ^ __C declaration:__ @z@

         __defined at:__ @types\/structs\/bitfields.h 15:9@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Overflow32 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Overflow32 where

  readRaw =
    \ptr0 ->
          pure Overflow32
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"overflow32_x") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"overflow32_y") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"overflow32_z") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Overflow32 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32 overflow32_x2 overflow32_y3 overflow32_z4 ->
               HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"overflow32_x") ptr0 overflow32_x2
            >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"overflow32_y") ptr0 overflow32_y3
            >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"overflow32_z") ptr0 overflow32_z4

deriving via HsBindgen.Runtime.Marshal.EquivStorable Overflow32 instance F.Storable Overflow32

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Overflow32 "overflow32_x" where

  type CBitfieldType Overflow32 "overflow32_x" =
    FC.CInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 17

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Overflow32) "overflow32_x")
         ) => GHC.Records.HasField "overflow32_x" (Ptr.Ptr Overflow32) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"overflow32_x")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Overflow32 "overflow32_y" where

  type CBitfieldType Overflow32 "overflow32_y" =
    FC.CInt

  bitfieldOffset# = \_ -> \_ -> 32

  bitfieldWidth# = \_ -> \_ -> 17

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Overflow32) "overflow32_y")
         ) => GHC.Records.HasField "overflow32_y" (Ptr.Ptr Overflow32) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"overflow32_y")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Overflow32 "overflow32_z" where

  type CBitfieldType Overflow32 "overflow32_z" =
    FC.CInt

  bitfieldOffset# = \_ -> \_ -> 64

  bitfieldWidth# = \_ -> \_ -> 17

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Overflow32) "overflow32_z")
         ) => GHC.Records.HasField "overflow32_z" (Ptr.Ptr Overflow32) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"overflow32_z")

{-| __C declaration:__ @struct overflow32b@

    __defined at:__ @types\/structs\/bitfields.h 18:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Overflow32b = Overflow32b
  { overflow32b_x :: FC.CLong
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/bitfields.h 19:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow32b_y :: FC.CLong
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/bitfields.h 20:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow32b_z :: FC.CLong
    {- ^ __C declaration:__ @z@

         __defined at:__ @types\/structs\/bitfields.h 21:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Overflow32b where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Overflow32b where

  readRaw =
    \ptr0 ->
          pure Overflow32b
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"overflow32b_x") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"overflow32b_y") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"overflow32b_z") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Overflow32b where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32b overflow32b_x2 overflow32b_y3 overflow32b_z4 ->
               HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"overflow32b_x") ptr0 overflow32b_x2
            >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"overflow32b_y") ptr0 overflow32b_y3
            >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"overflow32b_z") ptr0 overflow32b_z4

deriving via HsBindgen.Runtime.Marshal.EquivStorable Overflow32b instance F.Storable Overflow32b

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Overflow32b "overflow32b_x" where

  type CBitfieldType Overflow32b "overflow32b_x" =
    FC.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 17

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Overflow32b) "overflow32b_x")
         ) => GHC.Records.HasField "overflow32b_x" (Ptr.Ptr Overflow32b) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"overflow32b_x")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Overflow32b "overflow32b_y" where

  type CBitfieldType Overflow32b "overflow32b_y" =
    FC.CLong

  bitfieldOffset# = \_ -> \_ -> 17

  bitfieldWidth# = \_ -> \_ -> 17

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Overflow32b) "overflow32b_y")
         ) => GHC.Records.HasField "overflow32b_y" (Ptr.Ptr Overflow32b) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"overflow32b_y")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Overflow32b "overflow32b_z" where

  type CBitfieldType Overflow32b "overflow32b_z" =
    FC.CLong

  bitfieldOffset# = \_ -> \_ -> 34

  bitfieldWidth# = \_ -> \_ -> 17

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Overflow32b) "overflow32b_z")
         ) => GHC.Records.HasField "overflow32b_z" (Ptr.Ptr Overflow32b) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"overflow32b_z")

{-| __C declaration:__ @struct overflow32c@

    __defined at:__ @types\/structs\/bitfields.h 24:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Overflow32c = Overflow32c
  { overflow32c_x :: FC.CLong
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/bitfields.h 25:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow32c_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/bitfields.h 26:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow32c_z :: FC.CLong
    {- ^ __C declaration:__ @z@

         __defined at:__ @types\/structs\/bitfields.h 27:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Overflow32c where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Overflow32c where

  readRaw =
    \ptr0 ->
          pure Overflow32c
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"overflow32c_x") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"overflow32c_y") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"overflow32c_z") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Overflow32c where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32c overflow32c_x2 overflow32c_y3 overflow32c_z4 ->
               HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"overflow32c_x") ptr0 overflow32c_x2
            >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"overflow32c_y") ptr0 overflow32c_y3
            >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"overflow32c_z") ptr0 overflow32c_z4

deriving via HsBindgen.Runtime.Marshal.EquivStorable Overflow32c instance F.Storable Overflow32c

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Overflow32c "overflow32c_x" where

  type CBitfieldType Overflow32c "overflow32c_x" =
    FC.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 17

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Overflow32c) "overflow32c_x")
         ) => GHC.Records.HasField "overflow32c_x" (Ptr.Ptr Overflow32c) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"overflow32c_x")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Overflow32c "overflow32c_y" where

  type CBitfieldType Overflow32c "overflow32c_y" =
    FC.CInt

  bitfieldOffset# = \_ -> \_ -> 32

  bitfieldWidth# = \_ -> \_ -> 17

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Overflow32c) "overflow32c_y")
         ) => GHC.Records.HasField "overflow32c_y" (Ptr.Ptr Overflow32c) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"overflow32c_y")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Overflow32c "overflow32c_z" where

  type CBitfieldType Overflow32c "overflow32c_z" =
    FC.CLong

  bitfieldOffset# = \_ -> \_ -> 64

  bitfieldWidth# = \_ -> \_ -> 17

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Overflow32c) "overflow32c_z")
         ) => GHC.Records.HasField "overflow32c_z" (Ptr.Ptr Overflow32c) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"overflow32c_z")

{-| __C declaration:__ @struct overflow64@

    __defined at:__ @types\/structs\/bitfields.h 30:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data Overflow64 = Overflow64
  { overflow64_x :: FC.CLong
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/bitfields.h 31:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , overflow64_y :: FC.CLong
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/bitfields.h 32:10@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Overflow64 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Overflow64 where

  readRaw =
    \ptr0 ->
          pure Overflow64
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"overflow64_x") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"overflow64_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Overflow64 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow64 overflow64_x2 overflow64_y3 ->
               HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"overflow64_x") ptr0 overflow64_x2
            >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"overflow64_y") ptr0 overflow64_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Overflow64 instance F.Storable Overflow64

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Overflow64 "overflow64_x" where

  type CBitfieldType Overflow64 "overflow64_x" =
    FC.CLong

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 33

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Overflow64) "overflow64_x")
         ) => GHC.Records.HasField "overflow64_x" (Ptr.Ptr Overflow64) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"overflow64_x")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Overflow64 "overflow64_y" where

  type CBitfieldType Overflow64 "overflow64_y" =
    FC.CLong

  bitfieldOffset# = \_ -> \_ -> 64

  bitfieldWidth# = \_ -> \_ -> 33

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Overflow64) "overflow64_y")
         ) => GHC.Records.HasField "overflow64_y" (Ptr.Ptr Overflow64) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"overflow64_y")

{-| __C declaration:__ @struct alignA@

    __defined at:__ @types\/structs\/bitfields.h 36:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data AlignA = AlignA
  { alignA_x :: FC.CUChar
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/bitfields.h 37:16@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , alignA_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/bitfields.h 38:6@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize AlignA where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw AlignA where

  readRaw =
    \ptr0 ->
          pure AlignA
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"alignA_x") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"alignA_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw AlignA where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AlignA alignA_x2 alignA_y3 ->
               HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"alignA_x") ptr0 alignA_x2
            >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"alignA_y") ptr0 alignA_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable AlignA instance F.Storable AlignA

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield AlignA "alignA_x" where

  type CBitfieldType AlignA "alignA_x" = FC.CUChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType AlignA) "alignA_x")
         ) => GHC.Records.HasField "alignA_x" (Ptr.Ptr AlignA) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"alignA_x")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield AlignA "alignA_y" where

  type CBitfieldType AlignA "alignA_y" = FC.CInt

  bitfieldOffset# = \_ -> \_ -> 1

  bitfieldWidth# = \_ -> \_ -> 10

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType AlignA) "alignA_y")
         ) => GHC.Records.HasField "alignA_y" (Ptr.Ptr AlignA) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"alignA_y")

{-| __C declaration:__ @struct alignB@

    __defined at:__ @types\/structs\/bitfields.h 41:8@

    __exported by:__ @types\/structs\/bitfields.h@
-}
data AlignB = AlignB
  { alignB_x :: FC.CUChar
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/bitfields.h 42:16@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  , alignB_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/bitfields.h 43:6@

         __exported by:__ @types\/structs\/bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize AlignB where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw AlignB where

  readRaw =
    \ptr0 ->
          pure AlignB
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"alignB_x") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"alignB_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw AlignB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AlignB alignB_x2 alignB_y3 ->
               HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"alignB_x") ptr0 alignB_x2
            >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"alignB_y") ptr0 alignB_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable AlignB instance F.Storable AlignB

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield AlignB "alignB_x" where

  type CBitfieldType AlignB "alignB_x" = FC.CUChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 7

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType AlignB) "alignB_x")
         ) => GHC.Records.HasField "alignB_x" (Ptr.Ptr AlignB) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"alignB_x")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield AlignB "alignB_y" where

  type CBitfieldType AlignB "alignB_y" = FC.CInt

  bitfieldOffset# = \_ -> \_ -> 32

  bitfieldWidth# = \_ -> \_ -> 31

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType AlignB) "alignB_y")
         ) => GHC.Records.HasField "alignB_y" (Ptr.Ptr AlignB) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"alignB_y")
