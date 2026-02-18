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

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct primitive@

    __defined at:__ @types\/primitives\/primitive_types.h 1:8@

    __exported by:__ @types\/primitives\/primitive_types.h@
-}
data Primitive = Primitive
  { primitive_c :: RIP.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/primitives\/primitive_types.h 2:10@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_sc :: RIP.CSChar
    {- ^ __C declaration:__ @sc@

         __defined at:__ @types\/primitives\/primitive_types.h 3:17@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_uc :: RIP.CUChar
    {- ^ __C declaration:__ @uc@

         __defined at:__ @types\/primitives\/primitive_types.h 4:19@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_s :: RIP.CShort
    {- ^ __C declaration:__ @s@

         __defined at:__ @types\/primitives\/primitive_types.h 6:11@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_si :: RIP.CShort
    {- ^ __C declaration:__ @si@

         __defined at:__ @types\/primitives\/primitive_types.h 7:15@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ss :: RIP.CShort
    {- ^ __C declaration:__ @ss@

         __defined at:__ @types\/primitives\/primitive_types.h 8:18@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ssi :: RIP.CShort
    {- ^ __C declaration:__ @ssi@

         __defined at:__ @types\/primitives\/primitive_types.h 9:22@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_us :: RIP.CUShort
    {- ^ __C declaration:__ @us@

         __defined at:__ @types\/primitives\/primitive_types.h 11:20@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_usi :: RIP.CUShort
    {- ^ __C declaration:__ @usi@

         __defined at:__ @types\/primitives\/primitive_types.h 12:24@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_i :: RIP.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @types\/primitives\/primitive_types.h 14:9@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_s2 :: RIP.CInt
    {- ^ __C declaration:__ @s2@

         __defined at:__ @types\/primitives\/primitive_types.h 15:12@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_si2 :: RIP.CInt
    {- ^ __C declaration:__ @si2@

         __defined at:__ @types\/primitives\/primitive_types.h 16:16@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_u :: RIP.CUInt
    {- ^ __C declaration:__ @u@

         __defined at:__ @types\/primitives\/primitive_types.h 18:14@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ui :: RIP.CUInt
    {- ^ __C declaration:__ @ui@

         __defined at:__ @types\/primitives\/primitive_types.h 19:18@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_l :: RIP.CLong
    {- ^ __C declaration:__ @l@

         __defined at:__ @types\/primitives\/primitive_types.h 21:10@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_li :: RIP.CLong
    {- ^ __C declaration:__ @li@

         __defined at:__ @types\/primitives\/primitive_types.h 22:14@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_sl :: RIP.CLong
    {- ^ __C declaration:__ @sl@

         __defined at:__ @types\/primitives\/primitive_types.h 23:17@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_sli :: RIP.CLong
    {- ^ __C declaration:__ @sli@

         __defined at:__ @types\/primitives\/primitive_types.h 24:21@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ul :: RIP.CULong
    {- ^ __C declaration:__ @ul@

         __defined at:__ @types\/primitives\/primitive_types.h 26:19@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_uli :: RIP.CULong
    {- ^ __C declaration:__ @uli@

         __defined at:__ @types\/primitives\/primitive_types.h 27:23@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ll :: RIP.CLLong
    {- ^ __C declaration:__ @ll@

         __defined at:__ @types\/primitives\/primitive_types.h 29:15@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_lli :: RIP.CLLong
    {- ^ __C declaration:__ @lli@

         __defined at:__ @types\/primitives\/primitive_types.h 30:19@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_sll :: RIP.CLLong
    {- ^ __C declaration:__ @sll@

         __defined at:__ @types\/primitives\/primitive_types.h 31:22@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_slli :: RIP.CLLong
    {- ^ __C declaration:__ @slli@

         __defined at:__ @types\/primitives\/primitive_types.h 32:26@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ull :: RIP.CULLong
    {- ^ __C declaration:__ @ull@

         __defined at:__ @types\/primitives\/primitive_types.h 34:24@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ulli :: RIP.CULLong
    {- ^ __C declaration:__ @ulli@

         __defined at:__ @types\/primitives\/primitive_types.h 35:28@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_f :: RIP.CFloat
    {- ^ __C declaration:__ @f@

         __defined at:__ @types\/primitives\/primitive_types.h 37:11@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_d :: RIP.CDouble
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/primitives\/primitive_types.h 38:12@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Primitive where

  staticSizeOf = \_ -> (152 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Primitive where

  readRaw =
    \ptr0 ->
          pure Primitive
      <*> HasCField.readRaw (RIP.Proxy @"primitive_c") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_sc") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_uc") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_s") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_si") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_ss") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_ssi") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_us") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_usi") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_i") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_s2") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_si2") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_u") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_ui") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_l") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_li") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_sl") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_sli") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_ul") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_uli") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_ll") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_lli") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_sll") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_slli") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_ull") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_ulli") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_f") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"primitive_d") ptr0

instance Marshal.WriteRaw Primitive where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Primitive
            primitive_c2
            primitive_sc3
            primitive_uc4
            primitive_s5
            primitive_si6
            primitive_ss7
            primitive_ssi8
            primitive_us9
            primitive_usi10
            primitive_i11
            primitive_s212
            primitive_si213
            primitive_u14
            primitive_ui15
            primitive_l16
            primitive_li17
            primitive_sl18
            primitive_sli19
            primitive_ul20
            primitive_uli21
            primitive_ll22
            primitive_lli23
            primitive_sll24
            primitive_slli25
            primitive_ull26
            primitive_ulli27
            primitive_f28
            primitive_d29 ->
                 HasCField.writeRaw (RIP.Proxy @"primitive_c") ptr0 primitive_c2
              >> HasCField.writeRaw (RIP.Proxy @"primitive_sc") ptr0 primitive_sc3
              >> HasCField.writeRaw (RIP.Proxy @"primitive_uc") ptr0 primitive_uc4
              >> HasCField.writeRaw (RIP.Proxy @"primitive_s") ptr0 primitive_s5
              >> HasCField.writeRaw (RIP.Proxy @"primitive_si") ptr0 primitive_si6
              >> HasCField.writeRaw (RIP.Proxy @"primitive_ss") ptr0 primitive_ss7
              >> HasCField.writeRaw (RIP.Proxy @"primitive_ssi") ptr0 primitive_ssi8
              >> HasCField.writeRaw (RIP.Proxy @"primitive_us") ptr0 primitive_us9
              >> HasCField.writeRaw (RIP.Proxy @"primitive_usi") ptr0 primitive_usi10
              >> HasCField.writeRaw (RIP.Proxy @"primitive_i") ptr0 primitive_i11
              >> HasCField.writeRaw (RIP.Proxy @"primitive_s2") ptr0 primitive_s212
              >> HasCField.writeRaw (RIP.Proxy @"primitive_si2") ptr0 primitive_si213
              >> HasCField.writeRaw (RIP.Proxy @"primitive_u") ptr0 primitive_u14
              >> HasCField.writeRaw (RIP.Proxy @"primitive_ui") ptr0 primitive_ui15
              >> HasCField.writeRaw (RIP.Proxy @"primitive_l") ptr0 primitive_l16
              >> HasCField.writeRaw (RIP.Proxy @"primitive_li") ptr0 primitive_li17
              >> HasCField.writeRaw (RIP.Proxy @"primitive_sl") ptr0 primitive_sl18
              >> HasCField.writeRaw (RIP.Proxy @"primitive_sli") ptr0 primitive_sli19
              >> HasCField.writeRaw (RIP.Proxy @"primitive_ul") ptr0 primitive_ul20
              >> HasCField.writeRaw (RIP.Proxy @"primitive_uli") ptr0 primitive_uli21
              >> HasCField.writeRaw (RIP.Proxy @"primitive_ll") ptr0 primitive_ll22
              >> HasCField.writeRaw (RIP.Proxy @"primitive_lli") ptr0 primitive_lli23
              >> HasCField.writeRaw (RIP.Proxy @"primitive_sll") ptr0 primitive_sll24
              >> HasCField.writeRaw (RIP.Proxy @"primitive_slli") ptr0 primitive_slli25
              >> HasCField.writeRaw (RIP.Proxy @"primitive_ull") ptr0 primitive_ull26
              >> HasCField.writeRaw (RIP.Proxy @"primitive_ulli") ptr0 primitive_ulli27
              >> HasCField.writeRaw (RIP.Proxy @"primitive_f") ptr0 primitive_f28
              >> HasCField.writeRaw (RIP.Proxy @"primitive_d") ptr0 primitive_d29

deriving via Marshal.EquivStorable Primitive instance RIP.Storable Primitive

instance HasCField.HasCField Primitive "primitive_c" where

  type CFieldType Primitive "primitive_c" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "primitive_c" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_c")

instance HasCField.HasCField Primitive "primitive_sc" where

  type CFieldType Primitive "primitive_sc" = RIP.CSChar

  offset# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CSChar
         ) => RIP.HasField "primitive_sc" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_sc")

instance HasCField.HasCField Primitive "primitive_uc" where

  type CFieldType Primitive "primitive_uc" = RIP.CUChar

  offset# = \_ -> \_ -> 2

instance ( ((~) ty) RIP.CUChar
         ) => RIP.HasField "primitive_uc" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_uc")

instance HasCField.HasCField Primitive "primitive_s" where

  type CFieldType Primitive "primitive_s" = RIP.CShort

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CShort
         ) => RIP.HasField "primitive_s" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_s")

instance HasCField.HasCField Primitive "primitive_si" where

  type CFieldType Primitive "primitive_si" = RIP.CShort

  offset# = \_ -> \_ -> 6

instance ( ((~) ty) RIP.CShort
         ) => RIP.HasField "primitive_si" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_si")

instance HasCField.HasCField Primitive "primitive_ss" where

  type CFieldType Primitive "primitive_ss" = RIP.CShort

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CShort
         ) => RIP.HasField "primitive_ss" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_ss")

instance HasCField.HasCField Primitive "primitive_ssi" where

  type CFieldType Primitive "primitive_ssi" =
    RIP.CShort

  offset# = \_ -> \_ -> 10

instance ( ((~) ty) RIP.CShort
         ) => RIP.HasField "primitive_ssi" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_ssi")

instance HasCField.HasCField Primitive "primitive_us" where

  type CFieldType Primitive "primitive_us" =
    RIP.CUShort

  offset# = \_ -> \_ -> 12

instance ( ((~) ty) RIP.CUShort
         ) => RIP.HasField "primitive_us" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_us")

instance HasCField.HasCField Primitive "primitive_usi" where

  type CFieldType Primitive "primitive_usi" =
    RIP.CUShort

  offset# = \_ -> \_ -> 14

instance ( ((~) ty) RIP.CUShort
         ) => RIP.HasField "primitive_usi" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_usi")

instance HasCField.HasCField Primitive "primitive_i" where

  type CFieldType Primitive "primitive_i" = RIP.CInt

  offset# = \_ -> \_ -> 16

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "primitive_i" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_i")

instance HasCField.HasCField Primitive "primitive_s2" where

  type CFieldType Primitive "primitive_s2" = RIP.CInt

  offset# = \_ -> \_ -> 20

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "primitive_s2" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_s2")

instance HasCField.HasCField Primitive "primitive_si2" where

  type CFieldType Primitive "primitive_si2" = RIP.CInt

  offset# = \_ -> \_ -> 24

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "primitive_si2" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_si2")

instance HasCField.HasCField Primitive "primitive_u" where

  type CFieldType Primitive "primitive_u" = RIP.CUInt

  offset# = \_ -> \_ -> 28

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "primitive_u" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_u")

instance HasCField.HasCField Primitive "primitive_ui" where

  type CFieldType Primitive "primitive_ui" = RIP.CUInt

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "primitive_ui" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_ui")

instance HasCField.HasCField Primitive "primitive_l" where

  type CFieldType Primitive "primitive_l" = RIP.CLong

  offset# = \_ -> \_ -> 40

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "primitive_l" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_l")

instance HasCField.HasCField Primitive "primitive_li" where

  type CFieldType Primitive "primitive_li" = RIP.CLong

  offset# = \_ -> \_ -> 48

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "primitive_li" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_li")

instance HasCField.HasCField Primitive "primitive_sl" where

  type CFieldType Primitive "primitive_sl" = RIP.CLong

  offset# = \_ -> \_ -> 56

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "primitive_sl" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_sl")

instance HasCField.HasCField Primitive "primitive_sli" where

  type CFieldType Primitive "primitive_sli" = RIP.CLong

  offset# = \_ -> \_ -> 64

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "primitive_sli" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_sli")

instance HasCField.HasCField Primitive "primitive_ul" where

  type CFieldType Primitive "primitive_ul" = RIP.CULong

  offset# = \_ -> \_ -> 72

instance ( ((~) ty) RIP.CULong
         ) => RIP.HasField "primitive_ul" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_ul")

instance HasCField.HasCField Primitive "primitive_uli" where

  type CFieldType Primitive "primitive_uli" =
    RIP.CULong

  offset# = \_ -> \_ -> 80

instance ( ((~) ty) RIP.CULong
         ) => RIP.HasField "primitive_uli" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_uli")

instance HasCField.HasCField Primitive "primitive_ll" where

  type CFieldType Primitive "primitive_ll" = RIP.CLLong

  offset# = \_ -> \_ -> 88

instance ( ((~) ty) RIP.CLLong
         ) => RIP.HasField "primitive_ll" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_ll")

instance HasCField.HasCField Primitive "primitive_lli" where

  type CFieldType Primitive "primitive_lli" =
    RIP.CLLong

  offset# = \_ -> \_ -> 96

instance ( ((~) ty) RIP.CLLong
         ) => RIP.HasField "primitive_lli" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_lli")

instance HasCField.HasCField Primitive "primitive_sll" where

  type CFieldType Primitive "primitive_sll" =
    RIP.CLLong

  offset# = \_ -> \_ -> 104

instance ( ((~) ty) RIP.CLLong
         ) => RIP.HasField "primitive_sll" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_sll")

instance HasCField.HasCField Primitive "primitive_slli" where

  type CFieldType Primitive "primitive_slli" =
    RIP.CLLong

  offset# = \_ -> \_ -> 112

instance ( ((~) ty) RIP.CLLong
         ) => RIP.HasField "primitive_slli" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_slli")

instance HasCField.HasCField Primitive "primitive_ull" where

  type CFieldType Primitive "primitive_ull" =
    RIP.CULLong

  offset# = \_ -> \_ -> 120

instance ( ((~) ty) RIP.CULLong
         ) => RIP.HasField "primitive_ull" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_ull")

instance HasCField.HasCField Primitive "primitive_ulli" where

  type CFieldType Primitive "primitive_ulli" =
    RIP.CULLong

  offset# = \_ -> \_ -> 128

instance ( ((~) ty) RIP.CULLong
         ) => RIP.HasField "primitive_ulli" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_ulli")

instance HasCField.HasCField Primitive "primitive_f" where

  type CFieldType Primitive "primitive_f" = RIP.CFloat

  offset# = \_ -> \_ -> 136

instance ( ((~) ty) RIP.CFloat
         ) => RIP.HasField "primitive_f" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_f")

instance HasCField.HasCField Primitive "primitive_d" where

  type CFieldType Primitive "primitive_d" = RIP.CDouble

  offset# = \_ -> \_ -> 144

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "primitive_d" (RIP.Ptr Primitive) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"primitive_d")
