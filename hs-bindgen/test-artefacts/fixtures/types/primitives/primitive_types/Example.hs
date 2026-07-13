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
    ( Example.Primitive(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct primitive@

    __defined at:__ @types\/primitives\/primitive_types.h 1:8@

    __exported by:__ @types\/primitives\/primitive_types.h@
-}
data Primitive = Primitive
  { primitive_c :: BG.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/primitives\/primitive_types.h 2:10@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_sc :: BG.CSChar
    {- ^ __C declaration:__ @sc@

         __defined at:__ @types\/primitives\/primitive_types.h 3:17@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_uc :: BG.CUChar
    {- ^ __C declaration:__ @uc@

         __defined at:__ @types\/primitives\/primitive_types.h 4:19@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_s :: BG.CShort
    {- ^ __C declaration:__ @s@

         __defined at:__ @types\/primitives\/primitive_types.h 6:11@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_si :: BG.CShort
    {- ^ __C declaration:__ @si@

         __defined at:__ @types\/primitives\/primitive_types.h 7:15@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ss :: BG.CShort
    {- ^ __C declaration:__ @ss@

         __defined at:__ @types\/primitives\/primitive_types.h 8:18@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ssi :: BG.CShort
    {- ^ __C declaration:__ @ssi@

         __defined at:__ @types\/primitives\/primitive_types.h 9:22@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_us :: BG.CUShort
    {- ^ __C declaration:__ @us@

         __defined at:__ @types\/primitives\/primitive_types.h 11:20@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_usi :: BG.CUShort
    {- ^ __C declaration:__ @usi@

         __defined at:__ @types\/primitives\/primitive_types.h 12:24@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_i :: BG.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @types\/primitives\/primitive_types.h 14:9@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_s2 :: BG.CInt
    {- ^ __C declaration:__ @s2@

         __defined at:__ @types\/primitives\/primitive_types.h 15:12@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_si2 :: BG.CInt
    {- ^ __C declaration:__ @si2@

         __defined at:__ @types\/primitives\/primitive_types.h 16:16@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_u :: BG.CUInt
    {- ^ __C declaration:__ @u@

         __defined at:__ @types\/primitives\/primitive_types.h 18:14@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ui :: BG.CUInt
    {- ^ __C declaration:__ @ui@

         __defined at:__ @types\/primitives\/primitive_types.h 19:18@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_l :: BG.CLong
    {- ^ __C declaration:__ @l@

         __defined at:__ @types\/primitives\/primitive_types.h 21:10@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_li :: BG.CLong
    {- ^ __C declaration:__ @li@

         __defined at:__ @types\/primitives\/primitive_types.h 22:14@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_sl :: BG.CLong
    {- ^ __C declaration:__ @sl@

         __defined at:__ @types\/primitives\/primitive_types.h 23:17@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_sli :: BG.CLong
    {- ^ __C declaration:__ @sli@

         __defined at:__ @types\/primitives\/primitive_types.h 24:21@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ul :: BG.CULong
    {- ^ __C declaration:__ @ul@

         __defined at:__ @types\/primitives\/primitive_types.h 26:19@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_uli :: BG.CULong
    {- ^ __C declaration:__ @uli@

         __defined at:__ @types\/primitives\/primitive_types.h 27:23@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ll :: BG.CLLong
    {- ^ __C declaration:__ @ll@

         __defined at:__ @types\/primitives\/primitive_types.h 29:15@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_lli :: BG.CLLong
    {- ^ __C declaration:__ @lli@

         __defined at:__ @types\/primitives\/primitive_types.h 30:19@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_sll :: BG.CLLong
    {- ^ __C declaration:__ @sll@

         __defined at:__ @types\/primitives\/primitive_types.h 31:22@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_slli :: BG.CLLong
    {- ^ __C declaration:__ @slli@

         __defined at:__ @types\/primitives\/primitive_types.h 32:26@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ull :: BG.CULLong
    {- ^ __C declaration:__ @ull@

         __defined at:__ @types\/primitives\/primitive_types.h 34:24@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ulli :: BG.CULLong
    {- ^ __C declaration:__ @ulli@

         __defined at:__ @types\/primitives\/primitive_types.h 35:28@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_f :: BG.CFloat
    {- ^ __C declaration:__ @f@

         __defined at:__ @types\/primitives\/primitive_types.h 37:11@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_d :: BG.CDouble
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/primitives\/primitive_types.h 38:12@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Primitive where

  staticSizeOf = \_ -> (152 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Primitive where

  readRaw =
    \ptr0 ->
          pure Primitive
      <*> HasCField.readRaw (BG.Proxy @"primitive_c") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_sc") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_uc") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_s") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_si") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_ss") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_ssi") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_us") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_usi") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_i") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_s2") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_si2") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_u") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_ui") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_l") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_li") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_sl") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_sli") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_ul") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_uli") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_ll") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_lli") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_sll") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_slli") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_ull") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_ulli") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_f") ptr0
      <*> HasCField.readRaw (BG.Proxy @"primitive_d") ptr0

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
                 HasCField.writeRaw (BG.Proxy @"primitive_c") ptr0 primitive_c2
              >> HasCField.writeRaw (BG.Proxy @"primitive_sc") ptr0 primitive_sc3
              >> HasCField.writeRaw (BG.Proxy @"primitive_uc") ptr0 primitive_uc4
              >> HasCField.writeRaw (BG.Proxy @"primitive_s") ptr0 primitive_s5
              >> HasCField.writeRaw (BG.Proxy @"primitive_si") ptr0 primitive_si6
              >> HasCField.writeRaw (BG.Proxy @"primitive_ss") ptr0 primitive_ss7
              >> HasCField.writeRaw (BG.Proxy @"primitive_ssi") ptr0 primitive_ssi8
              >> HasCField.writeRaw (BG.Proxy @"primitive_us") ptr0 primitive_us9
              >> HasCField.writeRaw (BG.Proxy @"primitive_usi") ptr0 primitive_usi10
              >> HasCField.writeRaw (BG.Proxy @"primitive_i") ptr0 primitive_i11
              >> HasCField.writeRaw (BG.Proxy @"primitive_s2") ptr0 primitive_s212
              >> HasCField.writeRaw (BG.Proxy @"primitive_si2") ptr0 primitive_si213
              >> HasCField.writeRaw (BG.Proxy @"primitive_u") ptr0 primitive_u14
              >> HasCField.writeRaw (BG.Proxy @"primitive_ui") ptr0 primitive_ui15
              >> HasCField.writeRaw (BG.Proxy @"primitive_l") ptr0 primitive_l16
              >> HasCField.writeRaw (BG.Proxy @"primitive_li") ptr0 primitive_li17
              >> HasCField.writeRaw (BG.Proxy @"primitive_sl") ptr0 primitive_sl18
              >> HasCField.writeRaw (BG.Proxy @"primitive_sli") ptr0 primitive_sli19
              >> HasCField.writeRaw (BG.Proxy @"primitive_ul") ptr0 primitive_ul20
              >> HasCField.writeRaw (BG.Proxy @"primitive_uli") ptr0 primitive_uli21
              >> HasCField.writeRaw (BG.Proxy @"primitive_ll") ptr0 primitive_ll22
              >> HasCField.writeRaw (BG.Proxy @"primitive_lli") ptr0 primitive_lli23
              >> HasCField.writeRaw (BG.Proxy @"primitive_sll") ptr0 primitive_sll24
              >> HasCField.writeRaw (BG.Proxy @"primitive_slli") ptr0 primitive_slli25
              >> HasCField.writeRaw (BG.Proxy @"primitive_ull") ptr0 primitive_ull26
              >> HasCField.writeRaw (BG.Proxy @"primitive_ulli") ptr0 primitive_ulli27
              >> HasCField.writeRaw (BG.Proxy @"primitive_f") ptr0 primitive_f28
              >> HasCField.writeRaw (BG.Proxy @"primitive_d") ptr0 primitive_d29

deriving via Marshal.EquivStorable Primitive instance BG.Storable Primitive

instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "primitive_c" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_c = y1
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_c" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "primitive_c" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_c")

instance HasCField.HasCField Primitive "primitive_c" where

  type CFieldType Primitive "primitive_c" = BG.CChar

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.CSChar
         ) => BG.CompatHasField.HasField "primitive_sc" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_sc = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_sc" x0
      )

instance ( ty ~ BG.CSChar
         ) => BG.HasField "primitive_sc" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_sc")

instance HasCField.HasCField Primitive "primitive_sc" where

  type CFieldType Primitive "primitive_sc" = BG.CSChar

  offset# = \_ -> \_ -> 1

instance ( ty ~ BG.CUChar
         ) => BG.CompatHasField.HasField "primitive_uc" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_uc = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_uc" x0
      )

instance ( ty ~ BG.CUChar
         ) => BG.HasField "primitive_uc" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_uc")

instance HasCField.HasCField Primitive "primitive_uc" where

  type CFieldType Primitive "primitive_uc" = BG.CUChar

  offset# = \_ -> \_ -> 2

instance ( ty ~ BG.CShort
         ) => BG.CompatHasField.HasField "primitive_s" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_s = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_s" x0
      )

instance ( ty ~ BG.CShort
         ) => BG.HasField "primitive_s" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_s")

instance HasCField.HasCField Primitive "primitive_s" where

  type CFieldType Primitive "primitive_s" = BG.CShort

  offset# = \_ -> \_ -> 4

instance ( ty ~ BG.CShort
         ) => BG.CompatHasField.HasField "primitive_si" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_si = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_si" x0
      )

instance ( ty ~ BG.CShort
         ) => BG.HasField "primitive_si" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_si")

instance HasCField.HasCField Primitive "primitive_si" where

  type CFieldType Primitive "primitive_si" = BG.CShort

  offset# = \_ -> \_ -> 6

instance ( ty ~ BG.CShort
         ) => BG.CompatHasField.HasField "primitive_ss" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_ss = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_ss" x0
      )

instance ( ty ~ BG.CShort
         ) => BG.HasField "primitive_ss" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_ss")

instance HasCField.HasCField Primitive "primitive_ss" where

  type CFieldType Primitive "primitive_ss" = BG.CShort

  offset# = \_ -> \_ -> 8

instance ( ty ~ BG.CShort
         ) => BG.CompatHasField.HasField "primitive_ssi" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_ssi = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_ssi" x0
      )

instance ( ty ~ BG.CShort
         ) => BG.HasField "primitive_ssi" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_ssi")

instance HasCField.HasCField Primitive "primitive_ssi" where

  type CFieldType Primitive "primitive_ssi" = BG.CShort

  offset# = \_ -> \_ -> 10

instance ( ty ~ BG.CUShort
         ) => BG.CompatHasField.HasField "primitive_us" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_us = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_us" x0
      )

instance ( ty ~ BG.CUShort
         ) => BG.HasField "primitive_us" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_us")

instance HasCField.HasCField Primitive "primitive_us" where

  type CFieldType Primitive "primitive_us" = BG.CUShort

  offset# = \_ -> \_ -> 12

instance ( ty ~ BG.CUShort
         ) => BG.CompatHasField.HasField "primitive_usi" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_usi = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_usi" x0
      )

instance ( ty ~ BG.CUShort
         ) => BG.HasField "primitive_usi" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_usi")

instance HasCField.HasCField Primitive "primitive_usi" where

  type CFieldType Primitive "primitive_usi" =
    BG.CUShort

  offset# = \_ -> \_ -> 14

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "primitive_i" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_i = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_i" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "primitive_i" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_i")

instance HasCField.HasCField Primitive "primitive_i" where

  type CFieldType Primitive "primitive_i" = BG.CInt

  offset# = \_ -> \_ -> 16

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "primitive_s2" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_s2 = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_s2" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "primitive_s2" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_s2")

instance HasCField.HasCField Primitive "primitive_s2" where

  type CFieldType Primitive "primitive_s2" = BG.CInt

  offset# = \_ -> \_ -> 20

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "primitive_si2" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_si2 = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_si2" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "primitive_si2" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_si2")

instance HasCField.HasCField Primitive "primitive_si2" where

  type CFieldType Primitive "primitive_si2" = BG.CInt

  offset# = \_ -> \_ -> 24

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "primitive_u" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_u = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_u" x0
      )

instance ( ty ~ BG.CUInt
         ) => BG.HasField "primitive_u" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_u")

instance HasCField.HasCField Primitive "primitive_u" where

  type CFieldType Primitive "primitive_u" = BG.CUInt

  offset# = \_ -> \_ -> 28

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "primitive_ui" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_ui = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_ui" x0
      )

instance ( ty ~ BG.CUInt
         ) => BG.HasField "primitive_ui" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_ui")

instance HasCField.HasCField Primitive "primitive_ui" where

  type CFieldType Primitive "primitive_ui" = BG.CUInt

  offset# = \_ -> \_ -> 32

instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "primitive_l" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_l = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_l" x0
      )

instance ( ty ~ BG.CLong
         ) => BG.HasField "primitive_l" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_l")

instance HasCField.HasCField Primitive "primitive_l" where

  type CFieldType Primitive "primitive_l" = BG.CLong

  offset# = \_ -> \_ -> 40

instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "primitive_li" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_li = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_li" x0
      )

instance ( ty ~ BG.CLong
         ) => BG.HasField "primitive_li" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_li")

instance HasCField.HasCField Primitive "primitive_li" where

  type CFieldType Primitive "primitive_li" = BG.CLong

  offset# = \_ -> \_ -> 48

instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "primitive_sl" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_sl = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_sl" x0
      )

instance ( ty ~ BG.CLong
         ) => BG.HasField "primitive_sl" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_sl")

instance HasCField.HasCField Primitive "primitive_sl" where

  type CFieldType Primitive "primitive_sl" = BG.CLong

  offset# = \_ -> \_ -> 56

instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "primitive_sli" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_sli = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_sli" x0
      )

instance ( ty ~ BG.CLong
         ) => BG.HasField "primitive_sli" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_sli")

instance HasCField.HasCField Primitive "primitive_sli" where

  type CFieldType Primitive "primitive_sli" = BG.CLong

  offset# = \_ -> \_ -> 64

instance ( ty ~ BG.CULong
         ) => BG.CompatHasField.HasField "primitive_ul" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_ul = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_ul" x0
      )

instance ( ty ~ BG.CULong
         ) => BG.HasField "primitive_ul" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_ul")

instance HasCField.HasCField Primitive "primitive_ul" where

  type CFieldType Primitive "primitive_ul" = BG.CULong

  offset# = \_ -> \_ -> 72

instance ( ty ~ BG.CULong
         ) => BG.CompatHasField.HasField "primitive_uli" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_uli = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_uli" x0
      )

instance ( ty ~ BG.CULong
         ) => BG.HasField "primitive_uli" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_uli")

instance HasCField.HasCField Primitive "primitive_uli" where

  type CFieldType Primitive "primitive_uli" = BG.CULong

  offset# = \_ -> \_ -> 80

instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "primitive_ll" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_ll = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_ll" x0
      )

instance ( ty ~ BG.CLLong
         ) => BG.HasField "primitive_ll" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_ll")

instance HasCField.HasCField Primitive "primitive_ll" where

  type CFieldType Primitive "primitive_ll" = BG.CLLong

  offset# = \_ -> \_ -> 88

instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "primitive_lli" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_lli = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_lli" x0
      )

instance ( ty ~ BG.CLLong
         ) => BG.HasField "primitive_lli" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_lli")

instance HasCField.HasCField Primitive "primitive_lli" where

  type CFieldType Primitive "primitive_lli" = BG.CLLong

  offset# = \_ -> \_ -> 96

instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "primitive_sll" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_sll = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_sll" x0
      )

instance ( ty ~ BG.CLLong
         ) => BG.HasField "primitive_sll" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_sll")

instance HasCField.HasCField Primitive "primitive_sll" where

  type CFieldType Primitive "primitive_sll" = BG.CLLong

  offset# = \_ -> \_ -> 104

instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "primitive_slli" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_slli = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_slli" x0
      )

instance ( ty ~ BG.CLLong
         ) => BG.HasField "primitive_slli" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_slli")

instance HasCField.HasCField Primitive "primitive_slli" where

  type CFieldType Primitive "primitive_slli" =
    BG.CLLong

  offset# = \_ -> \_ -> 112

instance ( ty ~ BG.CULLong
         ) => BG.CompatHasField.HasField "primitive_ull" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_ull = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_ull" x0
      )

instance ( ty ~ BG.CULLong
         ) => BG.HasField "primitive_ull" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_ull")

instance HasCField.HasCField Primitive "primitive_ull" where

  type CFieldType Primitive "primitive_ull" =
    BG.CULLong

  offset# = \_ -> \_ -> 120

instance ( ty ~ BG.CULLong
         ) => BG.CompatHasField.HasField "primitive_ulli" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_ulli = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_ulli" x0
      )

instance ( ty ~ BG.CULLong
         ) => BG.HasField "primitive_ulli" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_ulli")

instance HasCField.HasCField Primitive "primitive_ulli" where

  type CFieldType Primitive "primitive_ulli" =
    BG.CULLong

  offset# = \_ -> \_ -> 128

instance ( ty ~ BG.CFloat
         ) => BG.CompatHasField.HasField "primitive_f" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_f = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_d = BG.getField @"primitive_d" x0
                    }
      , BG.getField @"primitive_f" x0
      )

instance ( ty ~ BG.CFloat
         ) => BG.HasField "primitive_f" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_f")

instance HasCField.HasCField Primitive "primitive_f" where

  type CFieldType Primitive "primitive_f" = BG.CFloat

  offset# = \_ -> \_ -> 136

instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "primitive_d" Primitive ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Primitive { primitive_d = y1
                    , primitive_c = BG.getField @"primitive_c" x0
                    , primitive_sc = BG.getField @"primitive_sc" x0
                    , primitive_uc = BG.getField @"primitive_uc" x0
                    , primitive_s = BG.getField @"primitive_s" x0
                    , primitive_si = BG.getField @"primitive_si" x0
                    , primitive_ss = BG.getField @"primitive_ss" x0
                    , primitive_ssi = BG.getField @"primitive_ssi" x0
                    , primitive_us = BG.getField @"primitive_us" x0
                    , primitive_usi = BG.getField @"primitive_usi" x0
                    , primitive_i = BG.getField @"primitive_i" x0
                    , primitive_s2 = BG.getField @"primitive_s2" x0
                    , primitive_si2 = BG.getField @"primitive_si2" x0
                    , primitive_u = BG.getField @"primitive_u" x0
                    , primitive_ui = BG.getField @"primitive_ui" x0
                    , primitive_l = BG.getField @"primitive_l" x0
                    , primitive_li = BG.getField @"primitive_li" x0
                    , primitive_sl = BG.getField @"primitive_sl" x0
                    , primitive_sli = BG.getField @"primitive_sli" x0
                    , primitive_ul = BG.getField @"primitive_ul" x0
                    , primitive_uli = BG.getField @"primitive_uli" x0
                    , primitive_ll = BG.getField @"primitive_ll" x0
                    , primitive_lli = BG.getField @"primitive_lli" x0
                    , primitive_sll = BG.getField @"primitive_sll" x0
                    , primitive_slli = BG.getField @"primitive_slli" x0
                    , primitive_ull = BG.getField @"primitive_ull" x0
                    , primitive_ulli = BG.getField @"primitive_ulli" x0
                    , primitive_f = BG.getField @"primitive_f" x0
                    }
      , BG.getField @"primitive_d" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "primitive_d" (BG.Ptr Primitive) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"primitive_d")

instance HasCField.HasCField Primitive "primitive_d" where

  type CFieldType Primitive "primitive_d" = BG.CDouble

  offset# = \_ -> \_ -> 144
