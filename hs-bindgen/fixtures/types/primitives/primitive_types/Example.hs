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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import GHC.Exts ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct primitive@

    __defined at:__ @types\/primitives\/primitive_types.h 1:8@

    __exported by:__ @types\/primitives\/primitive_types.h@
-}
data Primitive = Primitive
  { primitive_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/primitives\/primitive_types.h 2:10@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_sc :: FC.CSChar
    {- ^ __C declaration:__ @sc@

         __defined at:__ @types\/primitives\/primitive_types.h 3:17@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_uc :: FC.CUChar
    {- ^ __C declaration:__ @uc@

         __defined at:__ @types\/primitives\/primitive_types.h 4:19@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_s :: FC.CShort
    {- ^ __C declaration:__ @s@

         __defined at:__ @types\/primitives\/primitive_types.h 6:11@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_si :: FC.CShort
    {- ^ __C declaration:__ @si@

         __defined at:__ @types\/primitives\/primitive_types.h 7:15@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ss :: FC.CShort
    {- ^ __C declaration:__ @ss@

         __defined at:__ @types\/primitives\/primitive_types.h 8:18@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ssi :: FC.CShort
    {- ^ __C declaration:__ @ssi@

         __defined at:__ @types\/primitives\/primitive_types.h 9:22@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_us :: FC.CUShort
    {- ^ __C declaration:__ @us@

         __defined at:__ @types\/primitives\/primitive_types.h 11:20@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_usi :: FC.CUShort
    {- ^ __C declaration:__ @usi@

         __defined at:__ @types\/primitives\/primitive_types.h 12:24@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @types\/primitives\/primitive_types.h 14:9@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_s2 :: FC.CInt
    {- ^ __C declaration:__ @s2@

         __defined at:__ @types\/primitives\/primitive_types.h 15:12@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_si2 :: FC.CInt
    {- ^ __C declaration:__ @si2@

         __defined at:__ @types\/primitives\/primitive_types.h 16:16@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_u :: FC.CUInt
    {- ^ __C declaration:__ @u@

         __defined at:__ @types\/primitives\/primitive_types.h 18:14@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ui :: FC.CUInt
    {- ^ __C declaration:__ @ui@

         __defined at:__ @types\/primitives\/primitive_types.h 19:18@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_l :: FC.CLong
    {- ^ __C declaration:__ @l@

         __defined at:__ @types\/primitives\/primitive_types.h 21:10@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_li :: FC.CLong
    {- ^ __C declaration:__ @li@

         __defined at:__ @types\/primitives\/primitive_types.h 22:14@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_sl :: FC.CLong
    {- ^ __C declaration:__ @sl@

         __defined at:__ @types\/primitives\/primitive_types.h 23:17@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_sli :: FC.CLong
    {- ^ __C declaration:__ @sli@

         __defined at:__ @types\/primitives\/primitive_types.h 24:21@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ul :: FC.CULong
    {- ^ __C declaration:__ @ul@

         __defined at:__ @types\/primitives\/primitive_types.h 26:19@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_uli :: FC.CULong
    {- ^ __C declaration:__ @uli@

         __defined at:__ @types\/primitives\/primitive_types.h 27:23@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ll :: FC.CLLong
    {- ^ __C declaration:__ @ll@

         __defined at:__ @types\/primitives\/primitive_types.h 29:15@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_lli :: FC.CLLong
    {- ^ __C declaration:__ @lli@

         __defined at:__ @types\/primitives\/primitive_types.h 30:19@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_sll :: FC.CLLong
    {- ^ __C declaration:__ @sll@

         __defined at:__ @types\/primitives\/primitive_types.h 31:22@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_slli :: FC.CLLong
    {- ^ __C declaration:__ @slli@

         __defined at:__ @types\/primitives\/primitive_types.h 32:26@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ull :: FC.CULLong
    {- ^ __C declaration:__ @ull@

         __defined at:__ @types\/primitives\/primitive_types.h 34:24@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_ulli :: FC.CULLong
    {- ^ __C declaration:__ @ulli@

         __defined at:__ @types\/primitives\/primitive_types.h 35:28@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_f :: FC.CFloat
    {- ^ __C declaration:__ @f@

         __defined at:__ @types\/primitives\/primitive_types.h 37:11@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  , primitive_d :: FC.CDouble
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/primitives\/primitive_types.h 38:12@

         __exported by:__ @types\/primitives\/primitive_types.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Primitive where

  staticSizeOf = \_ -> (152 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Primitive where

  readRaw =
    \ptr0 ->
          pure Primitive
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_c") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_sc") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_uc") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_s") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_si") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_ss") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_ssi") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_us") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_usi") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_i") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_s2") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_si2") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_u") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_ui") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_l") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_li") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_sl") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_sli") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_ul") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_uli") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_ll") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_lli") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_sll") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_slli") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_ull") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_ulli") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_f") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"primitive_d") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Primitive where

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
                 HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_c") ptr0 primitive_c2
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_sc") ptr0 primitive_sc3
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_uc") ptr0 primitive_uc4
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_s") ptr0 primitive_s5
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_si") ptr0 primitive_si6
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_ss") ptr0 primitive_ss7
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_ssi") ptr0 primitive_ssi8
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_us") ptr0 primitive_us9
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_usi") ptr0 primitive_usi10
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_i") ptr0 primitive_i11
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_s2") ptr0 primitive_s212
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_si2") ptr0 primitive_si213
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_u") ptr0 primitive_u14
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_ui") ptr0 primitive_ui15
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_l") ptr0 primitive_l16
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_li") ptr0 primitive_li17
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_sl") ptr0 primitive_sl18
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_sli") ptr0 primitive_sli19
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_ul") ptr0 primitive_ul20
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_uli") ptr0 primitive_uli21
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_ll") ptr0 primitive_ll22
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_lli") ptr0 primitive_lli23
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_sll") ptr0 primitive_sll24
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_slli") ptr0 primitive_slli25
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_ull") ptr0 primitive_ull26
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_ulli") ptr0 primitive_ulli27
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_f") ptr0 primitive_f28
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"primitive_d") ptr0 primitive_d29

deriving via HsBindgen.Runtime.Marshal.EquivStorable Primitive instance F.Storable Primitive

instance Data.Primitive.Types.Prim Primitive where

  sizeOf# = \_ -> (152#)

  alignment# = \_ -> (8#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Primitive (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (1#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (2#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (3#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (4#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (5#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (6#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (7#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (8#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (9#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (10#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (11#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (12#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (13#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (14#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (15#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (16#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (17#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (18#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (19#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (20#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (21#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (22#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (23#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (24#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (25#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (26#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (28#) i1) (27#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (2#)) s5 of
                    (# s7, v8 #) ->
                      case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (3#)) s7 of
                        (# s9, v10 #) ->
                          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (4#)) s9 of
                            (# s11, v12 #) ->
                              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (5#)) s11 of
                                (# s13, v14 #) ->
                                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (6#)) s13 of
                                    (# s15, v16 #) ->
                                      case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (7#)) s15 of
                                        (# s17, v18 #) ->
                                          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (8#)) s17 of
                                            (# s19, v20 #) ->
                                              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (9#)) s19 of
                                                (# s21, v22 #) ->
                                                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (10#)) s21 of
                                                    (# s23, v24 #) ->
                                                      case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (11#)) s23 of
                                                        (# s25, v26 #) ->
                                                          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (12#)) s25 of
                                                            (# s27, v28 #) ->
                                                              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (13#)) s27 of
                                                                (# s29, v30 #) ->
                                                                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (14#)) s29 of
                                                                    (# s31, v32 #) ->
                                                                      case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (15#)) s31 of
                                                                        (# s33, v34 #) ->
                                                                          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (16#)) s33 of
                                                                            (# s35, v36 #) ->
                                                                              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (17#)) s35 of
                                                                                (# s37, v38 #) ->
                                                                                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (18#)) s37 of
                                                                                    (# s39, v40 #) ->
                                                                                      case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (19#)) s39 of
                                                                                        (# s41, v42 #) ->
                                                                                          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (20#)) s41 of
                                                                                            (# s43, v44 #) ->
                                                                                              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (21#)) s43 of
                                                                                                (# s45, v46 #) ->
                                                                                                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (22#)) s45 of
                                                                                                    (# s47, v48 #) ->
                                                                                                      case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (23#)) s47 of
                                                                                                        (# s49, v50 #) ->
                                                                                                          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (24#)) s49 of
                                                                                                            (# s51, v52 #) ->
                                                                                                              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (25#)) s51 of
                                                                                                                (# s53, v54 #) ->
                                                                                                                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (26#)) s53 of
                                                                                                                    (# s55, v56 #) ->
                                                                                                                      case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (28#) i1) (27#)) s55 of
                                                                                                                        (# s57, v58 #) ->
                                                                                                                          (#  s57
                                                                                                                          , Primitive v4 v6 v8 v10 v12 v14 v16 v18 v20 v22 v24 v26 v28 v30 v32 v34 v36 v38 v40 v42 v44 v46 v48 v50 v52 v54 v56 v58
                                                                                                                           #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Primitive
                primitive_c4
                primitive_sc5
                primitive_uc6
                primitive_s7
                primitive_si8
                primitive_ss9
                primitive_ssi10
                primitive_us11
                primitive_usi12
                primitive_i13
                primitive_s214
                primitive_si215
                primitive_u16
                primitive_ui17
                primitive_l18
                primitive_li19
                primitive_sl20
                primitive_sli21
                primitive_ul22
                primitive_uli23
                primitive_ll24
                primitive_lli25
                primitive_sll26
                primitive_slli27
                primitive_ull28
                primitive_ulli29
                primitive_f30
                primitive_d31 ->
                  case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (0#)) primitive_c4 s3 of
                    s32 ->
                      case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (1#)) primitive_sc5 s32 of
                        s33 ->
                          case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (2#)) primitive_uc6 s33 of
                            s34 ->
                              case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (3#)) primitive_s7 s34 of
                                s35 ->
                                  case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (4#)) primitive_si8 s35 of
                                    s36 ->
                                      case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (5#)) primitive_ss9 s36 of
                                        s37 ->
                                          case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (6#)) primitive_ssi10 s37 of
                                            s38 ->
                                              case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (7#)) primitive_us11 s38 of
                                                s39 ->
                                                  case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (8#)) primitive_usi12 s39 of
                                                    s40 ->
                                                      case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (9#)) primitive_i13 s40 of
                                                        s41 ->
                                                          case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (10#)) primitive_s214 s41 of
                                                            s42 ->
                                                              case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (11#)) primitive_si215 s42 of
                                                                s43 ->
                                                                  case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (12#)) primitive_u16 s43 of
                                                                    s44 ->
                                                                      case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (13#)) primitive_ui17 s44 of
                                                                        s45 ->
                                                                          case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (14#)) primitive_l18 s45 of
                                                                            s46 ->
                                                                              case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (15#)) primitive_li19 s46 of
                                                                                s47 ->
                                                                                  case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (16#)) primitive_sl20 s47 of
                                                                                    s48 ->
                                                                                      case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (17#)) primitive_sli21 s48 of
                                                                                        s49 ->
                                                                                          case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (18#)) primitive_ul22 s49 of
                                                                                            s50 ->
                                                                                              case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (19#)) primitive_uli23 s50 of
                                                                                                s51 ->
                                                                                                  case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (20#)) primitive_ll24 s51 of
                                                                                                    s52 ->
                                                                                                      case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (21#)) primitive_lli25 s52 of
                                                                                                        s53 ->
                                                                                                          case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (22#)) primitive_sll26 s53 of
                                                                                                            s54 ->
                                                                                                              case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (23#)) primitive_slli27 s54 of
                                                                                                                s55 ->
                                                                                                                  case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (24#)) primitive_ull28 s55 of
                                                                                                                    s56 ->
                                                                                                                      case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (25#)) primitive_ulli29 s56 of
                                                                                                                        s57 ->
                                                                                                                          case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (26#)) primitive_f30 s57 of
                                                                                                                            s58 ->
                                                                                                                              Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (28#) i1) (27#)) primitive_d31 s58

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Primitive (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (1#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (2#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (3#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (4#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (5#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (6#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (7#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (8#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (9#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (10#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (11#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (12#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (13#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (14#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (15#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (16#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (17#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (18#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (19#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (20#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (21#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (22#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (23#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (24#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (25#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (26#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (28#) i1) (27#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (2#)) s5 of
                    (# s7, v8 #) ->
                      case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (3#)) s7 of
                        (# s9, v10 #) ->
                          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (4#)) s9 of
                            (# s11, v12 #) ->
                              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (5#)) s11 of
                                (# s13, v14 #) ->
                                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (6#)) s13 of
                                    (# s15, v16 #) ->
                                      case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (7#)) s15 of
                                        (# s17, v18 #) ->
                                          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (8#)) s17 of
                                            (# s19, v20 #) ->
                                              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (9#)) s19 of
                                                (# s21, v22 #) ->
                                                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (10#)) s21 of
                                                    (# s23, v24 #) ->
                                                      case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (11#)) s23 of
                                                        (# s25, v26 #) ->
                                                          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (12#)) s25 of
                                                            (# s27, v28 #) ->
                                                              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (13#)) s27 of
                                                                (# s29, v30 #) ->
                                                                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (14#)) s29 of
                                                                    (# s31, v32 #) ->
                                                                      case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (15#)) s31 of
                                                                        (# s33, v34 #) ->
                                                                          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (16#)) s33 of
                                                                            (# s35, v36 #) ->
                                                                              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (17#)) s35 of
                                                                                (# s37, v38 #) ->
                                                                                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (18#)) s37 of
                                                                                    (# s39, v40 #) ->
                                                                                      case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (19#)) s39 of
                                                                                        (# s41, v42 #) ->
                                                                                          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (20#)) s41 of
                                                                                            (# s43, v44 #) ->
                                                                                              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (21#)) s43 of
                                                                                                (# s45, v46 #) ->
                                                                                                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (22#)) s45 of
                                                                                                    (# s47, v48 #) ->
                                                                                                      case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (23#)) s47 of
                                                                                                        (# s49, v50 #) ->
                                                                                                          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (24#)) s49 of
                                                                                                            (# s51, v52 #) ->
                                                                                                              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (25#)) s51 of
                                                                                                                (# s53, v54 #) ->
                                                                                                                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (26#)) s53 of
                                                                                                                    (# s55, v56 #) ->
                                                                                                                      case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (28#) i1) (27#)) s55 of
                                                                                                                        (# s57, v58 #) ->
                                                                                                                          (#  s57
                                                                                                                          , Primitive v4 v6 v8 v10 v12 v14 v16 v18 v20 v22 v24 v26 v28 v30 v32 v34 v36 v38 v40 v42 v44 v46 v48 v50 v52 v54 v56 v58
                                                                                                                           #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Primitive
                primitive_c4
                primitive_sc5
                primitive_uc6
                primitive_s7
                primitive_si8
                primitive_ss9
                primitive_ssi10
                primitive_us11
                primitive_usi12
                primitive_i13
                primitive_s214
                primitive_si215
                primitive_u16
                primitive_ui17
                primitive_l18
                primitive_li19
                primitive_sl20
                primitive_sli21
                primitive_ul22
                primitive_uli23
                primitive_ll24
                primitive_lli25
                primitive_sll26
                primitive_slli27
                primitive_ull28
                primitive_ulli29
                primitive_f30
                primitive_d31 ->
                  case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (0#)) primitive_c4 s3 of
                    s32 ->
                      case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (1#)) primitive_sc5 s32 of
                        s33 ->
                          case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (2#)) primitive_uc6 s33 of
                            s34 ->
                              case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (3#)) primitive_s7 s34 of
                                s35 ->
                                  case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (4#)) primitive_si8 s35 of
                                    s36 ->
                                      case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (5#)) primitive_ss9 s36 of
                                        s37 ->
                                          case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (6#)) primitive_ssi10 s37 of
                                            s38 ->
                                              case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (7#)) primitive_us11 s38 of
                                                s39 ->
                                                  case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (8#)) primitive_usi12 s39 of
                                                    s40 ->
                                                      case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (9#)) primitive_i13 s40 of
                                                        s41 ->
                                                          case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (10#)) primitive_s214 s41 of
                                                            s42 ->
                                                              case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (11#)) primitive_si215 s42 of
                                                                s43 ->
                                                                  case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (12#)) primitive_u16 s43 of
                                                                    s44 ->
                                                                      case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (13#)) primitive_ui17 s44 of
                                                                        s45 ->
                                                                          case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (14#)) primitive_l18 s45 of
                                                                            s46 ->
                                                                              case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (15#)) primitive_li19 s46 of
                                                                                s47 ->
                                                                                  case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (16#)) primitive_sl20 s47 of
                                                                                    s48 ->
                                                                                      case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (17#)) primitive_sli21 s48 of
                                                                                        s49 ->
                                                                                          case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (18#)) primitive_ul22 s49 of
                                                                                            s50 ->
                                                                                              case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (19#)) primitive_uli23 s50 of
                                                                                                s51 ->
                                                                                                  case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (20#)) primitive_ll24 s51 of
                                                                                                    s52 ->
                                                                                                      case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (21#)) primitive_lli25 s52 of
                                                                                                        s53 ->
                                                                                                          case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (22#)) primitive_sll26 s53 of
                                                                                                            s54 ->
                                                                                                              case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (23#)) primitive_slli27 s54 of
                                                                                                                s55 ->
                                                                                                                  case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (24#)) primitive_ull28 s55 of
                                                                                                                    s56 ->
                                                                                                                      case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (25#)) primitive_ulli29 s56 of
                                                                                                                        s57 ->
                                                                                                                          case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (26#)) primitive_f30 s57 of
                                                                                                                            s58 ->
                                                                                                                              Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (28#) i1) (27#)) primitive_d31 s58

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_c" where

  type CFieldType Primitive "primitive_c" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_c")
         ) => GHC.Records.HasField "primitive_c" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_c")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_sc" where

  type CFieldType Primitive "primitive_sc" = FC.CSChar

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_sc")
         ) => GHC.Records.HasField "primitive_sc" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_sc")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_uc" where

  type CFieldType Primitive "primitive_uc" = FC.CUChar

  offset# = \_ -> \_ -> 2

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_uc")
         ) => GHC.Records.HasField "primitive_uc" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_uc")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_s" where

  type CFieldType Primitive "primitive_s" = FC.CShort

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_s")
         ) => GHC.Records.HasField "primitive_s" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_s")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_si" where

  type CFieldType Primitive "primitive_si" = FC.CShort

  offset# = \_ -> \_ -> 6

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_si")
         ) => GHC.Records.HasField "primitive_si" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_si")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_ss" where

  type CFieldType Primitive "primitive_ss" = FC.CShort

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_ss")
         ) => GHC.Records.HasField "primitive_ss" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_ss")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_ssi" where

  type CFieldType Primitive "primitive_ssi" = FC.CShort

  offset# = \_ -> \_ -> 10

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_ssi")
         ) => GHC.Records.HasField "primitive_ssi" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_ssi")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_us" where

  type CFieldType Primitive "primitive_us" = FC.CUShort

  offset# = \_ -> \_ -> 12

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_us")
         ) => GHC.Records.HasField "primitive_us" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_us")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_usi" where

  type CFieldType Primitive "primitive_usi" =
    FC.CUShort

  offset# = \_ -> \_ -> 14

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_usi")
         ) => GHC.Records.HasField "primitive_usi" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_usi")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_i" where

  type CFieldType Primitive "primitive_i" = FC.CInt

  offset# = \_ -> \_ -> 16

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_i")
         ) => GHC.Records.HasField "primitive_i" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_i")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_s2" where

  type CFieldType Primitive "primitive_s2" = FC.CInt

  offset# = \_ -> \_ -> 20

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_s2")
         ) => GHC.Records.HasField "primitive_s2" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_s2")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_si2" where

  type CFieldType Primitive "primitive_si2" = FC.CInt

  offset# = \_ -> \_ -> 24

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_si2")
         ) => GHC.Records.HasField "primitive_si2" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_si2")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_u" where

  type CFieldType Primitive "primitive_u" = FC.CUInt

  offset# = \_ -> \_ -> 28

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_u")
         ) => GHC.Records.HasField "primitive_u" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_u")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_ui" where

  type CFieldType Primitive "primitive_ui" = FC.CUInt

  offset# = \_ -> \_ -> 32

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_ui")
         ) => GHC.Records.HasField "primitive_ui" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_ui")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_l" where

  type CFieldType Primitive "primitive_l" = FC.CLong

  offset# = \_ -> \_ -> 40

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_l")
         ) => GHC.Records.HasField "primitive_l" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_l")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_li" where

  type CFieldType Primitive "primitive_li" = FC.CLong

  offset# = \_ -> \_ -> 48

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_li")
         ) => GHC.Records.HasField "primitive_li" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_li")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_sl" where

  type CFieldType Primitive "primitive_sl" = FC.CLong

  offset# = \_ -> \_ -> 56

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_sl")
         ) => GHC.Records.HasField "primitive_sl" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_sl")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_sli" where

  type CFieldType Primitive "primitive_sli" = FC.CLong

  offset# = \_ -> \_ -> 64

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_sli")
         ) => GHC.Records.HasField "primitive_sli" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_sli")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_ul" where

  type CFieldType Primitive "primitive_ul" = FC.CULong

  offset# = \_ -> \_ -> 72

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_ul")
         ) => GHC.Records.HasField "primitive_ul" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_ul")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_uli" where

  type CFieldType Primitive "primitive_uli" = FC.CULong

  offset# = \_ -> \_ -> 80

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_uli")
         ) => GHC.Records.HasField "primitive_uli" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_uli")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_ll" where

  type CFieldType Primitive "primitive_ll" = FC.CLLong

  offset# = \_ -> \_ -> 88

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_ll")
         ) => GHC.Records.HasField "primitive_ll" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_ll")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_lli" where

  type CFieldType Primitive "primitive_lli" = FC.CLLong

  offset# = \_ -> \_ -> 96

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_lli")
         ) => GHC.Records.HasField "primitive_lli" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_lli")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_sll" where

  type CFieldType Primitive "primitive_sll" = FC.CLLong

  offset# = \_ -> \_ -> 104

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_sll")
         ) => GHC.Records.HasField "primitive_sll" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_sll")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_slli" where

  type CFieldType Primitive "primitive_slli" =
    FC.CLLong

  offset# = \_ -> \_ -> 112

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_slli")
         ) => GHC.Records.HasField "primitive_slli" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_slli")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_ull" where

  type CFieldType Primitive "primitive_ull" =
    FC.CULLong

  offset# = \_ -> \_ -> 120

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_ull")
         ) => GHC.Records.HasField "primitive_ull" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_ull")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_ulli" where

  type CFieldType Primitive "primitive_ulli" =
    FC.CULLong

  offset# = \_ -> \_ -> 128

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_ulli")
         ) => GHC.Records.HasField "primitive_ulli" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_ulli")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_f" where

  type CFieldType Primitive "primitive_f" = FC.CFloat

  offset# = \_ -> \_ -> 136

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_f")
         ) => GHC.Records.HasField "primitive_f" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_f")

instance HsBindgen.Runtime.HasCField.HasCField Primitive "primitive_d" where

  type CFieldType Primitive "primitive_d" = FC.CDouble

  offset# = \_ -> \_ -> 144

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Primitive) "primitive_d")
         ) => GHC.Records.HasField "primitive_d" (Ptr.Ptr Primitive) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"primitive_d")
