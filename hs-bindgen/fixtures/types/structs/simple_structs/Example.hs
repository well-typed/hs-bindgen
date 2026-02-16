{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Ord, Show, pure)

{-| __C declaration:__ @struct S1@

    __defined at:__ @types\/structs\/simple_structs.h 2:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S1 = S1
  { s1_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 3:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s1_b :: FC.CChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 4:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S1 where

  readRaw =
    \ptr0 ->
          pure S1
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s1_a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s1_b") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_a2 s1_b3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s1_a") ptr0 s1_a2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s1_b") ptr0 s1_b3

deriving via HsBindgen.Runtime.Marshal.EquivStorable S1 instance F.Storable S1

instance HsBindgen.Runtime.HasCField.HasCField S1 "s1_a" where

  type CFieldType S1 "s1_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "s1_a" (Ptr.Ptr S1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s1_a")

instance HsBindgen.Runtime.HasCField.HasCField S1 "s1_b" where

  type CFieldType S1 "s1_b" = FC.CChar

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "s1_b" (Ptr.Ptr S1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s1_b")

{-| __C declaration:__ @struct S2@

    __defined at:__ @types\/structs\/simple_structs.h 8:16@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S2_t = S2_t
  { s2_t_a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 9:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s2_t_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 10:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s2_t_c :: FC.CFloat
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/simple_structs.h 11:11@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S2_t where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S2_t where

  readRaw =
    \ptr0 ->
          pure S2_t
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s2_t_a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s2_t_b") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s2_t_c") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_t s2_t_a2 s2_t_b3 s2_t_c4 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s2_t_a") ptr0 s2_t_a2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s2_t_b") ptr0 s2_t_b3
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s2_t_c") ptr0 s2_t_c4

deriving via HsBindgen.Runtime.Marshal.EquivStorable S2_t instance F.Storable S2_t

instance HsBindgen.Runtime.HasCField.HasCField S2_t "s2_t_a" where

  type CFieldType S2_t "s2_t_a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "s2_t_a" (Ptr.Ptr S2_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s2_t_a")

instance HsBindgen.Runtime.HasCField.HasCField S2_t "s2_t_b" where

  type CFieldType S2_t "s2_t_b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "s2_t_b" (Ptr.Ptr S2_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s2_t_b")

instance HsBindgen.Runtime.HasCField.HasCField S2_t "s2_t_c" where

  type CFieldType S2_t "s2_t_c" = FC.CFloat

  offset# = \_ -> \_ -> 8

instance ( TyEq ty FC.CFloat
         ) => GHC.Records.HasField "s2_t_c" (Ptr.Ptr S2_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s2_t_c")

{-| __C declaration:__ @struct S3_t@

    __defined at:__ @types\/structs\/simple_structs.h 15:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S3_t = S3_t
  { s3_t_a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 16:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S3_t where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S3_t where

  readRaw =
    \ptr0 ->
          pure S3_t
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s3_t_a") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S3_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_t s3_t_a2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s3_t_a") ptr0 s3_t_a2

deriving via HsBindgen.Runtime.Marshal.EquivStorable S3_t instance F.Storable S3_t

instance HsBindgen.Runtime.HasCField.HasCField S3_t "s3_t_a" where

  type CFieldType S3_t "s3_t_a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "s3_t_a" (Ptr.Ptr S3_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s3_t_a")

{-| __C declaration:__ @struct S4@

    __defined at:__ @types\/structs\/simple_structs.h 19:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S4 = S4
  { s4_b :: FC.CChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 20:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s4_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 21:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s4_c :: Ptr.Ptr FC.CInt
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/simple_structs.h 22:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S4 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S4 where

  readRaw =
    \ptr0 ->
          pure S4
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s4_b") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s4_a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s4_c") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S4 s4_b2 s4_a3 s4_c4 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s4_b") ptr0 s4_b2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s4_a") ptr0 s4_a3
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s4_c") ptr0 s4_c4

deriving via HsBindgen.Runtime.Marshal.EquivStorable S4 instance F.Storable S4

instance HsBindgen.Runtime.HasCField.HasCField S4 "s4_b" where

  type CFieldType S4 "s4_b" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "s4_b" (Ptr.Ptr S4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s4_b")

instance HsBindgen.Runtime.HasCField.HasCField S4 "s4_a" where

  type CFieldType S4 "s4_a" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "s4_a" (Ptr.Ptr S4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s4_a")

instance HsBindgen.Runtime.HasCField.HasCField S4 "s4_c" where

  type CFieldType S4 "s4_c" = Ptr.Ptr FC.CInt

  offset# = \_ -> \_ -> 8

instance ( TyEq ty (Ptr.Ptr FC.CInt)
         ) => GHC.Records.HasField "s4_c" (Ptr.Ptr S4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s4_c")

{-| __C declaration:__ @struct S5@

    __defined at:__ @types\/structs\/simple_structs.h 26:16@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S5 = S5
  { s5_a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 27:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s5_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 28:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S5 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S5 where

  readRaw =
    \ptr0 ->
          pure S5
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s5_a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s5_b") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S5 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S5 s5_a2 s5_b3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s5_a") ptr0 s5_a2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s5_b") ptr0 s5_b3

deriving via HsBindgen.Runtime.Marshal.EquivStorable S5 instance F.Storable S5

instance HsBindgen.Runtime.HasCField.HasCField S5 "s5_a" where

  type CFieldType S5 "s5_a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "s5_a" (Ptr.Ptr S5) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s5_a")

instance HsBindgen.Runtime.HasCField.HasCField S5 "s5_b" where

  type CFieldType S5 "s5_b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "s5_b" (Ptr.Ptr S5) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s5_b")

{-| __C declaration:__ @struct S6@

    __defined at:__ @types\/structs\/simple_structs.h 31:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S6 = S6
  { s6_a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 31:18@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s6_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 31:25@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S6 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S6 where

  readRaw =
    \ptr0 ->
          pure S6
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s6_a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s6_b") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S6 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S6 s6_a2 s6_b3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s6_a") ptr0 s6_a2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s6_b") ptr0 s6_b3

deriving via HsBindgen.Runtime.Marshal.EquivStorable S6 instance F.Storable S6

instance HsBindgen.Runtime.HasCField.HasCField S6 "s6_a" where

  type CFieldType S6 "s6_a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "s6_a" (Ptr.Ptr S6) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s6_a")

instance HsBindgen.Runtime.HasCField.HasCField S6 "s6_b" where

  type CFieldType S6 "s6_b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "s6_b" (Ptr.Ptr S6) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s6_b")

{-| __C declaration:__ @struct \@S7a_Aux@

    __defined at:__ @types\/structs\/simple_structs.h 34:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S7a_Aux = S7a_Aux
  { s7a_Aux_a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 34:23@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s7a_Aux_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 34:30@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S7a_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S7a_Aux where

  readRaw =
    \ptr0 ->
          pure S7a_Aux
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s7a_Aux_a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s7a_Aux_b") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S7a_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7a_Aux s7a_Aux_a2 s7a_Aux_b3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s7a_Aux_a") ptr0 s7a_Aux_a2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s7a_Aux_b") ptr0 s7a_Aux_b3

deriving via HsBindgen.Runtime.Marshal.EquivStorable S7a_Aux instance F.Storable S7a_Aux

instance HsBindgen.Runtime.HasCField.HasCField S7a_Aux "s7a_Aux_a" where

  type CFieldType S7a_Aux "s7a_Aux_a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "s7a_Aux_a" (Ptr.Ptr S7a_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s7a_Aux_a")

instance HsBindgen.Runtime.HasCField.HasCField S7a_Aux "s7a_Aux_b" where

  type CFieldType S7a_Aux "s7a_Aux_b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "s7a_Aux_b" (Ptr.Ptr S7a_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s7a_Aux_b")

{-| __C declaration:__ @S7a@

    __defined at:__ @types\/structs\/simple_structs.h 34:36@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
newtype S7a = S7a
  { unwrapS7a :: Ptr.Ptr S7a_Aux
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.Ptr S7a_Aux)
         ) => GHC.Records.HasField "unwrapS7a" (Ptr.Ptr S7a) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapS7a")

instance HsBindgen.Runtime.HasCField.HasCField S7a "unwrapS7a" where

  type CFieldType S7a "unwrapS7a" = Ptr.Ptr S7a_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@S7b_Aux@

    __defined at:__ @types\/structs\/simple_structs.h 35:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S7b_Aux = S7b_Aux
  { s7b_Aux_a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 35:23@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s7b_Aux_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 35:30@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S7b_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S7b_Aux where

  readRaw =
    \ptr0 ->
          pure S7b_Aux
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s7b_Aux_a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s7b_Aux_b") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S7b_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7b_Aux s7b_Aux_a2 s7b_Aux_b3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s7b_Aux_a") ptr0 s7b_Aux_a2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s7b_Aux_b") ptr0 s7b_Aux_b3

deriving via HsBindgen.Runtime.Marshal.EquivStorable S7b_Aux instance F.Storable S7b_Aux

instance HsBindgen.Runtime.HasCField.HasCField S7b_Aux "s7b_Aux_a" where

  type CFieldType S7b_Aux "s7b_Aux_a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "s7b_Aux_a" (Ptr.Ptr S7b_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s7b_Aux_a")

instance HsBindgen.Runtime.HasCField.HasCField S7b_Aux "s7b_Aux_b" where

  type CFieldType S7b_Aux "s7b_Aux_b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "s7b_Aux_b" (Ptr.Ptr S7b_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s7b_Aux_b")

{-| __C declaration:__ @S7b@

    __defined at:__ @types\/structs\/simple_structs.h 35:38@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
newtype S7b = S7b
  { unwrapS7b :: Ptr.Ptr (Ptr.Ptr (Ptr.Ptr S7b_Aux))
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.Ptr (Ptr.Ptr (Ptr.Ptr S7b_Aux)))
         ) => GHC.Records.HasField "unwrapS7b" (Ptr.Ptr S7b) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapS7b")

instance HsBindgen.Runtime.HasCField.HasCField S7b "unwrapS7b" where

  type CFieldType S7b "unwrapS7b" =
    Ptr.Ptr (Ptr.Ptr (Ptr.Ptr S7b_Aux))

  offset# = \_ -> \_ -> 0
