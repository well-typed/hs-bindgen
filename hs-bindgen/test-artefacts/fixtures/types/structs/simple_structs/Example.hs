{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.S1(..)
    , Example.S2_t(..)
    , Example.S3_t(..)
    , Example.S4(..)
    , Example.S5(..)
    , Example.S6(..)
    , Example.S7a_Aux(..)
    , Example.S7a(..)
    , Example.S7b_Aux(..)
    , Example.S7b(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct S1@

    __defined at:__ @types\/structs\/simple_structs.h 2:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S1 = S1
  { s1_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 3:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s1_b :: BG.CChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 4:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S1 where

  readRaw =
    \ptr0 ->
          pure S1
      <*> HasCField.readRaw (BG.Proxy @"s1_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s1_b") ptr0

instance Marshal.WriteRaw S1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_a2 s1_b3 ->
               HasCField.writeRaw (BG.Proxy @"s1_a") ptr0 s1_a2
            >> HasCField.writeRaw (BG.Proxy @"s1_b") ptr0 s1_b3

deriving via Marshal.EquivStorable S1 instance BG.Storable S1

deriving via Struct.IsStructViaStorable S1 instance Struct.IsStruct S1

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 3:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s1_a" S1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S1 {s1_a = y1, s1_b = BG.getField @"s1_b" x0}
      , BG.getField @"s1_a" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s1_a" (BG.Ptr S1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s1_a")

instance HasCField.HasCField S1 "s1_a" where

  type CFieldType S1 "s1_a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 4:10@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "s1_b" S1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S1 {s1_b = y1, s1_a = BG.getField @"s1_a" x0}
      , BG.getField @"s1_b" x0
      )

instance (ty ~ BG.CChar) => BG.HasField "s1_b" (BG.Ptr S1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s1_b")

instance HasCField.HasCField S1 "s1_b" where

  type CFieldType S1 "s1_b" = BG.CChar

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct S2@

    __defined at:__ @types\/structs\/simple_structs.h 8:16@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S2_t = S2_t
  { s2_t_a :: BG.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 9:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s2_t_b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 10:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s2_t_c :: BG.CFloat
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/simple_structs.h 11:11@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S2_t where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2_t where

  readRaw =
    \ptr0 ->
          pure S2_t
      <*> HasCField.readRaw (BG.Proxy @"s2_t_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s2_t_b") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s2_t_c") ptr0

instance Marshal.WriteRaw S2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_t s2_t_a2 s2_t_b3 s2_t_c4 ->
               HasCField.writeRaw (BG.Proxy @"s2_t_a") ptr0 s2_t_a2
            >> HasCField.writeRaw (BG.Proxy @"s2_t_b") ptr0 s2_t_b3
            >> HasCField.writeRaw (BG.Proxy @"s2_t_c") ptr0 s2_t_c4

deriving via Marshal.EquivStorable S2_t instance BG.Storable S2_t

deriving via Struct.IsStructViaStorable S2_t instance Struct.IsStruct S2_t

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 9:10@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "s2_t_a" S2_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2_t { s2_t_a = y1
               , s2_t_b = BG.getField @"s2_t_b" x0
               , s2_t_c = BG.getField @"s2_t_c" x0
               }
      , BG.getField @"s2_t_a" x0
      )

instance (ty ~ BG.CChar) => BG.HasField "s2_t_a" (BG.Ptr S2_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s2_t_a")

instance HasCField.HasCField S2_t "s2_t_a" where

  type CFieldType S2_t "s2_t_a" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 10:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s2_t_b" S2_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2_t { s2_t_b = y1
               , s2_t_a = BG.getField @"s2_t_a" x0
               , s2_t_c = BG.getField @"s2_t_c" x0
               }
      , BG.getField @"s2_t_b" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s2_t_b" (BG.Ptr S2_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s2_t_b")

instance HasCField.HasCField S2_t "s2_t_b" where

  type CFieldType S2_t "s2_t_b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @c@

    __defined at:__ @types\/structs\/simple_structs.h 11:11@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CFloat) => BG.CompatHasField.HasField "s2_t_c" S2_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2_t { s2_t_c = y1
               , s2_t_a = BG.getField @"s2_t_a" x0
               , s2_t_b = BG.getField @"s2_t_b" x0
               }
      , BG.getField @"s2_t_c" x0
      )

instance ( ty ~ BG.CFloat
         ) => BG.HasField "s2_t_c" (BG.Ptr S2_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s2_t_c")

instance HasCField.HasCField S2_t "s2_t_c" where

  type CFieldType S2_t "s2_t_c" = BG.CFloat

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct S3_t@

    __defined at:__ @types\/structs\/simple_structs.h 15:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S3_t = S3_t
  { s3_t_a :: BG.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 16:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S3_t where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw S3_t where

  readRaw =
    \ptr0 ->
          pure S3_t
      <*> HasCField.readRaw (BG.Proxy @"s3_t_a") ptr0

instance Marshal.WriteRaw S3_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_t s3_t_a2 ->
            HasCField.writeRaw (BG.Proxy @"s3_t_a") ptr0 s3_t_a2

deriving via Marshal.EquivStorable S3_t instance BG.Storable S3_t

deriving via Struct.IsStructViaStorable S3_t instance Struct.IsStruct S3_t

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 16:10@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "s3_t_a" S3_t ty where

  hasField =
    \x0 ->
      (\y1 -> S3_t {s3_t_a = y1}, BG.getField @"s3_t_a" x0)

instance (ty ~ BG.CChar) => BG.HasField "s3_t_a" (BG.Ptr S3_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s3_t_a")

instance HasCField.HasCField S3_t "s3_t_a" where

  type CFieldType S3_t "s3_t_a" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S4@

    __defined at:__ @types\/structs\/simple_structs.h 19:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S4 = S4
  { s4_b :: BG.CChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 20:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s4_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 21:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s4_c :: BG.Ptr BG.CInt
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/simple_structs.h 22:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S4 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw S4 where

  readRaw =
    \ptr0 ->
          pure S4
      <*> HasCField.readRaw (BG.Proxy @"s4_b") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s4_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s4_c") ptr0

instance Marshal.WriteRaw S4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S4 s4_b2 s4_a3 s4_c4 ->
               HasCField.writeRaw (BG.Proxy @"s4_b") ptr0 s4_b2
            >> HasCField.writeRaw (BG.Proxy @"s4_a") ptr0 s4_a3
            >> HasCField.writeRaw (BG.Proxy @"s4_c") ptr0 s4_c4

deriving via Marshal.EquivStorable S4 instance BG.Storable S4

deriving via Struct.IsStructViaStorable S4 instance Struct.IsStruct S4

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 20:10@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "s4_b" S4 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S4 {s4_b = y1, s4_a = BG.getField @"s4_a" x0, s4_c = BG.getField @"s4_c" x0}
      , BG.getField @"s4_b" x0
      )

instance (ty ~ BG.CChar) => BG.HasField "s4_b" (BG.Ptr S4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s4_b")

instance HasCField.HasCField S4 "s4_b" where

  type CFieldType S4 "s4_b" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 21:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s4_a" S4 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S4 {s4_a = y1, s4_b = BG.getField @"s4_b" x0, s4_c = BG.getField @"s4_c" x0}
      , BG.getField @"s4_a" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s4_a" (BG.Ptr S4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s4_a")

instance HasCField.HasCField S4 "s4_a" where

  type CFieldType S4 "s4_a" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @c@

    __defined at:__ @types\/structs\/simple_structs.h 22:10@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.Ptr BG.CInt) => BG.CompatHasField.HasField "s4_c" S4 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S4 {s4_c = y1, s4_b = BG.getField @"s4_b" x0, s4_a = BG.getField @"s4_a" x0}
      , BG.getField @"s4_c" x0
      )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "s4_c" (BG.Ptr S4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s4_c")

instance HasCField.HasCField S4 "s4_c" where

  type CFieldType S4 "s4_c" = BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct S5@

    __defined at:__ @types\/structs\/simple_structs.h 26:16@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S5 = S5
  { s5_a :: BG.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 27:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s5_b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 28:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S5 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S5 where

  readRaw =
    \ptr0 ->
          pure S5
      <*> HasCField.readRaw (BG.Proxy @"s5_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s5_b") ptr0

instance Marshal.WriteRaw S5 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S5 s5_a2 s5_b3 ->
               HasCField.writeRaw (BG.Proxy @"s5_a") ptr0 s5_a2
            >> HasCField.writeRaw (BG.Proxy @"s5_b") ptr0 s5_b3

deriving via Marshal.EquivStorable S5 instance BG.Storable S5

deriving via Struct.IsStructViaStorable S5 instance Struct.IsStruct S5

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 27:10@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "s5_a" S5 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S5 {s5_a = y1, s5_b = BG.getField @"s5_b" x0}
      , BG.getField @"s5_a" x0
      )

instance (ty ~ BG.CChar) => BG.HasField "s5_a" (BG.Ptr S5) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s5_a")

instance HasCField.HasCField S5 "s5_a" where

  type CFieldType S5 "s5_a" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 28:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s5_b" S5 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S5 {s5_b = y1, s5_a = BG.getField @"s5_a" x0}
      , BG.getField @"s5_b" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s5_b" (BG.Ptr S5) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s5_b")

instance HasCField.HasCField S5 "s5_b" where

  type CFieldType S5 "s5_b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct S6@

    __defined at:__ @types\/structs\/simple_structs.h 31:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S6 = S6
  { s6_a :: BG.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 31:18@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s6_b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 31:25@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S6 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S6 where

  readRaw =
    \ptr0 ->
          pure S6
      <*> HasCField.readRaw (BG.Proxy @"s6_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s6_b") ptr0

instance Marshal.WriteRaw S6 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S6 s6_a2 s6_b3 ->
               HasCField.writeRaw (BG.Proxy @"s6_a") ptr0 s6_a2
            >> HasCField.writeRaw (BG.Proxy @"s6_b") ptr0 s6_b3

deriving via Marshal.EquivStorable S6 instance BG.Storable S6

deriving via Struct.IsStructViaStorable S6 instance Struct.IsStruct S6

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 31:18@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "s6_a" S6 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S6 {s6_a = y1, s6_b = BG.getField @"s6_b" x0}
      , BG.getField @"s6_a" x0
      )

instance (ty ~ BG.CChar) => BG.HasField "s6_a" (BG.Ptr S6) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s6_a")

instance HasCField.HasCField S6 "s6_a" where

  type CFieldType S6 "s6_a" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 31:25@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s6_b" S6 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S6 {s6_b = y1, s6_a = BG.getField @"s6_a" x0}
      , BG.getField @"s6_b" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s6_b" (BG.Ptr S6) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s6_b")

instance HasCField.HasCField S6 "s6_b" where

  type CFieldType S6 "s6_b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct \@S7a_Aux@

    __defined at:__ @types\/structs\/simple_structs.h 34:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S7a_Aux = S7a_Aux
  { s7a_Aux_a :: BG.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 34:23@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s7a_Aux_b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 34:30@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S7a_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S7a_Aux where

  readRaw =
    \ptr0 ->
          pure S7a_Aux
      <*> HasCField.readRaw (BG.Proxy @"s7a_Aux_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s7a_Aux_b") ptr0

instance Marshal.WriteRaw S7a_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7a_Aux s7a_Aux_a2 s7a_Aux_b3 ->
               HasCField.writeRaw (BG.Proxy @"s7a_Aux_a") ptr0 s7a_Aux_a2
            >> HasCField.writeRaw (BG.Proxy @"s7a_Aux_b") ptr0 s7a_Aux_b3

deriving via Marshal.EquivStorable S7a_Aux instance BG.Storable S7a_Aux

deriving via Struct.IsStructViaStorable S7a_Aux instance Struct.IsStruct S7a_Aux

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 34:23@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "s7a_Aux_a" S7a_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S7a_Aux {s7a_Aux_a = y1, s7a_Aux_b = BG.getField @"s7a_Aux_b" x0}
      , BG.getField @"s7a_Aux_a" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "s7a_Aux_a" (BG.Ptr S7a_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s7a_Aux_a")

instance HasCField.HasCField S7a_Aux "s7a_Aux_a" where

  type CFieldType S7a_Aux "s7a_Aux_a" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 34:30@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "s7a_Aux_b" S7a_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S7a_Aux {s7a_Aux_b = y1, s7a_Aux_a = BG.getField @"s7a_Aux_a" x0}
      , BG.getField @"s7a_Aux_b" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "s7a_Aux_b" (BG.Ptr S7a_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s7a_Aux_b")

instance HasCField.HasCField S7a_Aux "s7a_Aux_b" where

  type CFieldType S7a_Aux "s7a_Aux_b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @S7a@

    __defined at:__ @types\/structs\/simple_structs.h 34:36@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
newtype S7a = S7a
  { unwrapS7a :: BG.Ptr S7a_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr S7a_Aux
         ) => BG.CompatHasField.HasField "unwrapS7a" S7a ty where

  hasField =
    \x0 ->
      (\y1 ->
         S7a {unwrapS7a = y1}, BG.getField @"unwrapS7a" x0)

instance ( ty ~ BG.Ptr S7a_Aux
         ) => BG.HasField "unwrapS7a" (BG.Ptr S7a) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapS7a")

instance HasCField.HasCField S7a "unwrapS7a" where

  type CFieldType S7a "unwrapS7a" = BG.Ptr S7a_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@S7b_Aux@

    __defined at:__ @types\/structs\/simple_structs.h 35:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S7b_Aux = S7b_Aux
  { s7b_Aux_a :: BG.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 35:23@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s7b_Aux_b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 35:30@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S7b_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S7b_Aux where

  readRaw =
    \ptr0 ->
          pure S7b_Aux
      <*> HasCField.readRaw (BG.Proxy @"s7b_Aux_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s7b_Aux_b") ptr0

instance Marshal.WriteRaw S7b_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7b_Aux s7b_Aux_a2 s7b_Aux_b3 ->
               HasCField.writeRaw (BG.Proxy @"s7b_Aux_a") ptr0 s7b_Aux_a2
            >> HasCField.writeRaw (BG.Proxy @"s7b_Aux_b") ptr0 s7b_Aux_b3

deriving via Marshal.EquivStorable S7b_Aux instance BG.Storable S7b_Aux

deriving via Struct.IsStructViaStorable S7b_Aux instance Struct.IsStruct S7b_Aux

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 35:23@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "s7b_Aux_a" S7b_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S7b_Aux {s7b_Aux_a = y1, s7b_Aux_b = BG.getField @"s7b_Aux_b" x0}
      , BG.getField @"s7b_Aux_a" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "s7b_Aux_a" (BG.Ptr S7b_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s7b_Aux_a")

instance HasCField.HasCField S7b_Aux "s7b_Aux_a" where

  type CFieldType S7b_Aux "s7b_Aux_a" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 35:30@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "s7b_Aux_b" S7b_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S7b_Aux {s7b_Aux_b = y1, s7b_Aux_a = BG.getField @"s7b_Aux_a" x0}
      , BG.getField @"s7b_Aux_b" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "s7b_Aux_b" (BG.Ptr S7b_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s7b_Aux_b")

instance HasCField.HasCField S7b_Aux "s7b_Aux_b" where

  type CFieldType S7b_Aux "s7b_Aux_b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @S7b@

    __defined at:__ @types\/structs\/simple_structs.h 35:38@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
newtype S7b = S7b
  { unwrapS7b :: BG.Ptr (BG.Ptr (BG.Ptr S7b_Aux))
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr (BG.Ptr (BG.Ptr S7b_Aux))
         ) => BG.CompatHasField.HasField "unwrapS7b" S7b ty where

  hasField =
    \x0 ->
      (\y1 ->
         S7b {unwrapS7b = y1}, BG.getField @"unwrapS7b" x0)

instance ( ty ~ BG.Ptr (BG.Ptr (BG.Ptr S7b_Aux))
         ) => BG.HasField "unwrapS7b" (BG.Ptr S7b) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapS7b")

instance HasCField.HasCField S7b "unwrapS7b" where

  type CFieldType S7b "unwrapS7b" =
    BG.Ptr (BG.Ptr (BG.Ptr S7b_Aux))

  offset# = \_ -> \_ -> 0
