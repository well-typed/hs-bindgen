{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
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
  { a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 3:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: BG.CChar
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
      <*> HasCField.readRaw (BG.Proxy @"a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"b") ptr0

instance Marshal.WriteRaw S1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 a2 b3 ->
               HasCField.writeRaw (BG.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (BG.Proxy @"b") ptr0 b3

deriving via Marshal.EquivStorable S1 instance BG.Storable S1

deriving via Struct.IsStructViaStorable S1 instance Struct.IsStruct S1

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 3:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "a" S1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         S1 {a = y1, b = BG.getField @"b" x0}, BG.getField @"a" x0)

instance (ty ~ BG.CInt) => BG.HasField "a" (BG.Ptr S1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a")

instance HasCField.HasCField S1 "a" where

  type CFieldType S1 "a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 4:10@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "b" S1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         S1 {b = y1, a = BG.getField @"a" x0}, BG.getField @"b" x0)

instance (ty ~ BG.CChar) => BG.HasField "b" (BG.Ptr S1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b")

instance HasCField.HasCField S1 "b" where

  type CFieldType S1 "b" = BG.CChar

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct S2@

    __defined at:__ @types\/structs\/simple_structs.h 8:16@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S2_t = S2_t
  { a :: BG.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 9:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 10:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , c :: BG.CFloat
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
      <*> HasCField.readRaw (BG.Proxy @"a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"b") ptr0
      <*> HasCField.readRaw (BG.Proxy @"c") ptr0

instance Marshal.WriteRaw S2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_t a2 b3 c4 ->
               HasCField.writeRaw (BG.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (BG.Proxy @"b") ptr0 b3
            >> HasCField.writeRaw (BG.Proxy @"c") ptr0 c4

deriving via Marshal.EquivStorable S2_t instance BG.Storable S2_t

deriving via Struct.IsStructViaStorable S2_t instance Struct.IsStruct S2_t

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 9:10@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "a" S2_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2_t {a = y1, b = BG.getField @"b" x0, c = BG.getField @"c" x0}
      , BG.getField @"a" x0
      )

instance (ty ~ BG.CChar) => BG.HasField "a" (BG.Ptr S2_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a")

instance HasCField.HasCField S2_t "a" where

  type CFieldType S2_t "a" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 10:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "b" S2_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2_t {b = y1, a = BG.getField @"a" x0, c = BG.getField @"c" x0}
      , BG.getField @"b" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "b" (BG.Ptr S2_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b")

instance HasCField.HasCField S2_t "b" where

  type CFieldType S2_t "b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @c@

    __defined at:__ @types\/structs\/simple_structs.h 11:11@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CFloat) => BG.CompatHasField.HasField "c" S2_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2_t {c = y1, a = BG.getField @"a" x0, b = BG.getField @"b" x0}
      , BG.getField @"c" x0
      )

instance (ty ~ BG.CFloat) => BG.HasField "c" (BG.Ptr S2_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"c")

instance HasCField.HasCField S2_t "c" where

  type CFieldType S2_t "c" = BG.CFloat

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct S3_t@

    __defined at:__ @types\/structs\/simple_structs.h 15:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S3_t = S3_t
  { a :: BG.CChar
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
      <*> HasCField.readRaw (BG.Proxy @"a") ptr0

instance Marshal.WriteRaw S3_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_t a2 -> HasCField.writeRaw (BG.Proxy @"a") ptr0 a2

deriving via Marshal.EquivStorable S3_t instance BG.Storable S3_t

deriving via Struct.IsStructViaStorable S3_t instance Struct.IsStruct S3_t

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 16:10@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "a" S3_t ty where

  hasField =
    \x0 -> (\y1 -> S3_t {a = y1}, BG.getField @"a" x0)

instance (ty ~ BG.CChar) => BG.HasField "a" (BG.Ptr S3_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a")

instance HasCField.HasCField S3_t "a" where

  type CFieldType S3_t "a" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S4@

    __defined at:__ @types\/structs\/simple_structs.h 19:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S4 = S4
  { b :: BG.CChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 20:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 21:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , c :: BG.Ptr BG.CInt
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
      <*> HasCField.readRaw (BG.Proxy @"b") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"c") ptr0

instance Marshal.WriteRaw S4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S4 b2 a3 c4 ->
               HasCField.writeRaw (BG.Proxy @"b") ptr0 b2
            >> HasCField.writeRaw (BG.Proxy @"a") ptr0 a3
            >> HasCField.writeRaw (BG.Proxy @"c") ptr0 c4

deriving via Marshal.EquivStorable S4 instance BG.Storable S4

deriving via Struct.IsStructViaStorable S4 instance Struct.IsStruct S4

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 20:10@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "b" S4 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S4 {b = y1, a = BG.getField @"a" x0, c = BG.getField @"c" x0}
      , BG.getField @"b" x0
      )

instance (ty ~ BG.CChar) => BG.HasField "b" (BG.Ptr S4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b")

instance HasCField.HasCField S4 "b" where

  type CFieldType S4 "b" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 21:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "a" S4 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S4 {a = y1, b = BG.getField @"b" x0, c = BG.getField @"c" x0}
      , BG.getField @"a" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "a" (BG.Ptr S4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a")

instance HasCField.HasCField S4 "a" where

  type CFieldType S4 "a" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @c@

    __defined at:__ @types\/structs\/simple_structs.h 22:10@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.Ptr BG.CInt) => BG.CompatHasField.HasField "c" S4 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S4 {c = y1, b = BG.getField @"b" x0, a = BG.getField @"a" x0}
      , BG.getField @"c" x0
      )

instance (ty ~ BG.Ptr BG.CInt) => BG.HasField "c" (BG.Ptr S4) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"c")

instance HasCField.HasCField S4 "c" where

  type CFieldType S4 "c" = BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct S5@

    __defined at:__ @types\/structs\/simple_structs.h 26:16@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S5 = S5
  { a :: BG.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 27:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: BG.CInt
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
      <*> HasCField.readRaw (BG.Proxy @"a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"b") ptr0

instance Marshal.WriteRaw S5 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S5 a2 b3 ->
               HasCField.writeRaw (BG.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (BG.Proxy @"b") ptr0 b3

deriving via Marshal.EquivStorable S5 instance BG.Storable S5

deriving via Struct.IsStructViaStorable S5 instance Struct.IsStruct S5

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 27:10@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "a" S5 ty where

  hasField =
    \x0 ->
      (\y1 ->
         S5 {a = y1, b = BG.getField @"b" x0}, BG.getField @"a" x0)

instance (ty ~ BG.CChar) => BG.HasField "a" (BG.Ptr S5) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a")

instance HasCField.HasCField S5 "a" where

  type CFieldType S5 "a" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 28:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "b" S5 ty where

  hasField =
    \x0 ->
      (\y1 ->
         S5 {b = y1, a = BG.getField @"a" x0}, BG.getField @"b" x0)

instance (ty ~ BG.CInt) => BG.HasField "b" (BG.Ptr S5) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b")

instance HasCField.HasCField S5 "b" where

  type CFieldType S5 "b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct S6@

    __defined at:__ @types\/structs\/simple_structs.h 31:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S6 = S6
  { a :: BG.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 31:18@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: BG.CInt
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
      <*> HasCField.readRaw (BG.Proxy @"a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"b") ptr0

instance Marshal.WriteRaw S6 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S6 a2 b3 ->
               HasCField.writeRaw (BG.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (BG.Proxy @"b") ptr0 b3

deriving via Marshal.EquivStorable S6 instance BG.Storable S6

deriving via Struct.IsStructViaStorable S6 instance Struct.IsStruct S6

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 31:18@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "a" S6 ty where

  hasField =
    \x0 ->
      (\y1 ->
         S6 {a = y1, b = BG.getField @"b" x0}, BG.getField @"a" x0)

instance (ty ~ BG.CChar) => BG.HasField "a" (BG.Ptr S6) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a")

instance HasCField.HasCField S6 "a" where

  type CFieldType S6 "a" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 31:25@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "b" S6 ty where

  hasField =
    \x0 ->
      (\y1 ->
         S6 {b = y1, a = BG.getField @"a" x0}, BG.getField @"b" x0)

instance (ty ~ BG.CInt) => BG.HasField "b" (BG.Ptr S6) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b")

instance HasCField.HasCField S6 "b" where

  type CFieldType S6 "b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct \@S7a_Aux@

    __defined at:__ @types\/structs\/simple_structs.h 34:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S7a_Aux = S7a_Aux
  { a :: BG.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 34:23@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: BG.CInt
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
      <*> HasCField.readRaw (BG.Proxy @"a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"b") ptr0

instance Marshal.WriteRaw S7a_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7a_Aux a2 b3 ->
               HasCField.writeRaw (BG.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (BG.Proxy @"b") ptr0 b3

deriving via Marshal.EquivStorable S7a_Aux instance BG.Storable S7a_Aux

deriving via Struct.IsStructViaStorable S7a_Aux instance Struct.IsStruct S7a_Aux

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 34:23@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "a" S7a_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         S7a_Aux {a = y1, b = BG.getField @"b" x0}, BG.getField @"a" x0)

instance (ty ~ BG.CChar) => BG.HasField "a" (BG.Ptr S7a_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a")

instance HasCField.HasCField S7a_Aux "a" where

  type CFieldType S7a_Aux "a" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 34:30@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "b" S7a_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         S7a_Aux {b = y1, a = BG.getField @"a" x0}, BG.getField @"b" x0)

instance (ty ~ BG.CInt) => BG.HasField "b" (BG.Ptr S7a_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b")

instance HasCField.HasCField S7a_Aux "b" where

  type CFieldType S7a_Aux "b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @S7a@

    __defined at:__ @types\/structs\/simple_structs.h 34:36@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
newtype S7a = S7a
  { unwrap :: BG.Ptr S7a_Aux
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
         ) => BG.CompatHasField.HasField "unwrap" S7a ty where

  hasField =
    \x0 ->
      (\y1 -> S7a {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ BG.Ptr S7a_Aux
         ) => BG.HasField "unwrap" (BG.Ptr S7a) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField S7a "unwrap" where

  type CFieldType S7a "unwrap" = BG.Ptr S7a_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@S7b_Aux@

    __defined at:__ @types\/structs\/simple_structs.h 35:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S7b_Aux = S7b_Aux
  { a :: BG.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 35:23@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: BG.CInt
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
      <*> HasCField.readRaw (BG.Proxy @"a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"b") ptr0

instance Marshal.WriteRaw S7b_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7b_Aux a2 b3 ->
               HasCField.writeRaw (BG.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (BG.Proxy @"b") ptr0 b3

deriving via Marshal.EquivStorable S7b_Aux instance BG.Storable S7b_Aux

deriving via Struct.IsStructViaStorable S7b_Aux instance Struct.IsStruct S7b_Aux

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/simple_structs.h 35:23@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "a" S7b_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         S7b_Aux {a = y1, b = BG.getField @"b" x0}, BG.getField @"a" x0)

instance (ty ~ BG.CChar) => BG.HasField "a" (BG.Ptr S7b_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a")

instance HasCField.HasCField S7b_Aux "a" where

  type CFieldType S7b_Aux "a" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/simple_structs.h 35:30@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "b" S7b_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         S7b_Aux {b = y1, a = BG.getField @"a" x0}, BG.getField @"b" x0)

instance (ty ~ BG.CInt) => BG.HasField "b" (BG.Ptr S7b_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b")

instance HasCField.HasCField S7b_Aux "b" where

  type CFieldType S7b_Aux "b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @S7b@

    __defined at:__ @types\/structs\/simple_structs.h 35:38@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
newtype S7b = S7b
  { unwrap :: BG.Ptr (BG.Ptr (BG.Ptr S7b_Aux))
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
         ) => BG.CompatHasField.HasField "unwrap" S7b ty where

  hasField =
    \x0 ->
      (\y1 -> S7b {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ BG.Ptr (BG.Ptr (BG.Ptr S7b_Aux))
         ) => BG.HasField "unwrap" (BG.Ptr S7b) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField S7b "unwrap" where

  type CFieldType S7b "unwrap" =
    BG.Ptr (BG.Ptr (BG.Ptr S7b_Aux))

  offset# = \_ -> \_ -> 0
