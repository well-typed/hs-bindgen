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
    ( Example.S1_c(..)
    , Example.S1(..)
    , Example.S2_inner_deep(..)
    , Example.S2_inner(..)
    , Example.S2(..)
    , Example.S3(..)
    , Example.S3_c(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct \@S1_c@

    __defined at:__ @types\/structs\/anonymous.h 3:3@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S1_c = S1_c
  { s1_c_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/anonymous.h 4:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s1_c_b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/anonymous.h 5:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S1_c where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S1_c where

  readRaw =
    \ptr0 ->
          pure S1_c
      <*> HasCField.readRaw (BG.Proxy @"s1_c_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s1_c_b") ptr0

instance Marshal.WriteRaw S1_c where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1_c s1_c_a2 s1_c_b3 ->
               HasCField.writeRaw (BG.Proxy @"s1_c_a") ptr0 s1_c_a2
            >> HasCField.writeRaw (BG.Proxy @"s1_c_b") ptr0 s1_c_b3

deriving via Marshal.EquivStorable S1_c instance BG.Storable S1_c

deriving via Struct.IsStructViaStorable S1_c instance Struct.IsStruct S1_c

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/anonymous.h 4:9@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s1_c_a" S1_c ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S1_c {s1_c_a = y1, s1_c_b = BG.getField @"s1_c_b" x0}
      , BG.getField @"s1_c_a" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s1_c_a" (BG.Ptr S1_c) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s1_c_a")

instance HasCField.HasCField S1_c "s1_c_a" where

  type CFieldType S1_c "s1_c_a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/anonymous.h 5:9@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s1_c_b" S1_c ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S1_c {s1_c_b = y1, s1_c_a = BG.getField @"s1_c_a" x0}
      , BG.getField @"s1_c_b" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s1_c_b" (BG.Ptr S1_c) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s1_c_b")

instance HasCField.HasCField S1_c "s1_c_b" where

  type CFieldType S1_c "s1_c_b" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct S1@

    __defined at:__ @types\/structs\/anonymous.h 2:8@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S1 = S1
  { s1_c :: S1_c
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/anonymous.h 6:5@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s1_d :: BG.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/anonymous.h 8:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S1 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S1 where

  readRaw =
    \ptr0 ->
          pure S1
      <*> HasCField.readRaw (BG.Proxy @"s1_c") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s1_d") ptr0

instance Marshal.WriteRaw S1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_c2 s1_d3 ->
               HasCField.writeRaw (BG.Proxy @"s1_c") ptr0 s1_c2
            >> HasCField.writeRaw (BG.Proxy @"s1_d") ptr0 s1_d3

deriving via Marshal.EquivStorable S1 instance BG.Storable S1

deriving via Struct.IsStructViaStorable S1 instance Struct.IsStruct S1

{-| __C declaration:__ @c@

    __defined at:__ @types\/structs\/anonymous.h 6:5@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance (ty ~ S1_c) => BG.CompatHasField.HasField "s1_c" S1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S1 {s1_c = y1, s1_d = BG.getField @"s1_d" x0}
      , BG.getField @"s1_c" x0
      )

instance (ty ~ S1_c) => BG.HasField "s1_c" (BG.Ptr S1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s1_c")

instance HasCField.HasCField S1 "s1_c" where

  type CFieldType S1 "s1_c" = S1_c

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @d@

    __defined at:__ @types\/structs\/anonymous.h 8:7@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s1_d" S1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S1 {s1_d = y1, s1_c = BG.getField @"s1_c" x0}
      , BG.getField @"s1_d" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s1_d" (BG.Ptr S1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s1_d")

instance HasCField.HasCField S1 "s1_d" where

  type CFieldType S1 "s1_d" = BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct \@S2_inner_deep@

    __defined at:__ @types\/structs\/anonymous.h 15:5@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S2_inner_deep = S2_inner_deep
  { s2_inner_deep_b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/anonymous.h 16:11@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S2_inner_deep where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2_inner_deep where

  readRaw =
    \ptr0 ->
          pure S2_inner_deep
      <*> HasCField.readRaw (BG.Proxy @"s2_inner_deep_b") ptr0

instance Marshal.WriteRaw S2_inner_deep where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_inner_deep s2_inner_deep_b2 ->
            HasCField.writeRaw (BG.Proxy @"s2_inner_deep_b") ptr0 s2_inner_deep_b2

deriving via Marshal.EquivStorable S2_inner_deep instance BG.Storable S2_inner_deep

deriving via Struct.IsStructViaStorable S2_inner_deep instance Struct.IsStruct S2_inner_deep

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/anonymous.h 16:11@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "s2_inner_deep_b" S2_inner_deep ty where

  hasField =
    \x0 ->
      ( \y1 -> S2_inner_deep {s2_inner_deep_b = y1}
      , BG.getField @"s2_inner_deep_b" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "s2_inner_deep_b" (BG.Ptr S2_inner_deep) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s2_inner_deep_b")

instance HasCField.HasCField S2_inner_deep "s2_inner_deep_b" where

  type CFieldType S2_inner_deep "s2_inner_deep_b" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@S2_inner@

    __defined at:__ @types\/structs\/anonymous.h 13:3@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S2_inner = S2_inner
  { s2_inner_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/anonymous.h 14:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s2_inner_deep :: S2_inner_deep
    {- ^ __C declaration:__ @deep@

         __defined at:__ @types\/structs\/anonymous.h 17:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S2_inner where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2_inner where

  readRaw =
    \ptr0 ->
          pure S2_inner
      <*> HasCField.readRaw (BG.Proxy @"s2_inner_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s2_inner_deep") ptr0

instance Marshal.WriteRaw S2_inner where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_inner s2_inner_a2 s2_inner_deep3 ->
               HasCField.writeRaw (BG.Proxy @"s2_inner_a") ptr0 s2_inner_a2
            >> HasCField.writeRaw (BG.Proxy @"s2_inner_deep") ptr0 s2_inner_deep3

deriving via Marshal.EquivStorable S2_inner instance BG.Storable S2_inner

deriving via Struct.IsStructViaStorable S2_inner instance Struct.IsStruct S2_inner

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/anonymous.h 14:9@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "s2_inner_a" S2_inner ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2_inner {s2_inner_a = y1, s2_inner_deep = BG.getField @"s2_inner_deep" x0}
      , BG.getField @"s2_inner_a" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "s2_inner_a" (BG.Ptr S2_inner) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s2_inner_a")

instance HasCField.HasCField S2_inner "s2_inner_a" where

  type CFieldType S2_inner "s2_inner_a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @deep@

    __defined at:__ @types\/structs\/anonymous.h 17:7@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance ( ty ~ S2_inner_deep
         ) => BG.CompatHasField.HasField "s2_inner_deep" S2_inner ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2_inner {s2_inner_deep = y1, s2_inner_a = BG.getField @"s2_inner_a" x0}
      , BG.getField @"s2_inner_deep" x0
      )

instance ( ty ~ S2_inner_deep
         ) => BG.HasField "s2_inner_deep" (BG.Ptr S2_inner) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s2_inner_deep")

instance HasCField.HasCField S2_inner "s2_inner_deep" where

  type CFieldType S2_inner "s2_inner_deep" =
    S2_inner_deep

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct S2@

    __defined at:__ @types\/structs\/anonymous.h 12:8@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S2 = S2
  { s2_inner :: S2_inner
    {- ^ __C declaration:__ @inner@

         __defined at:__ @types\/structs\/anonymous.h 18:5@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s2_d :: BG.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/anonymous.h 20:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S2 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2 where

  readRaw =
    \ptr0 ->
          pure S2
      <*> HasCField.readRaw (BG.Proxy @"s2_inner") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s2_d") ptr0

instance Marshal.WriteRaw S2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_inner2 s2_d3 ->
               HasCField.writeRaw (BG.Proxy @"s2_inner") ptr0 s2_inner2
            >> HasCField.writeRaw (BG.Proxy @"s2_d") ptr0 s2_d3

deriving via Marshal.EquivStorable S2 instance BG.Storable S2

deriving via Struct.IsStructViaStorable S2 instance Struct.IsStruct S2

{-| __C declaration:__ @inner@

    __defined at:__ @types\/structs\/anonymous.h 18:5@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance (ty ~ S2_inner) => BG.CompatHasField.HasField "s2_inner" S2 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2 {s2_inner = y1, s2_d = BG.getField @"s2_d" x0}
      , BG.getField @"s2_inner" x0
      )

instance (ty ~ S2_inner) => BG.HasField "s2_inner" (BG.Ptr S2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s2_inner")

instance HasCField.HasCField S2 "s2_inner" where

  type CFieldType S2 "s2_inner" = S2_inner

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @d@

    __defined at:__ @types\/structs\/anonymous.h 20:7@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s2_d" S2 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S2 {s2_d = y1, s2_inner = BG.getField @"s2_inner" x0}
      , BG.getField @"s2_d" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s2_d" (BG.Ptr S2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s2_d")

instance HasCField.HasCField S2 "s2_d" where

  type CFieldType S2 "s2_d" = BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct S3@

    __defined at:__ @types\/structs\/anonymous.h 24:8@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S3 = S3
  { s3_c :: BG.Ptr (BG.Ptr S3_c)
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/anonymous.h 28:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s3_d :: BG.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/anonymous.h 30:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S3 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw S3 where

  readRaw =
    \ptr0 ->
          pure S3
      <*> HasCField.readRaw (BG.Proxy @"s3_c") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s3_d") ptr0

instance Marshal.WriteRaw S3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3 s3_c2 s3_d3 ->
               HasCField.writeRaw (BG.Proxy @"s3_c") ptr0 s3_c2
            >> HasCField.writeRaw (BG.Proxy @"s3_d") ptr0 s3_d3

deriving via Marshal.EquivStorable S3 instance BG.Storable S3

deriving via Struct.IsStructViaStorable S3 instance Struct.IsStruct S3

{-| __C declaration:__ @c@

    __defined at:__ @types\/structs\/anonymous.h 28:7@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance ( ty ~ BG.Ptr (BG.Ptr S3_c)
         ) => BG.CompatHasField.HasField "s3_c" S3 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S3 {s3_c = y1, s3_d = BG.getField @"s3_d" x0}
      , BG.getField @"s3_c" x0
      )

instance ( ty ~ BG.Ptr (BG.Ptr S3_c)
         ) => BG.HasField "s3_c" (BG.Ptr S3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s3_c")

instance HasCField.HasCField S3 "s3_c" where

  type CFieldType S3 "s3_c" = BG.Ptr (BG.Ptr S3_c)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @d@

    __defined at:__ @types\/structs\/anonymous.h 30:7@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s3_d" S3 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S3 {s3_d = y1, s3_c = BG.getField @"s3_c" x0}
      , BG.getField @"s3_d" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s3_d" (BG.Ptr S3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s3_d")

instance HasCField.HasCField S3 "s3_d" where

  type CFieldType S3 "s3_d" = BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct \@S3_c@

    __defined at:__ @types\/structs\/anonymous.h 25:3@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S3_c = S3_c
  { s3_c_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/anonymous.h 26:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s3_c_b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/anonymous.h 27:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S3_c where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S3_c where

  readRaw =
    \ptr0 ->
          pure S3_c
      <*> HasCField.readRaw (BG.Proxy @"s3_c_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s3_c_b") ptr0

instance Marshal.WriteRaw S3_c where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_c s3_c_a2 s3_c_b3 ->
               HasCField.writeRaw (BG.Proxy @"s3_c_a") ptr0 s3_c_a2
            >> HasCField.writeRaw (BG.Proxy @"s3_c_b") ptr0 s3_c_b3

deriving via Marshal.EquivStorable S3_c instance BG.Storable S3_c

deriving via Struct.IsStructViaStorable S3_c instance Struct.IsStruct S3_c

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/anonymous.h 26:9@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s3_c_a" S3_c ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S3_c {s3_c_a = y1, s3_c_b = BG.getField @"s3_c_b" x0}
      , BG.getField @"s3_c_a" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s3_c_a" (BG.Ptr S3_c) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s3_c_a")

instance HasCField.HasCField S3_c "s3_c_a" where

  type CFieldType S3_c "s3_c_a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/anonymous.h 27:9@

    __exported by:__ @types\/structs\/anonymous.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s3_c_b" S3_c ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S3_c {s3_c_b = y1, s3_c_a = BG.getField @"s3_c_a" x0}
      , BG.getField @"s3_c_b" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s3_c_b" (BG.Ptr S3_c) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s3_c_b")

instance HasCField.HasCField S3_c "s3_c_b" where

  type CFieldType S3_c "s3_c_b" = BG.CInt

  offset# = \_ -> \_ -> 4
