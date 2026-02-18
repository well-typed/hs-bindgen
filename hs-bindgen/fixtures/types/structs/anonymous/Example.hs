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

{-| __C declaration:__ @struct \@S1_c@

    __defined at:__ @types\/structs\/anonymous.h 3:3@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S1_c = S1_c
  { s1_c_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/anonymous.h 4:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s1_c_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/anonymous.h 5:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S1_c where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S1_c where

  readRaw =
    \ptr0 ->
          pure S1_c
      <*> HasCField.readRaw (RIP.Proxy @"s1_c_a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s1_c_b") ptr0

instance Marshal.WriteRaw S1_c where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1_c s1_c_a2 s1_c_b3 ->
               HasCField.writeRaw (RIP.Proxy @"s1_c_a") ptr0 s1_c_a2
            >> HasCField.writeRaw (RIP.Proxy @"s1_c_b") ptr0 s1_c_b3

deriving via Marshal.EquivStorable S1_c instance RIP.Storable S1_c

instance HasCField.HasCField S1_c "s1_c_a" where

  type CFieldType S1_c "s1_c_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s1_c_a" (RIP.Ptr S1_c) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s1_c_a")

instance HasCField.HasCField S1_c "s1_c_b" where

  type CFieldType S1_c "s1_c_b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s1_c_b" (RIP.Ptr S1_c) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s1_c_b")

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
  , s1_d :: RIP.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/anonymous.h 8:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S1 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S1 where

  readRaw =
    \ptr0 ->
          pure S1
      <*> HasCField.readRaw (RIP.Proxy @"s1_c") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s1_d") ptr0

instance Marshal.WriteRaw S1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_c2 s1_d3 ->
               HasCField.writeRaw (RIP.Proxy @"s1_c") ptr0 s1_c2
            >> HasCField.writeRaw (RIP.Proxy @"s1_d") ptr0 s1_d3

deriving via Marshal.EquivStorable S1 instance RIP.Storable S1

instance HasCField.HasCField S1 "s1_c" where

  type CFieldType S1 "s1_c" = S1_c

  offset# = \_ -> \_ -> 0

instance (((~) ty) S1_c) => RIP.HasField "s1_c" (RIP.Ptr S1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s1_c")

instance HasCField.HasCField S1 "s1_d" where

  type CFieldType S1 "s1_d" = RIP.CInt

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s1_d" (RIP.Ptr S1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s1_d")

{-| __C declaration:__ @struct \@S2_inner_deep@

    __defined at:__ @types\/structs\/anonymous.h 15:5@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S2_inner_deep = S2_inner_deep
  { s2_inner_deep_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/anonymous.h 16:11@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S2_inner_deep where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2_inner_deep where

  readRaw =
    \ptr0 ->
          pure S2_inner_deep
      <*> HasCField.readRaw (RIP.Proxy @"s2_inner_deep_b") ptr0

instance Marshal.WriteRaw S2_inner_deep where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_inner_deep s2_inner_deep_b2 ->
            HasCField.writeRaw (RIP.Proxy @"s2_inner_deep_b") ptr0 s2_inner_deep_b2

deriving via Marshal.EquivStorable S2_inner_deep instance RIP.Storable S2_inner_deep

instance HasCField.HasCField S2_inner_deep "s2_inner_deep_b" where

  type CFieldType S2_inner_deep "s2_inner_deep_b" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s2_inner_deep_b" (RIP.Ptr S2_inner_deep) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"s2_inner_deep_b")

{-| __C declaration:__ @struct \@S2_inner@

    __defined at:__ @types\/structs\/anonymous.h 13:3@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S2_inner = S2_inner
  { s2_inner_a :: RIP.CInt
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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S2_inner where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2_inner where

  readRaw =
    \ptr0 ->
          pure S2_inner
      <*> HasCField.readRaw (RIP.Proxy @"s2_inner_a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s2_inner_deep") ptr0

instance Marshal.WriteRaw S2_inner where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_inner s2_inner_a2 s2_inner_deep3 ->
               HasCField.writeRaw (RIP.Proxy @"s2_inner_a") ptr0 s2_inner_a2
            >> HasCField.writeRaw (RIP.Proxy @"s2_inner_deep") ptr0 s2_inner_deep3

deriving via Marshal.EquivStorable S2_inner instance RIP.Storable S2_inner

instance HasCField.HasCField S2_inner "s2_inner_a" where

  type CFieldType S2_inner "s2_inner_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s2_inner_a" (RIP.Ptr S2_inner) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"s2_inner_a")

instance HasCField.HasCField S2_inner "s2_inner_deep" where

  type CFieldType S2_inner "s2_inner_deep" =
    S2_inner_deep

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) S2_inner_deep
         ) => RIP.HasField "s2_inner_deep" (RIP.Ptr S2_inner) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"s2_inner_deep")

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
  , s2_d :: RIP.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/anonymous.h 20:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S2 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2 where

  readRaw =
    \ptr0 ->
          pure S2
      <*> HasCField.readRaw (RIP.Proxy @"s2_inner") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s2_d") ptr0

instance Marshal.WriteRaw S2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_inner2 s2_d3 ->
               HasCField.writeRaw (RIP.Proxy @"s2_inner") ptr0 s2_inner2
            >> HasCField.writeRaw (RIP.Proxy @"s2_d") ptr0 s2_d3

deriving via Marshal.EquivStorable S2 instance RIP.Storable S2

instance HasCField.HasCField S2 "s2_inner" where

  type CFieldType S2 "s2_inner" = S2_inner

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) S2_inner
         ) => RIP.HasField "s2_inner" (RIP.Ptr S2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s2_inner")

instance HasCField.HasCField S2 "s2_d" where

  type CFieldType S2 "s2_d" = RIP.CInt

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s2_d" (RIP.Ptr S2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s2_d")

{-| __C declaration:__ @struct S3@

    __defined at:__ @types\/structs\/anonymous.h 24:8@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S3 = S3
  { s3_c :: RIP.Ptr (RIP.Ptr S3_c)
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/anonymous.h 28:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s3_d :: RIP.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/anonymous.h 30:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S3 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw S3 where

  readRaw =
    \ptr0 ->
          pure S3
      <*> HasCField.readRaw (RIP.Proxy @"s3_c") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s3_d") ptr0

instance Marshal.WriteRaw S3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3 s3_c2 s3_d3 ->
               HasCField.writeRaw (RIP.Proxy @"s3_c") ptr0 s3_c2
            >> HasCField.writeRaw (RIP.Proxy @"s3_d") ptr0 s3_d3

deriving via Marshal.EquivStorable S3 instance RIP.Storable S3

instance HasCField.HasCField S3 "s3_c" where

  type CFieldType S3 "s3_c" = RIP.Ptr (RIP.Ptr S3_c)

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.Ptr (RIP.Ptr S3_c))
         ) => RIP.HasField "s3_c" (RIP.Ptr S3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s3_c")

instance HasCField.HasCField S3 "s3_d" where

  type CFieldType S3 "s3_d" = RIP.CInt

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s3_d" (RIP.Ptr S3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s3_d")

{-| __C declaration:__ @struct \@S3_c@

    __defined at:__ @types\/structs\/anonymous.h 25:3@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S3_c = S3_c
  { s3_c_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/anonymous.h 26:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s3_c_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/anonymous.h 27:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S3_c where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S3_c where

  readRaw =
    \ptr0 ->
          pure S3_c
      <*> HasCField.readRaw (RIP.Proxy @"s3_c_a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s3_c_b") ptr0

instance Marshal.WriteRaw S3_c where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_c s3_c_a2 s3_c_b3 ->
               HasCField.writeRaw (RIP.Proxy @"s3_c_a") ptr0 s3_c_a2
            >> HasCField.writeRaw (RIP.Proxy @"s3_c_b") ptr0 s3_c_b3

deriving via Marshal.EquivStorable S3_c instance RIP.Storable S3_c

instance HasCField.HasCField S3_c "s3_c_a" where

  type CFieldType S3_c "s3_c_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s3_c_a" (RIP.Ptr S3_c) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s3_c_a")

instance HasCField.HasCField S3_c "s3_c_b" where

  type CFieldType S3_c "s3_c_b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s3_c_b" (RIP.Ptr S3_c) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s3_c_b")
