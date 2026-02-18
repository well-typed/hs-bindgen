{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

{-| __C declaration:__ @struct S1@

    __defined at:__ @types\/structs\/simple_structs.h 2:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S1 = S1
  { a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 3:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: RIP.CChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 4:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S1 where

  readRaw =
    \ptr0 ->
          pure S1
      <*> HasCField.readRaw (RIP.Proxy @"a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"b") ptr0

instance Marshal.WriteRaw S1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 a2 b3 ->
               HasCField.writeRaw (RIP.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (RIP.Proxy @"b") ptr0 b3

deriving via Marshal.EquivStorable S1 instance RIP.Storable S1

instance HasCField.HasCField S1 "a" where

  type CFieldType S1 "a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (((~) ty) RIP.CInt) => RIP.HasField "a" (RIP.Ptr S1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a")

instance HasCField.HasCField S1 "b" where

  type CFieldType S1 "b" = RIP.CChar

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "b" (RIP.Ptr S1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"b")

{-| __C declaration:__ @struct S2@

    __defined at:__ @types\/structs\/simple_structs.h 8:16@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S2_t = S2_t
  { a :: RIP.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 9:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 10:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , c :: RIP.CFloat
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/simple_structs.h 11:11@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S2_t where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2_t where

  readRaw =
    \ptr0 ->
          pure S2_t
      <*> HasCField.readRaw (RIP.Proxy @"a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"b") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"c") ptr0

instance Marshal.WriteRaw S2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_t a2 b3 c4 ->
               HasCField.writeRaw (RIP.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (RIP.Proxy @"b") ptr0 b3
            >> HasCField.writeRaw (RIP.Proxy @"c") ptr0 c4

deriving via Marshal.EquivStorable S2_t instance RIP.Storable S2_t

instance HasCField.HasCField S2_t "a" where

  type CFieldType S2_t "a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "a" (RIP.Ptr S2_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a")

instance HasCField.HasCField S2_t "b" where

  type CFieldType S2_t "b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "b" (RIP.Ptr S2_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"b")

instance HasCField.HasCField S2_t "c" where

  type CFieldType S2_t "c" = RIP.CFloat

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CFloat
         ) => RIP.HasField "c" (RIP.Ptr S2_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"c")

{-| __C declaration:__ @struct S3_t@

    __defined at:__ @types\/structs\/simple_structs.h 15:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S3_t = S3_t
  { a :: RIP.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 16:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S3_t where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw S3_t where

  readRaw =
    \ptr0 ->
          pure S3_t
      <*> HasCField.readRaw (RIP.Proxy @"a") ptr0

instance Marshal.WriteRaw S3_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_t a2 ->
            HasCField.writeRaw (RIP.Proxy @"a") ptr0 a2

deriving via Marshal.EquivStorable S3_t instance RIP.Storable S3_t

instance HasCField.HasCField S3_t "a" where

  type CFieldType S3_t "a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "a" (RIP.Ptr S3_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a")

{-| __C declaration:__ @struct S4@

    __defined at:__ @types\/structs\/simple_structs.h 19:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S4 = S4
  { b :: RIP.CChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 20:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 21:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , c :: RIP.Ptr RIP.CInt
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/simple_structs.h 22:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S4 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw S4 where

  readRaw =
    \ptr0 ->
          pure S4
      <*> HasCField.readRaw (RIP.Proxy @"b") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"c") ptr0

instance Marshal.WriteRaw S4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S4 b2 a3 c4 ->
               HasCField.writeRaw (RIP.Proxy @"b") ptr0 b2
            >> HasCField.writeRaw (RIP.Proxy @"a") ptr0 a3
            >> HasCField.writeRaw (RIP.Proxy @"c") ptr0 c4

deriving via Marshal.EquivStorable S4 instance RIP.Storable S4

instance HasCField.HasCField S4 "b" where

  type CFieldType S4 "b" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "b" (RIP.Ptr S4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"b")

instance HasCField.HasCField S4 "a" where

  type CFieldType S4 "a" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance (((~) ty) RIP.CInt) => RIP.HasField "a" (RIP.Ptr S4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a")

instance HasCField.HasCField S4 "c" where

  type CFieldType S4 "c" = RIP.Ptr RIP.CInt

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr RIP.CInt)
         ) => RIP.HasField "c" (RIP.Ptr S4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"c")

{-| __C declaration:__ @struct S5@

    __defined at:__ @types\/structs\/simple_structs.h 26:16@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S5 = S5
  { a :: RIP.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 27:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 28:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S5 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S5 where

  readRaw =
    \ptr0 ->
          pure S5
      <*> HasCField.readRaw (RIP.Proxy @"a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"b") ptr0

instance Marshal.WriteRaw S5 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S5 a2 b3 ->
               HasCField.writeRaw (RIP.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (RIP.Proxy @"b") ptr0 b3

deriving via Marshal.EquivStorable S5 instance RIP.Storable S5

instance HasCField.HasCField S5 "a" where

  type CFieldType S5 "a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "a" (RIP.Ptr S5) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a")

instance HasCField.HasCField S5 "b" where

  type CFieldType S5 "b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance (((~) ty) RIP.CInt) => RIP.HasField "b" (RIP.Ptr S5) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"b")

{-| __C declaration:__ @struct S6@

    __defined at:__ @types\/structs\/simple_structs.h 31:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S6 = S6
  { a :: RIP.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 31:18@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 31:25@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S6 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S6 where

  readRaw =
    \ptr0 ->
          pure S6
      <*> HasCField.readRaw (RIP.Proxy @"a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"b") ptr0

instance Marshal.WriteRaw S6 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S6 a2 b3 ->
               HasCField.writeRaw (RIP.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (RIP.Proxy @"b") ptr0 b3

deriving via Marshal.EquivStorable S6 instance RIP.Storable S6

instance HasCField.HasCField S6 "a" where

  type CFieldType S6 "a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "a" (RIP.Ptr S6) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a")

instance HasCField.HasCField S6 "b" where

  type CFieldType S6 "b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance (((~) ty) RIP.CInt) => RIP.HasField "b" (RIP.Ptr S6) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"b")

{-| __C declaration:__ @struct \@S7a_Aux@

    __defined at:__ @types\/structs\/simple_structs.h 34:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S7a_Aux = S7a_Aux
  { a :: RIP.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 34:23@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 34:30@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S7a_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S7a_Aux where

  readRaw =
    \ptr0 ->
          pure S7a_Aux
      <*> HasCField.readRaw (RIP.Proxy @"a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"b") ptr0

instance Marshal.WriteRaw S7a_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7a_Aux a2 b3 ->
               HasCField.writeRaw (RIP.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (RIP.Proxy @"b") ptr0 b3

deriving via Marshal.EquivStorable S7a_Aux instance RIP.Storable S7a_Aux

instance HasCField.HasCField S7a_Aux "a" where

  type CFieldType S7a_Aux "a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "a" (RIP.Ptr S7a_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a")

instance HasCField.HasCField S7a_Aux "b" where

  type CFieldType S7a_Aux "b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "b" (RIP.Ptr S7a_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"b")

{-| __C declaration:__ @S7a@

    __defined at:__ @types\/structs\/simple_structs.h 34:36@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
newtype S7a = S7a
  { unwrap :: RIP.Ptr S7a_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr S7a_Aux)
         ) => RIP.HasField "unwrap" (RIP.Ptr S7a) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField S7a "unwrap" where

  type CFieldType S7a "unwrap" = RIP.Ptr S7a_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@S7b_Aux@

    __defined at:__ @types\/structs\/simple_structs.h 35:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S7b_Aux = S7b_Aux
  { a :: RIP.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 35:23@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 35:30@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S7b_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S7b_Aux where

  readRaw =
    \ptr0 ->
          pure S7b_Aux
      <*> HasCField.readRaw (RIP.Proxy @"a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"b") ptr0

instance Marshal.WriteRaw S7b_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7b_Aux a2 b3 ->
               HasCField.writeRaw (RIP.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (RIP.Proxy @"b") ptr0 b3

deriving via Marshal.EquivStorable S7b_Aux instance RIP.Storable S7b_Aux

instance HasCField.HasCField S7b_Aux "a" where

  type CFieldType S7b_Aux "a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "a" (RIP.Ptr S7b_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a")

instance HasCField.HasCField S7b_Aux "b" where

  type CFieldType S7b_Aux "b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "b" (RIP.Ptr S7b_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"b")

{-| __C declaration:__ @S7b@

    __defined at:__ @types\/structs\/simple_structs.h 35:38@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
newtype S7b = S7b
  { unwrap :: RIP.Ptr (RIP.Ptr (RIP.Ptr S7b_Aux))
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr (RIP.Ptr (RIP.Ptr S7b_Aux)))
         ) => RIP.HasField "unwrap" (RIP.Ptr S7b) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField S7b "unwrap" where

  type CFieldType S7b "unwrap" =
    RIP.Ptr (RIP.Ptr (RIP.Ptr S7b_Aux))

  offset# = \_ -> \_ -> 0
