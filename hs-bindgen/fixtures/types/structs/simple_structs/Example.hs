{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
  { s1_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 3:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s1_b :: RIP.CChar
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
      <*> HasCField.readRaw (RIP.Proxy @"s1_a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s1_b") ptr0

instance Marshal.WriteRaw S1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_a2 s1_b3 ->
               HasCField.writeRaw (RIP.Proxy @"s1_a") ptr0 s1_a2
            >> HasCField.writeRaw (RIP.Proxy @"s1_b") ptr0 s1_b3

deriving via Marshal.EquivStorable S1 instance RIP.Storable S1

instance HasCField.HasCField S1 "s1_a" where

  type CFieldType S1 "s1_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s1_a" (RIP.Ptr S1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s1_a")

instance HasCField.HasCField S1 "s1_b" where

  type CFieldType S1 "s1_b" = RIP.CChar

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "s1_b" (RIP.Ptr S1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s1_b")

{-| __C declaration:__ @struct S2@

    __defined at:__ @types\/structs\/simple_structs.h 8:16@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S2_t = S2_t
  { s2_t_a :: RIP.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 9:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s2_t_b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 10:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s2_t_c :: RIP.CFloat
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
      <*> HasCField.readRaw (RIP.Proxy @"s2_t_a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s2_t_b") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s2_t_c") ptr0

instance Marshal.WriteRaw S2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_t s2_t_a2 s2_t_b3 s2_t_c4 ->
               HasCField.writeRaw (RIP.Proxy @"s2_t_a") ptr0 s2_t_a2
            >> HasCField.writeRaw (RIP.Proxy @"s2_t_b") ptr0 s2_t_b3
            >> HasCField.writeRaw (RIP.Proxy @"s2_t_c") ptr0 s2_t_c4

deriving via Marshal.EquivStorable S2_t instance RIP.Storable S2_t

instance HasCField.HasCField S2_t "s2_t_a" where

  type CFieldType S2_t "s2_t_a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "s2_t_a" (RIP.Ptr S2_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s2_t_a")

instance HasCField.HasCField S2_t "s2_t_b" where

  type CFieldType S2_t "s2_t_b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s2_t_b" (RIP.Ptr S2_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s2_t_b")

instance HasCField.HasCField S2_t "s2_t_c" where

  type CFieldType S2_t "s2_t_c" = RIP.CFloat

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CFloat
         ) => RIP.HasField "s2_t_c" (RIP.Ptr S2_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s2_t_c")

{-| __C declaration:__ @struct S3_t@

    __defined at:__ @types\/structs\/simple_structs.h 15:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S3_t = S3_t
  { s3_t_a :: RIP.CChar
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
      <*> HasCField.readRaw (RIP.Proxy @"s3_t_a") ptr0

instance Marshal.WriteRaw S3_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_t s3_t_a2 ->
            HasCField.writeRaw (RIP.Proxy @"s3_t_a") ptr0 s3_t_a2

deriving via Marshal.EquivStorable S3_t instance RIP.Storable S3_t

instance HasCField.HasCField S3_t "s3_t_a" where

  type CFieldType S3_t "s3_t_a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "s3_t_a" (RIP.Ptr S3_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s3_t_a")

{-| __C declaration:__ @struct S4@

    __defined at:__ @types\/structs\/simple_structs.h 19:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S4 = S4
  { s4_b :: RIP.CChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 20:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s4_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 21:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s4_c :: RIP.Ptr RIP.CInt
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
      <*> HasCField.readRaw (RIP.Proxy @"s4_b") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s4_a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s4_c") ptr0

instance Marshal.WriteRaw S4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S4 s4_b2 s4_a3 s4_c4 ->
               HasCField.writeRaw (RIP.Proxy @"s4_b") ptr0 s4_b2
            >> HasCField.writeRaw (RIP.Proxy @"s4_a") ptr0 s4_a3
            >> HasCField.writeRaw (RIP.Proxy @"s4_c") ptr0 s4_c4

deriving via Marshal.EquivStorable S4 instance RIP.Storable S4

instance HasCField.HasCField S4 "s4_b" where

  type CFieldType S4 "s4_b" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "s4_b" (RIP.Ptr S4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s4_b")

instance HasCField.HasCField S4 "s4_a" where

  type CFieldType S4 "s4_a" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s4_a" (RIP.Ptr S4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s4_a")

instance HasCField.HasCField S4 "s4_c" where

  type CFieldType S4 "s4_c" = RIP.Ptr RIP.CInt

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr RIP.CInt)
         ) => RIP.HasField "s4_c" (RIP.Ptr S4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s4_c")

{-| __C declaration:__ @struct S5@

    __defined at:__ @types\/structs\/simple_structs.h 26:16@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S5 = S5
  { s5_a :: RIP.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 27:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s5_b :: RIP.CInt
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
      <*> HasCField.readRaw (RIP.Proxy @"s5_a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s5_b") ptr0

instance Marshal.WriteRaw S5 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S5 s5_a2 s5_b3 ->
               HasCField.writeRaw (RIP.Proxy @"s5_a") ptr0 s5_a2
            >> HasCField.writeRaw (RIP.Proxy @"s5_b") ptr0 s5_b3

deriving via Marshal.EquivStorable S5 instance RIP.Storable S5

instance HasCField.HasCField S5 "s5_a" where

  type CFieldType S5 "s5_a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "s5_a" (RIP.Ptr S5) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s5_a")

instance HasCField.HasCField S5 "s5_b" where

  type CFieldType S5 "s5_b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s5_b" (RIP.Ptr S5) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s5_b")

{-| __C declaration:__ @struct S6@

    __defined at:__ @types\/structs\/simple_structs.h 31:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S6 = S6
  { s6_a :: RIP.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 31:18@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s6_b :: RIP.CInt
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
      <*> HasCField.readRaw (RIP.Proxy @"s6_a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s6_b") ptr0

instance Marshal.WriteRaw S6 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S6 s6_a2 s6_b3 ->
               HasCField.writeRaw (RIP.Proxy @"s6_a") ptr0 s6_a2
            >> HasCField.writeRaw (RIP.Proxy @"s6_b") ptr0 s6_b3

deriving via Marshal.EquivStorable S6 instance RIP.Storable S6

instance HasCField.HasCField S6 "s6_a" where

  type CFieldType S6 "s6_a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "s6_a" (RIP.Ptr S6) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s6_a")

instance HasCField.HasCField S6 "s6_b" where

  type CFieldType S6 "s6_b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s6_b" (RIP.Ptr S6) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s6_b")

{-| __C declaration:__ @struct \@S7a_Aux@

    __defined at:__ @types\/structs\/simple_structs.h 34:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S7a_Aux = S7a_Aux
  { s7a_Aux_a :: RIP.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 34:23@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s7a_Aux_b :: RIP.CInt
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
      <*> HasCField.readRaw (RIP.Proxy @"s7a_Aux_a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s7a_Aux_b") ptr0

instance Marshal.WriteRaw S7a_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7a_Aux s7a_Aux_a2 s7a_Aux_b3 ->
               HasCField.writeRaw (RIP.Proxy @"s7a_Aux_a") ptr0 s7a_Aux_a2
            >> HasCField.writeRaw (RIP.Proxy @"s7a_Aux_b") ptr0 s7a_Aux_b3

deriving via Marshal.EquivStorable S7a_Aux instance RIP.Storable S7a_Aux

instance HasCField.HasCField S7a_Aux "s7a_Aux_a" where

  type CFieldType S7a_Aux "s7a_Aux_a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "s7a_Aux_a" (RIP.Ptr S7a_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s7a_Aux_a")

instance HasCField.HasCField S7a_Aux "s7a_Aux_b" where

  type CFieldType S7a_Aux "s7a_Aux_b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s7a_Aux_b" (RIP.Ptr S7a_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s7a_Aux_b")

{-| __C declaration:__ @S7a@

    __defined at:__ @types\/structs\/simple_structs.h 34:36@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
newtype S7a = S7a
  { unwrapS7a :: RIP.Ptr S7a_Aux
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
         ) => RIP.HasField "unwrapS7a" (RIP.Ptr S7a) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapS7a")

instance HasCField.HasCField S7a "unwrapS7a" where

  type CFieldType S7a "unwrapS7a" = RIP.Ptr S7a_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@S7b_Aux@

    __defined at:__ @types\/structs\/simple_structs.h 35:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S7b_Aux = S7b_Aux
  { s7b_Aux_a :: RIP.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 35:23@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , s7b_Aux_b :: RIP.CInt
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
      <*> HasCField.readRaw (RIP.Proxy @"s7b_Aux_a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"s7b_Aux_b") ptr0

instance Marshal.WriteRaw S7b_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7b_Aux s7b_Aux_a2 s7b_Aux_b3 ->
               HasCField.writeRaw (RIP.Proxy @"s7b_Aux_a") ptr0 s7b_Aux_a2
            >> HasCField.writeRaw (RIP.Proxy @"s7b_Aux_b") ptr0 s7b_Aux_b3

deriving via Marshal.EquivStorable S7b_Aux instance RIP.Storable S7b_Aux

instance HasCField.HasCField S7b_Aux "s7b_Aux_a" where

  type CFieldType S7b_Aux "s7b_Aux_a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "s7b_Aux_a" (RIP.Ptr S7b_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s7b_Aux_a")

instance HasCField.HasCField S7b_Aux "s7b_Aux_b" where

  type CFieldType S7b_Aux "s7b_Aux_b" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s7b_Aux_b" (RIP.Ptr S7b_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s7b_Aux_b")

{-| __C declaration:__ @S7b@

    __defined at:__ @types\/structs\/simple_structs.h 35:38@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
newtype S7b = S7b
  { unwrapS7b :: RIP.Ptr (RIP.Ptr (RIP.Ptr S7b_Aux))
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
         ) => RIP.HasField "unwrapS7b" (RIP.Ptr S7b) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapS7b")

instance HasCField.HasCField S7b "unwrapS7b" where

  type CFieldType S7b "unwrapS7b" =
    RIP.Ptr (RIP.Ptr (RIP.Ptr S7b_Aux))

  offset# = \_ -> \_ -> 0
