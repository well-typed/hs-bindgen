{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
import Prelude ((<*>), (>>), Eq, Int, Ord, Show, pure)

{-| __C declaration:__ @struct S1@

    __defined at:__ @types\/structs\/simple_structs.h 2:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S1 = S1
  { a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 3:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: FC.CChar
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
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"b") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 a2 b3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a") ptr0 a2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"b") ptr0 b3

deriving via HsBindgen.Runtime.Marshal.EquivStorable S1 instance F.Storable S1

instance HsBindgen.Runtime.HasCField.HasCField S1 "a" where

  type CFieldType S1 "a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "a" (Ptr.Ptr S1) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a")

instance HsBindgen.Runtime.HasCField.HasCField S1 "b" where

  type CFieldType S1 "b" = FC.CChar

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "b" (Ptr.Ptr S1) (Ptr.Ptr FC.CChar) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"b")

{-| __C declaration:__ @struct S2@

    __defined at:__ @types\/structs\/simple_structs.h 8:16@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S2_t = S2_t
  { a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 9:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 10:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , c :: FC.CFloat
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
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"b") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"c") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_t a2 b3 c4 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a") ptr0 a2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"b") ptr0 b3
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"c") ptr0 c4

deriving via HsBindgen.Runtime.Marshal.EquivStorable S2_t instance F.Storable S2_t

instance HsBindgen.Runtime.HasCField.HasCField S2_t "a" where

  type CFieldType S2_t "a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "a" (Ptr.Ptr S2_t) (Ptr.Ptr FC.CChar) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a")

instance HsBindgen.Runtime.HasCField.HasCField S2_t "b" where

  type CFieldType S2_t "b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "b" (Ptr.Ptr S2_t) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"b")

instance HsBindgen.Runtime.HasCField.HasCField S2_t "c" where

  type CFieldType S2_t "c" = FC.CFloat

  offset# = \_ -> \_ -> 8

instance GHC.Records.HasField "c" (Ptr.Ptr S2_t) (Ptr.Ptr FC.CFloat) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"c")

{-| __C declaration:__ @struct S3_t@

    __defined at:__ @types\/structs\/simple_structs.h 15:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S3_t = S3_t
  { a :: FC.CChar
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
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S3_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_t a2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a") ptr0 a2

deriving via HsBindgen.Runtime.Marshal.EquivStorable S3_t instance F.Storable S3_t

instance HsBindgen.Runtime.HasCField.HasCField S3_t "a" where

  type CFieldType S3_t "a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "a" (Ptr.Ptr S3_t) (Ptr.Ptr FC.CChar) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a")

{-| __C declaration:__ @struct S4@

    __defined at:__ @types\/structs\/simple_structs.h 19:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S4 = S4
  { b :: FC.CChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/simple_structs.h 20:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 21:9@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , c :: Ptr.Ptr FC.CInt
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
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"b") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"c") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S4 b2 a3 c4 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"b") ptr0 b2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a") ptr0 a3
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"c") ptr0 c4

deriving via HsBindgen.Runtime.Marshal.EquivStorable S4 instance F.Storable S4

instance HsBindgen.Runtime.HasCField.HasCField S4 "b" where

  type CFieldType S4 "b" = FC.CChar

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "b" (Ptr.Ptr S4) (Ptr.Ptr FC.CChar) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"b")

instance HsBindgen.Runtime.HasCField.HasCField S4 "a" where

  type CFieldType S4 "a" = FC.CInt

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "a" (Ptr.Ptr S4) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a")

instance HsBindgen.Runtime.HasCField.HasCField S4 "c" where

  type CFieldType S4 "c" = Ptr.Ptr FC.CInt

  offset# = \_ -> \_ -> 8

instance GHC.Records.HasField "c" (Ptr.Ptr S4) (Ptr.Ptr (Ptr.Ptr FC.CInt)) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"c")

{-| __C declaration:__ @struct S5@

    __defined at:__ @types\/structs\/simple_structs.h 26:16@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S5 = S5
  { a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 27:10@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: FC.CInt
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
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"b") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S5 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S5 a2 b3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a") ptr0 a2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"b") ptr0 b3

deriving via HsBindgen.Runtime.Marshal.EquivStorable S5 instance F.Storable S5

instance HsBindgen.Runtime.HasCField.HasCField S5 "a" where

  type CFieldType S5 "a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "a" (Ptr.Ptr S5) (Ptr.Ptr FC.CChar) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a")

instance HsBindgen.Runtime.HasCField.HasCField S5 "b" where

  type CFieldType S5 "b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "b" (Ptr.Ptr S5) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"b")

{-| __C declaration:__ @struct S6@

    __defined at:__ @types\/structs\/simple_structs.h 31:8@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S6 = S6
  { a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 31:18@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: FC.CInt
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
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"b") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S6 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S6 a2 b3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a") ptr0 a2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"b") ptr0 b3

deriving via HsBindgen.Runtime.Marshal.EquivStorable S6 instance F.Storable S6

instance HsBindgen.Runtime.HasCField.HasCField S6 "a" where

  type CFieldType S6 "a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "a" (Ptr.Ptr S6) (Ptr.Ptr FC.CChar) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a")

instance HsBindgen.Runtime.HasCField.HasCField S6 "b" where

  type CFieldType S6 "b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "b" (Ptr.Ptr S6) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"b")

{-| __C declaration:__ @struct \@S7a_Aux@

    __defined at:__ @types\/structs\/simple_structs.h 34:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S7a_Aux = S7a_Aux
  { a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 34:23@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: FC.CInt
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
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"b") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S7a_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7a_Aux a2 b3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a") ptr0 a2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"b") ptr0 b3

deriving via HsBindgen.Runtime.Marshal.EquivStorable S7a_Aux instance F.Storable S7a_Aux

instance HsBindgen.Runtime.HasCField.HasCField S7a_Aux "a" where

  type CFieldType S7a_Aux "a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "a" (Ptr.Ptr S7a_Aux) (Ptr.Ptr FC.CChar) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a")

instance HsBindgen.Runtime.HasCField.HasCField S7a_Aux "b" where

  type CFieldType S7a_Aux "b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "b" (Ptr.Ptr S7a_Aux) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"b")

{-| __C declaration:__ @S7a@

    __defined at:__ @types\/structs\/simple_structs.h 34:36@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
newtype S7a = S7a
  { unwrap :: Ptr.Ptr S7a_Aux
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance GHC.Records.HasField "unwrap" (Ptr.Ptr S7a) (Ptr.Ptr (Ptr.Ptr S7a_Aux)) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrap")

instance HsBindgen.Runtime.HasCField.HasCField S7a "unwrap" where

  type CFieldType S7a "unwrap" = Ptr.Ptr S7a_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@S7b_Aux@

    __defined at:__ @types\/structs\/simple_structs.h 35:9@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
data S7b_Aux = S7b_Aux
  { a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/simple_structs.h 35:23@

         __exported by:__ @types\/structs\/simple_structs.h@
    -}
  , b :: FC.CInt
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
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"b") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S7b_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7b_Aux a2 b3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a") ptr0 a2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"b") ptr0 b3

deriving via HsBindgen.Runtime.Marshal.EquivStorable S7b_Aux instance F.Storable S7b_Aux

instance HsBindgen.Runtime.HasCField.HasCField S7b_Aux "a" where

  type CFieldType S7b_Aux "a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "a" (Ptr.Ptr S7b_Aux) (Ptr.Ptr FC.CChar) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a")

instance HsBindgen.Runtime.HasCField.HasCField S7b_Aux "b" where

  type CFieldType S7b_Aux "b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "b" (Ptr.Ptr S7b_Aux) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"b")

{-| __C declaration:__ @S7b@

    __defined at:__ @types\/structs\/simple_structs.h 35:38@

    __exported by:__ @types\/structs\/simple_structs.h@
-}
newtype S7b = S7b
  { unwrap :: Ptr.Ptr (Ptr.Ptr (Ptr.Ptr S7b_Aux))
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance GHC.Records.HasField "unwrap" (Ptr.Ptr S7b) (Ptr.Ptr (Ptr.Ptr (Ptr.Ptr (Ptr.Ptr S7b_Aux)))) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrap")

instance HsBindgen.Runtime.HasCField.HasCField S7b "unwrap" where

  type CFieldType S7b "unwrap" =
    Ptr.Ptr (Ptr.Ptr (Ptr.Ptr S7b_Aux))

  offset# = \_ -> \_ -> 0
