{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.S(..)
    , Example.More_aligned_int(..)
    , Example.S2(..)
    , Example.My_unpacked_struct(..)
    , Example.My_packed_struct(..)
    , Example.Wait_status_ptr_t(..)
    , Example.Wait
    , Example.T1(..)
    , Example.Short_a(..)
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @struct S@

    __defined at:__ @attributes\/type_attributes.h 8:8@

    __exported by:__ @attributes\/type_attributes.h@
-}
data S = S
  { s_f :: CA.ConstantArray 3 BG.CShort
    {- ^ __C declaration:__ @f@

         __defined at:__ @attributes\/type_attributes.h 8:18@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_f") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_f2 ->
            HasCField.writeRaw (BG.Proxy @"s_f") ptr0 s_f2

deriving via Marshal.EquivStorable S instance BG.Storable S

deriving via Struct.IsStructViaStorable S instance Struct.IsStruct S

{-| __C declaration:__ @f@

    __defined at:__ @attributes\/type_attributes.h 8:18@

    __exported by:__ @attributes\/type_attributes.h@
-}
instance ( ty ~ CA.ConstantArray 3 BG.CShort
         ) => BG.CompatHasField.HasField "s_f" S ty where

  hasField =
    \x0 -> (\y1 -> S {s_f = y1}, BG.getField @"s_f" x0)

instance ( ty ~ CA.ConstantArray 3 BG.CShort
         ) => BG.HasField "s_f" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_f")

instance HasCField.HasCField S "s_f" where

  type CFieldType S "s_f" =
    CA.ConstantArray 3 BG.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @more_aligned_int@

    __defined at:__ @attributes\/type_attributes.h 9:13@

    __exported by:__ @attributes\/type_attributes.h@
-}
newtype More_aligned_int = More_aligned_int
  { unwrapMore_aligned_int :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapMore_aligned_int" More_aligned_int ty where

  hasField =
    \x0 ->
      ( \y1 ->
          More_aligned_int {unwrapMore_aligned_int = y1}
      , BG.getField @"unwrapMore_aligned_int" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapMore_aligned_int" (BG.Ptr More_aligned_int) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMore_aligned_int")

instance HasCField.HasCField More_aligned_int "unwrapMore_aligned_int" where

  type CFieldType More_aligned_int "unwrapMore_aligned_int" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S2@

    __defined at:__ @attributes\/type_attributes.h 11:8@

    __exported by:__ @attributes\/type_attributes.h@
-}
data S2 = S2
  { s2_f :: CA.ConstantArray 3 BG.CShort
    {- ^ __C declaration:__ @f@

         __defined at:__ @attributes\/type_attributes.h 11:19@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S2 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (16 :: Int)

instance Marshal.ReadRaw S2 where

  readRaw =
    \ptr0 ->
          pure S2
      <*> HasCField.readRaw (BG.Proxy @"s2_f") ptr0

instance Marshal.WriteRaw S2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_f2 ->
            HasCField.writeRaw (BG.Proxy @"s2_f") ptr0 s2_f2

deriving via Marshal.EquivStorable S2 instance BG.Storable S2

deriving via Struct.IsStructViaStorable S2 instance Struct.IsStruct S2

{-| __C declaration:__ @f@

    __defined at:__ @attributes\/type_attributes.h 11:19@

    __exported by:__ @attributes\/type_attributes.h@
-}
instance ( ty ~ CA.ConstantArray 3 BG.CShort
         ) => BG.CompatHasField.HasField "s2_f" S2 ty where

  hasField =
    \x0 ->
      (\y1 -> S2 {s2_f = y1}, BG.getField @"s2_f" x0)

instance ( ty ~ CA.ConstantArray 3 BG.CShort
         ) => BG.HasField "s2_f" (BG.Ptr S2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s2_f")

instance HasCField.HasCField S2 "s2_f" where

  type CFieldType S2 "s2_f" =
    CA.ConstantArray 3 BG.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct my_unpacked_struct@

    __defined at:__ @attributes\/type_attributes.h 13:8@

    __exported by:__ @attributes\/type_attributes.h@
-}
data My_unpacked_struct = My_unpacked_struct
  { my_unpacked_struct_c :: BG.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/type_attributes.h 15:8@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  , my_unpacked_struct_i :: BG.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/type_attributes.h 16:7@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize My_unpacked_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw My_unpacked_struct where

  readRaw =
    \ptr0 ->
          pure My_unpacked_struct
      <*> HasCField.readRaw (BG.Proxy @"my_unpacked_struct_c") ptr0
      <*> HasCField.readRaw (BG.Proxy @"my_unpacked_struct_i") ptr0

instance Marshal.WriteRaw My_unpacked_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          My_unpacked_struct my_unpacked_struct_c2 my_unpacked_struct_i3 ->
               HasCField.writeRaw (BG.Proxy @"my_unpacked_struct_c") ptr0 my_unpacked_struct_c2
            >> HasCField.writeRaw (BG.Proxy @"my_unpacked_struct_i") ptr0 my_unpacked_struct_i3

deriving via Marshal.EquivStorable My_unpacked_struct instance BG.Storable My_unpacked_struct

deriving via Struct.IsStructViaStorable My_unpacked_struct instance Struct.IsStruct My_unpacked_struct

{-| __C declaration:__ @c@

    __defined at:__ @attributes\/type_attributes.h 15:8@

    __exported by:__ @attributes\/type_attributes.h@
-}
instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "my_unpacked_struct_c" My_unpacked_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          My_unpacked_struct { my_unpacked_struct_c = y1
                             , my_unpacked_struct_i = BG.getField @"my_unpacked_struct_i" x0
                             }
      , BG.getField @"my_unpacked_struct_c" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "my_unpacked_struct_c" (BG.Ptr My_unpacked_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"my_unpacked_struct_c")

instance HasCField.HasCField My_unpacked_struct "my_unpacked_struct_c" where

  type CFieldType My_unpacked_struct "my_unpacked_struct_c" =
    BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @i@

    __defined at:__ @attributes\/type_attributes.h 16:7@

    __exported by:__ @attributes\/type_attributes.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "my_unpacked_struct_i" My_unpacked_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          My_unpacked_struct { my_unpacked_struct_i = y1
                             , my_unpacked_struct_c = BG.getField @"my_unpacked_struct_c" x0
                             }
      , BG.getField @"my_unpacked_struct_i" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "my_unpacked_struct_i" (BG.Ptr My_unpacked_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"my_unpacked_struct_i")

instance HasCField.HasCField My_unpacked_struct "my_unpacked_struct_i" where

  type CFieldType My_unpacked_struct "my_unpacked_struct_i" =
    BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct my_packed_struct@

    __defined at:__ @attributes\/type_attributes.h 19:37@

    __exported by:__ @attributes\/type_attributes.h@
-}
data My_packed_struct = My_packed_struct
  { my_packed_struct_c :: BG.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/type_attributes.h 21:9@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  , my_packed_struct_i :: BG.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/type_attributes.h 22:9@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  , my_packed_struct_s :: My_unpacked_struct
    {- ^ __C declaration:__ @s@

         __defined at:__ @attributes\/type_attributes.h 23:30@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize My_packed_struct where

  staticSizeOf = \_ -> (13 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw My_packed_struct where

  readRaw =
    \ptr0 ->
          pure My_packed_struct
      <*> HasCField.readRaw (BG.Proxy @"my_packed_struct_c") ptr0
      <*> HasCField.readRaw (BG.Proxy @"my_packed_struct_i") ptr0
      <*> HasCField.readRaw (BG.Proxy @"my_packed_struct_s") ptr0

instance Marshal.WriteRaw My_packed_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          My_packed_struct my_packed_struct_c2 my_packed_struct_i3 my_packed_struct_s4 ->
               HasCField.writeRaw (BG.Proxy @"my_packed_struct_c") ptr0 my_packed_struct_c2
            >> HasCField.writeRaw (BG.Proxy @"my_packed_struct_i") ptr0 my_packed_struct_i3
            >> HasCField.writeRaw (BG.Proxy @"my_packed_struct_s") ptr0 my_packed_struct_s4

deriving via Marshal.EquivStorable My_packed_struct instance BG.Storable My_packed_struct

deriving via Struct.IsStructViaStorable My_packed_struct instance Struct.IsStruct My_packed_struct

{-| __C declaration:__ @c@

    __defined at:__ @attributes\/type_attributes.h 21:9@

    __exported by:__ @attributes\/type_attributes.h@
-}
instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "my_packed_struct_c" My_packed_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          My_packed_struct { my_packed_struct_c = y1
                           , my_packed_struct_i = BG.getField @"my_packed_struct_i" x0
                           , my_packed_struct_s = BG.getField @"my_packed_struct_s" x0
                           }
      , BG.getField @"my_packed_struct_c" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "my_packed_struct_c" (BG.Ptr My_packed_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"my_packed_struct_c")

instance HasCField.HasCField My_packed_struct "my_packed_struct_c" where

  type CFieldType My_packed_struct "my_packed_struct_c" =
    BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @i@

    __defined at:__ @attributes\/type_attributes.h 22:9@

    __exported by:__ @attributes\/type_attributes.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "my_packed_struct_i" My_packed_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          My_packed_struct { my_packed_struct_i = y1
                           , my_packed_struct_c = BG.getField @"my_packed_struct_c" x0
                           , my_packed_struct_s = BG.getField @"my_packed_struct_s" x0
                           }
      , BG.getField @"my_packed_struct_i" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "my_packed_struct_i" (BG.Ptr My_packed_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"my_packed_struct_i")

instance HasCField.HasCField My_packed_struct "my_packed_struct_i" where

  type CFieldType My_packed_struct "my_packed_struct_i" =
    BG.CInt

  offset# = \_ -> \_ -> 1

{-| __C declaration:__ @s@

    __defined at:__ @attributes\/type_attributes.h 23:30@

    __exported by:__ @attributes\/type_attributes.h@
-}
instance ( ty ~ My_unpacked_struct
         ) => BG.CompatHasField.HasField "my_packed_struct_s" My_packed_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          My_packed_struct { my_packed_struct_s = y1
                           , my_packed_struct_c = BG.getField @"my_packed_struct_c" x0
                           , my_packed_struct_i = BG.getField @"my_packed_struct_i" x0
                           }
      , BG.getField @"my_packed_struct_s" x0
      )

instance ( ty ~ My_unpacked_struct
         ) => BG.HasField "my_packed_struct_s" (BG.Ptr My_packed_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"my_packed_struct_s")

instance HasCField.HasCField My_packed_struct "my_packed_struct_s" where

  type CFieldType My_packed_struct "my_packed_struct_s" =
    My_unpacked_struct

  offset# = \_ -> \_ -> 5

{-| __C declaration:__ @union wait_status_ptr_t@

    __defined at:__ @attributes\/type_attributes.h 26:9@

    __exported by:__ @attributes\/type_attributes.h@
-}
newtype Wait_status_ptr_t = Wait_status_ptr_t
  { unwrapWait_status_ptr_t :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 8 8 instance Marshal.StaticSize Wait_status_ptr_t

deriving via BG.SizedByteArray 8 8 instance Marshal.ReadRaw Wait_status_ptr_t

deriving via BG.SizedByteArray 8 8 instance Marshal.WriteRaw Wait_status_ptr_t

deriving via Marshal.EquivStorable Wait_status_ptr_t instance BG.Storable Wait_status_ptr_t

deriving via BG.SizedByteArray 8 8 instance Union.IsUnion Wait_status_ptr_t

{-| __C declaration:__ @__ip@

    __defined at:__ @attributes\/type_attributes.h 28:8@

    __exported by:__ @attributes\/type_attributes.h@
-}
instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "wait_status_ptr_t___ip" Wait_status_ptr_t ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @__ip@

    __defined at:__ @attributes\/type_attributes.h 28:8@

    __exported by:__ @attributes\/type_attributes.h@
-}
instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.CompatHasField.HasField "wait_status_ptr_t___ip" Wait_status_ptr_t ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"wait_status_ptr_t___ip" x0)

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "wait_status_ptr_t___ip" (BG.Ptr Wait_status_ptr_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"wait_status_ptr_t___ip")

instance HasCField.HasCField Wait_status_ptr_t "wait_status_ptr_t___ip" where

  type CFieldType Wait_status_ptr_t "wait_status_ptr_t___ip" =
    BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @__up@

    __defined at:__ @attributes\/type_attributes.h 29:15@

    __exported by:__ @attributes\/type_attributes.h@
-}
instance ( ty ~ BG.Ptr Wait
         ) => BG.HasField "wait_status_ptr_t___up" Wait_status_ptr_t ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @__up@

    __defined at:__ @attributes\/type_attributes.h 29:15@

    __exported by:__ @attributes\/type_attributes.h@
-}
instance ( ty ~ BG.Ptr Wait
         ) => BG.CompatHasField.HasField "wait_status_ptr_t___up" Wait_status_ptr_t ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"wait_status_ptr_t___up" x0)

instance ( ty ~ BG.Ptr Wait
         ) => BG.HasField "wait_status_ptr_t___up" (BG.Ptr Wait_status_ptr_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"wait_status_ptr_t___up")

instance HasCField.HasCField Wait_status_ptr_t "wait_status_ptr_t___up" where

  type CFieldType Wait_status_ptr_t "wait_status_ptr_t___up" =
    BG.Ptr Wait

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union wait@

    __defined at:__ @attributes\/type_attributes.h 29:9@

    __exported by:__ @attributes\/type_attributes.h@
-}
data Wait

{-| __C declaration:__ @T1@

    __defined at:__ @attributes\/type_attributes.h 32:13@

    __exported by:__ @attributes\/type_attributes.h@
-}
newtype T1 = T1
  { unwrapT1 :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapT1" T1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T1 {unwrapT1 = y1}, BG.getField @"unwrapT1" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapT1" (BG.Ptr T1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT1")

instance HasCField.HasCField T1 "unwrapT1" where

  type CFieldType T1 "unwrapT1" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @short_a@

    __defined at:__ @attributes\/type_attributes.h 34:46@

    __exported by:__ @attributes\/type_attributes.h@
-}
newtype Short_a = Short_a
  { unwrapShort_a :: BG.CShort
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CShort
         ) => BG.CompatHasField.HasField "unwrapShort_a" Short_a ty where

  hasField =
    \x0 ->
      (\y1 ->
         Short_a {unwrapShort_a = y1}, BG.getField @"unwrapShort_a" x0)

instance ( ty ~ BG.CShort
         ) => BG.HasField "unwrapShort_a" (BG.Ptr Short_a) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapShort_a")

instance HasCField.HasCField Short_a "unwrapShort_a" where

  type CFieldType Short_a "unwrapShort_a" = BG.CShort

  offset# = \_ -> \_ -> 0
