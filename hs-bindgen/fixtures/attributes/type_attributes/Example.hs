{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct S@

    __defined at:__ @attributes\/type_attributes.h 8:8@

    __exported by:__ @attributes\/type_attributes.h@
-}
data S = S
  { s_f :: (CA.ConstantArray 3) RIP.CShort
    {- ^ __C declaration:__ @f@

         __defined at:__ @attributes\/type_attributes.h 8:18@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (RIP.Proxy @"s_f") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_f2 ->
            HasCField.writeRaw (RIP.Proxy @"s_f") ptr0 s_f2

deriving via Marshal.EquivStorable S instance RIP.Storable S

instance HasCField.HasCField S "s_f" where

  type CFieldType S "s_f" =
    (CA.ConstantArray 3) RIP.CShort

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CShort)
         ) => RIP.HasField "s_f" (RIP.Ptr S) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s_f")

{-| __C declaration:__ @more_aligned_int@

    __defined at:__ @attributes\/type_attributes.h 9:13@

    __exported by:__ @attributes\/type_attributes.h@
-}
newtype More_aligned_int = More_aligned_int
  { unwrapMore_aligned_int :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapMore_aligned_int" (RIP.Ptr More_aligned_int) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMore_aligned_int")

instance HasCField.HasCField More_aligned_int "unwrapMore_aligned_int" where

  type CFieldType More_aligned_int "unwrapMore_aligned_int" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S2@

    __defined at:__ @attributes\/type_attributes.h 11:8@

    __exported by:__ @attributes\/type_attributes.h@
-}
data S2 = S2
  { s2_f :: (CA.ConstantArray 3) RIP.CShort
    {- ^ __C declaration:__ @f@

         __defined at:__ @attributes\/type_attributes.h 11:19@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S2 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (16 :: Int)

instance Marshal.ReadRaw S2 where

  readRaw =
    \ptr0 ->
          pure S2
      <*> HasCField.readRaw (RIP.Proxy @"s2_f") ptr0

instance Marshal.WriteRaw S2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_f2 ->
            HasCField.writeRaw (RIP.Proxy @"s2_f") ptr0 s2_f2

deriving via Marshal.EquivStorable S2 instance RIP.Storable S2

instance HasCField.HasCField S2 "s2_f" where

  type CFieldType S2 "s2_f" =
    (CA.ConstantArray 3) RIP.CShort

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CShort)
         ) => RIP.HasField "s2_f" (RIP.Ptr S2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s2_f")

{-| __C declaration:__ @struct my_unpacked_struct@

    __defined at:__ @attributes\/type_attributes.h 13:8@

    __exported by:__ @attributes\/type_attributes.h@
-}
data My_unpacked_struct = My_unpacked_struct
  { my_unpacked_struct_c :: RIP.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/type_attributes.h 15:8@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  , my_unpacked_struct_i :: RIP.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/type_attributes.h 16:7@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize My_unpacked_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw My_unpacked_struct where

  readRaw =
    \ptr0 ->
          pure My_unpacked_struct
      <*> HasCField.readRaw (RIP.Proxy @"my_unpacked_struct_c") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"my_unpacked_struct_i") ptr0

instance Marshal.WriteRaw My_unpacked_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          My_unpacked_struct my_unpacked_struct_c2 my_unpacked_struct_i3 ->
               HasCField.writeRaw (RIP.Proxy @"my_unpacked_struct_c") ptr0 my_unpacked_struct_c2
            >> HasCField.writeRaw (RIP.Proxy @"my_unpacked_struct_i") ptr0 my_unpacked_struct_i3

deriving via Marshal.EquivStorable My_unpacked_struct instance RIP.Storable My_unpacked_struct

instance HasCField.HasCField My_unpacked_struct "my_unpacked_struct_c" where

  type CFieldType My_unpacked_struct "my_unpacked_struct_c" =
    RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "my_unpacked_struct_c" (RIP.Ptr My_unpacked_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"my_unpacked_struct_c")

instance HasCField.HasCField My_unpacked_struct "my_unpacked_struct_i" where

  type CFieldType My_unpacked_struct "my_unpacked_struct_i" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "my_unpacked_struct_i" (RIP.Ptr My_unpacked_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"my_unpacked_struct_i")

{-| __C declaration:__ @struct my_packed_struct@

    __defined at:__ @attributes\/type_attributes.h 19:37@

    __exported by:__ @attributes\/type_attributes.h@
-}
data My_packed_struct = My_packed_struct
  { my_packed_struct_c :: RIP.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/type_attributes.h 21:9@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  , my_packed_struct_i :: RIP.CInt
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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize My_packed_struct where

  staticSizeOf = \_ -> (13 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw My_packed_struct where

  readRaw =
    \ptr0 ->
          pure My_packed_struct
      <*> HasCField.readRaw (RIP.Proxy @"my_packed_struct_c") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"my_packed_struct_i") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"my_packed_struct_s") ptr0

instance Marshal.WriteRaw My_packed_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          My_packed_struct my_packed_struct_c2 my_packed_struct_i3 my_packed_struct_s4 ->
               HasCField.writeRaw (RIP.Proxy @"my_packed_struct_c") ptr0 my_packed_struct_c2
            >> HasCField.writeRaw (RIP.Proxy @"my_packed_struct_i") ptr0 my_packed_struct_i3
            >> HasCField.writeRaw (RIP.Proxy @"my_packed_struct_s") ptr0 my_packed_struct_s4

deriving via Marshal.EquivStorable My_packed_struct instance RIP.Storable My_packed_struct

instance HasCField.HasCField My_packed_struct "my_packed_struct_c" where

  type CFieldType My_packed_struct "my_packed_struct_c" =
    RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "my_packed_struct_c" (RIP.Ptr My_packed_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"my_packed_struct_c")

instance HasCField.HasCField My_packed_struct "my_packed_struct_i" where

  type CFieldType My_packed_struct "my_packed_struct_i" =
    RIP.CInt

  offset# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "my_packed_struct_i" (RIP.Ptr My_packed_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"my_packed_struct_i")

instance HasCField.HasCField My_packed_struct "my_packed_struct_s" where

  type CFieldType My_packed_struct "my_packed_struct_s" =
    My_unpacked_struct

  offset# = \_ -> \_ -> 5

instance ( ((~) ty) My_unpacked_struct
         ) => RIP.HasField "my_packed_struct_s" (RIP.Ptr My_packed_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"my_packed_struct_s")

{-| __C declaration:__ @union wait_status_ptr_t@

    __defined at:__ @attributes\/type_attributes.h 26:9@

    __exported by:__ @attributes\/type_attributes.h@
-}
newtype Wait_status_ptr_t = Wait_status_ptr_t
  { unwrapWait_status_ptr_t :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 8) 8 instance Marshal.StaticSize Wait_status_ptr_t

deriving via (RIP.SizedByteArray 8) 8 instance Marshal.ReadRaw Wait_status_ptr_t

deriving via (RIP.SizedByteArray 8) 8 instance Marshal.WriteRaw Wait_status_ptr_t

deriving via Marshal.EquivStorable Wait_status_ptr_t instance RIP.Storable Wait_status_ptr_t

{-|

  __See:__ 'set_wait_status_ptr_t___ip'

__C declaration:__ @__ip@

__defined at:__ @attributes\/type_attributes.h 28:8@

__exported by:__ @attributes\/type_attributes.h@
-}
get_wait_status_ptr_t___ip ::
     Wait_status_ptr_t
  -> RIP.Ptr RIP.CInt
get_wait_status_ptr_t___ip = RIP.getUnionPayload

{-|

  __See:__ 'get_wait_status_ptr_t___ip'

-}
set_wait_status_ptr_t___ip ::
     RIP.Ptr RIP.CInt
  -> Wait_status_ptr_t
set_wait_status_ptr_t___ip = RIP.setUnionPayload

{-|

  __See:__ 'set_wait_status_ptr_t___up'

__C declaration:__ @__up@

__defined at:__ @attributes\/type_attributes.h 29:15@

__exported by:__ @attributes\/type_attributes.h@
-}
get_wait_status_ptr_t___up ::
     Wait_status_ptr_t
  -> RIP.Ptr Wait
get_wait_status_ptr_t___up = RIP.getUnionPayload

{-|

  __See:__ 'get_wait_status_ptr_t___up'

-}
set_wait_status_ptr_t___up ::
     RIP.Ptr Wait
  -> Wait_status_ptr_t
set_wait_status_ptr_t___up = RIP.setUnionPayload

instance HasCField.HasCField Wait_status_ptr_t "wait_status_ptr_t___ip" where

  type CFieldType Wait_status_ptr_t "wait_status_ptr_t___ip" =
    RIP.Ptr RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.Ptr RIP.CInt)
         ) => RIP.HasField "wait_status_ptr_t___ip" (RIP.Ptr Wait_status_ptr_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"wait_status_ptr_t___ip")

instance HasCField.HasCField Wait_status_ptr_t "wait_status_ptr_t___up" where

  type CFieldType Wait_status_ptr_t "wait_status_ptr_t___up" =
    RIP.Ptr Wait

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.Ptr Wait)
         ) => RIP.HasField "wait_status_ptr_t___up" (RIP.Ptr Wait_status_ptr_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"wait_status_ptr_t___up")

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
  { unwrapT1 :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapT1" (RIP.Ptr T1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT1")

instance HasCField.HasCField T1 "unwrapT1" where

  type CFieldType T1 "unwrapT1" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @short_a@

    __defined at:__ @attributes\/type_attributes.h 34:46@

    __exported by:__ @attributes\/type_attributes.h@
-}
newtype Short_a = Short_a
  { unwrapShort_a :: RIP.CShort
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CShort
         ) => RIP.HasField "unwrapShort_a" (RIP.Ptr Short_a) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapShort_a")

instance HasCField.HasCField Short_a "unwrapShort_a" where

  type CFieldType Short_a "unwrapShort_a" = RIP.CShort

  offset# = \_ -> \_ -> 0
