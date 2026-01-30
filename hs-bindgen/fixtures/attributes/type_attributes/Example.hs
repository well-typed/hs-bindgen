{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Array.Byte
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.ByteArray
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Internal.SizedByteArray
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @struct S@

    __defined at:__ @attributes\/type_attributes.h 8:8@

    __exported by:__ @attributes\/type_attributes.h@
-}
data S = S
  { s_f :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CShort
    {- ^ __C declaration:__ @f@

         __defined at:__ @attributes\/type_attributes.h 8:18@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s_f") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_f2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s_f") ptr0 s_f2

deriving via HsBindgen.Runtime.Marshal.EquivStorable S instance F.Storable S

instance HsBindgen.Runtime.HasCField.HasCField S "s_f" where

  type CFieldType S "s_f" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CShort

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S) "s_f")
         ) => GHC.Records.HasField "s_f" (Ptr.Ptr S) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s_f")

{-| __C declaration:__ @more_aligned_int@

    __defined at:__ @attributes\/type_attributes.h 9:13@

    __exported by:__ @attributes\/type_attributes.h@
-}
newtype More_aligned_int = More_aligned_int
  { unwrapMore_aligned_int :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType More_aligned_int) "unwrapMore_aligned_int")
         ) => GHC.Records.HasField "unwrapMore_aligned_int" (Ptr.Ptr More_aligned_int) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMore_aligned_int")

instance HsBindgen.Runtime.HasCField.HasCField More_aligned_int "unwrapMore_aligned_int" where

  type CFieldType More_aligned_int "unwrapMore_aligned_int" =
    FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S2@

    __defined at:__ @attributes\/type_attributes.h 11:8@

    __exported by:__ @attributes\/type_attributes.h@
-}
data S2 = S2
  { s2_f :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CShort
    {- ^ __C declaration:__ @f@

         __defined at:__ @attributes\/type_attributes.h 11:19@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S2 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (16 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S2 where

  readRaw =
    \ptr0 ->
          pure S2
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s2_f") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_f2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s2_f") ptr0 s2_f2

deriving via HsBindgen.Runtime.Marshal.EquivStorable S2 instance F.Storable S2

instance HsBindgen.Runtime.HasCField.HasCField S2 "s2_f" where

  type CFieldType S2 "s2_f" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CShort

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2) "s2_f")
         ) => GHC.Records.HasField "s2_f" (Ptr.Ptr S2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s2_f")

{-| __C declaration:__ @struct my_unpacked_struct@

    __defined at:__ @attributes\/type_attributes.h 13:8@

    __exported by:__ @attributes\/type_attributes.h@
-}
data My_unpacked_struct = My_unpacked_struct
  { my_unpacked_struct_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/type_attributes.h 15:8@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  , my_unpacked_struct_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/type_attributes.h 16:7@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize My_unpacked_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw My_unpacked_struct where

  readRaw =
    \ptr0 ->
          pure My_unpacked_struct
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"my_unpacked_struct_c") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"my_unpacked_struct_i") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw My_unpacked_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          My_unpacked_struct my_unpacked_struct_c2 my_unpacked_struct_i3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"my_unpacked_struct_c") ptr0 my_unpacked_struct_c2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"my_unpacked_struct_i") ptr0 my_unpacked_struct_i3

deriving via HsBindgen.Runtime.Marshal.EquivStorable My_unpacked_struct instance F.Storable My_unpacked_struct

instance HsBindgen.Runtime.HasCField.HasCField My_unpacked_struct "my_unpacked_struct_c" where

  type CFieldType My_unpacked_struct "my_unpacked_struct_c" =
    FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType My_unpacked_struct) "my_unpacked_struct_c")
         ) => GHC.Records.HasField "my_unpacked_struct_c" (Ptr.Ptr My_unpacked_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"my_unpacked_struct_c")

instance HsBindgen.Runtime.HasCField.HasCField My_unpacked_struct "my_unpacked_struct_i" where

  type CFieldType My_unpacked_struct "my_unpacked_struct_i" =
    FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType My_unpacked_struct) "my_unpacked_struct_i")
         ) => GHC.Records.HasField "my_unpacked_struct_i" (Ptr.Ptr My_unpacked_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"my_unpacked_struct_i")

{-| __C declaration:__ @struct my_packed_struct@

    __defined at:__ @attributes\/type_attributes.h 19:37@

    __exported by:__ @attributes\/type_attributes.h@
-}
data My_packed_struct = My_packed_struct
  { my_packed_struct_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/type_attributes.h 21:9@

         __exported by:__ @attributes\/type_attributes.h@
    -}
  , my_packed_struct_i :: FC.CInt
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
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize My_packed_struct where

  staticSizeOf = \_ -> (13 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw My_packed_struct where

  readRaw =
    \ptr0 ->
          pure My_packed_struct
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"my_packed_struct_c") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"my_packed_struct_i") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"my_packed_struct_s") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw My_packed_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          My_packed_struct my_packed_struct_c2 my_packed_struct_i3 my_packed_struct_s4 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"my_packed_struct_c") ptr0 my_packed_struct_c2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"my_packed_struct_i") ptr0 my_packed_struct_i3
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"my_packed_struct_s") ptr0 my_packed_struct_s4

deriving via HsBindgen.Runtime.Marshal.EquivStorable My_packed_struct instance F.Storable My_packed_struct

instance HsBindgen.Runtime.HasCField.HasCField My_packed_struct "my_packed_struct_c" where

  type CFieldType My_packed_struct "my_packed_struct_c" =
    FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType My_packed_struct) "my_packed_struct_c")
         ) => GHC.Records.HasField "my_packed_struct_c" (Ptr.Ptr My_packed_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"my_packed_struct_c")

instance HsBindgen.Runtime.HasCField.HasCField My_packed_struct "my_packed_struct_i" where

  type CFieldType My_packed_struct "my_packed_struct_i" =
    FC.CInt

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType My_packed_struct) "my_packed_struct_i")
         ) => GHC.Records.HasField "my_packed_struct_i" (Ptr.Ptr My_packed_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"my_packed_struct_i")

instance HsBindgen.Runtime.HasCField.HasCField My_packed_struct "my_packed_struct_s" where

  type CFieldType My_packed_struct "my_packed_struct_s" =
    My_unpacked_struct

  offset# = \_ -> \_ -> 5

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType My_packed_struct) "my_packed_struct_s")
         ) => GHC.Records.HasField "my_packed_struct_s" (Ptr.Ptr My_packed_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"my_packed_struct_s")

{-| __C declaration:__ @union wait_status_ptr_t@

    __defined at:__ @attributes\/type_attributes.h 26:9@

    __exported by:__ @attributes\/type_attributes.h@
-}
newtype Wait_status_ptr_t = Wait_status_ptr_t
  { unwrapWait_status_ptr_t :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 8 instance HsBindgen.Runtime.Marshal.StaticSize Wait_status_ptr_t

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 8 instance HsBindgen.Runtime.Marshal.ReadRaw Wait_status_ptr_t

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 8 instance HsBindgen.Runtime.Marshal.WriteRaw Wait_status_ptr_t

deriving via HsBindgen.Runtime.Marshal.EquivStorable Wait_status_ptr_t instance F.Storable Wait_status_ptr_t

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 8 instance Data.Primitive.Types.Prim Wait_status_ptr_t

{-|

  __See:__ 'set_wait_status_ptr_t___ip'

__C declaration:__ @__ip@

__defined at:__ @attributes\/type_attributes.h 28:8@

__exported by:__ @attributes\/type_attributes.h@
-}
get_wait_status_ptr_t___ip ::
     Wait_status_ptr_t
  -> Ptr.Ptr FC.CInt
get_wait_status_ptr_t___ip =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_wait_status_ptr_t___ip'

-}
set_wait_status_ptr_t___ip ::
     Ptr.Ptr FC.CInt
  -> Wait_status_ptr_t
set_wait_status_ptr_t___ip =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

{-|

  __See:__ 'set_wait_status_ptr_t___up'

__C declaration:__ @__up@

__defined at:__ @attributes\/type_attributes.h 29:15@

__exported by:__ @attributes\/type_attributes.h@
-}
get_wait_status_ptr_t___up ::
     Wait_status_ptr_t
  -> Ptr.Ptr Wait
get_wait_status_ptr_t___up =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_wait_status_ptr_t___up'

-}
set_wait_status_ptr_t___up ::
     Ptr.Ptr Wait
  -> Wait_status_ptr_t
set_wait_status_ptr_t___up =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField Wait_status_ptr_t "wait_status_ptr_t___ip" where

  type CFieldType Wait_status_ptr_t "wait_status_ptr_t___ip" =
    Ptr.Ptr FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Wait_status_ptr_t) "wait_status_ptr_t___ip")
         ) => GHC.Records.HasField "wait_status_ptr_t___ip" (Ptr.Ptr Wait_status_ptr_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"wait_status_ptr_t___ip")

instance HsBindgen.Runtime.HasCField.HasCField Wait_status_ptr_t "wait_status_ptr_t___up" where

  type CFieldType Wait_status_ptr_t "wait_status_ptr_t___up" =
    Ptr.Ptr Wait

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Wait_status_ptr_t) "wait_status_ptr_t___up")
         ) => GHC.Records.HasField "wait_status_ptr_t___up" (Ptr.Ptr Wait_status_ptr_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"wait_status_ptr_t___up")

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
  { unwrapT1 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType T1) "unwrapT1")
         ) => GHC.Records.HasField "unwrapT1" (Ptr.Ptr T1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapT1")

instance HsBindgen.Runtime.HasCField.HasCField T1 "unwrapT1" where

  type CFieldType T1 "unwrapT1" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @short_a@

    __defined at:__ @attributes\/type_attributes.h 34:46@

    __exported by:__ @attributes\/type_attributes.h@
-}
newtype Short_a = Short_a
  { unwrapShort_a :: FC.CShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Short_a) "unwrapShort_a")
         ) => GHC.Records.HasField "unwrapShort_a" (Ptr.Ptr Short_a) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapShort_a")

instance HsBindgen.Runtime.HasCField.HasCField Short_a "unwrapShort_a" where

  type CFieldType Short_a "unwrapShort_a" = FC.CShort

  offset# = \_ -> \_ -> 0
