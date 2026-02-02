{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.Bitfield
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @MC@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 4:9@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
newtype MC = MC
  { unwrapMC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MC) "unwrapMC")
         ) => GHC.Records.HasField "unwrapMC" (Ptr.Ptr MC) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMC")

instance HsBindgen.Runtime.HasCField.HasCField MC "unwrapMC" where

  type CFieldType MC "unwrapMC" = FC.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @TC@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 5:14@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
newtype TC = TC
  { unwrapTC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType TC) "unwrapTC")
         ) => GHC.Records.HasField "unwrapTC" (Ptr.Ptr TC) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTC")

instance HsBindgen.Runtime.HasCField.HasCField TC "unwrapTC" where

  type CFieldType TC "unwrapTC" = FC.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 18:16@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct1 = Struct1
  { struct1_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 18:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Struct1 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Struct1 where

  readRaw =
    \ptr0 ->
          pure Struct1
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"struct1_a") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Struct1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1 struct1_a2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"struct1_a") ptr0 struct1_a2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Struct1 instance F.Storable Struct1

instance Data.Primitive.Types.Prim Struct1 where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Struct1 (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Struct1 v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct1 struct1_a4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 struct1_a4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Struct1 (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Struct1 v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct1 struct1_a4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 struct1_a4 s3

instance HsBindgen.Runtime.HasCField.HasCField Struct1 "struct1_a" where

  type CFieldType Struct1 "struct1_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct1) "struct1_a")
         ) => GHC.Records.HasField "struct1_a" (Ptr.Ptr Struct1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct1_a")

{-| __C declaration:__ @struct struct2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 19:9@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct2 = Struct2
  { struct2_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 19:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Struct2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Struct2 where

  readRaw =
    \ptr0 ->
          pure Struct2
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"struct2_a") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Struct2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_a2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"struct2_a") ptr0 struct2_a2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Struct2 instance F.Storable Struct2

instance Data.Primitive.Types.Prim Struct2 where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Struct2 (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Struct2 v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct2 struct2_a4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 struct2_a4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Struct2 (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Struct2 v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct2 struct2_a4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 struct2_a4 s3

instance HsBindgen.Runtime.HasCField.HasCField Struct2 "struct2_a" where

  type CFieldType Struct2 "struct2_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct2) "struct2_a")
         ) => GHC.Records.HasField "struct2_a" (Ptr.Ptr Struct2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct2_a")

{-| __C declaration:__ @struct struct3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 20:16@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct3 = Struct3
  { struct3_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 20:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Struct3 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Struct3 where

  readRaw =
    \ptr0 ->
          pure Struct3
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"struct3_a") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Struct3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct3 struct3_a2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"struct3_a") ptr0 struct3_a2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Struct3 instance F.Storable Struct3

instance Data.Primitive.Types.Prim Struct3 where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Struct3 (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Struct3 v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct3 struct3_a4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 struct3_a4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Struct3 (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Struct3 v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct3 struct3_a4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 struct3_a4 s3

instance HsBindgen.Runtime.HasCField.HasCField Struct3 "struct3_a" where

  type CFieldType Struct3 "struct3_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct3) "struct3_a")
         ) => GHC.Records.HasField "struct3_a" (Ptr.Ptr Struct3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct3_a")

{-| __C declaration:__ @struct3_t@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 20:35@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
newtype Struct3_t = Struct3_t
  { unwrapStruct3_t :: Struct3
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , Data.Primitive.Types.Prim
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct3_t) "unwrapStruct3_t")
         ) => GHC.Records.HasField "unwrapStruct3_t" (Ptr.Ptr Struct3_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStruct3_t")

instance HsBindgen.Runtime.HasCField.HasCField Struct3_t "unwrapStruct3_t" where

  type CFieldType Struct3_t "unwrapStruct3_t" = Struct3

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct struct4@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 21:16@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
data Struct4 = Struct4
  { struct4_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 21:30@

         __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Struct4 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Struct4 where

  readRaw =
    \ptr0 ->
          pure Struct4
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"struct4_a") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Struct4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct4 struct4_a2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"struct4_a") ptr0 struct4_a2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Struct4 instance F.Storable Struct4

instance Data.Primitive.Types.Prim Struct4 where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Struct4 (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Struct4 v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct4 struct4_a4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 struct4_a4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Struct4 (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Struct4 v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct4 struct4_a4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 struct4_a4 s3

instance HsBindgen.Runtime.HasCField.HasCField Struct4 "struct4_a" where

  type CFieldType Struct4 "struct4_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct4) "struct4_a")
         ) => GHC.Records.HasField "struct4_a" (Ptr.Ptr Struct4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct4_a")
