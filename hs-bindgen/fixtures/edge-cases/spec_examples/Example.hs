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

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.Bitfield
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import GHC.Exts ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, return)

{-| Examples from the initial specification

__C declaration:__ @int16_T@

__defined at:__ @edge-cases\/spec_examples.h 10:15@

__exported by:__ @edge-cases\/spec_examples.h@
-}
newtype Int16_T = Int16_T
  { unwrapInt16_T :: FC.CShort
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Int16_T) "unwrapInt16_T")
         ) => GHC.Records.HasField "unwrapInt16_T" (Ptr.Ptr Int16_T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt16_T")

instance HsBindgen.Runtime.HasCField.HasCField Int16_T "unwrapInt16_T" where

  type CFieldType Int16_T "unwrapInt16_T" = FC.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int32_T@

    __defined at:__ @edge-cases\/spec_examples.h 11:13@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
newtype Int32_T = Int32_T
  { unwrapInt32_T :: FC.CInt
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Int32_T) "unwrapInt32_T")
         ) => GHC.Records.HasField "unwrapInt32_T" (Ptr.Ptr Int32_T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt32_T")

instance HsBindgen.Runtime.HasCField.HasCField Int32_T "unwrapInt32_T" where

  type CFieldType Int32_T "unwrapInt32_T" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int64_T@

    __defined at:__ @edge-cases\/spec_examples.h 12:19@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
newtype Int64_T = Int64_T
  { unwrapInt64_T :: FC.CLLong
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Int64_T) "unwrapInt64_T")
         ) => GHC.Records.HasField "unwrapInt64_T" (Ptr.Ptr Int64_T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt64_T")

instance HsBindgen.Runtime.HasCField.HasCField Int64_T "unwrapInt64_T" where

  type CFieldType Int64_T "unwrapInt64_T" = FC.CLLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct cint16_T@

    __defined at:__ @edge-cases\/spec_examples.h 14:9@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data Cint16_T = Cint16_T
  { cint16_T_re :: Int16_T
    {- ^ __C declaration:__ @re@

         __defined at:__ @edge-cases\/spec_examples.h 15:11@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , cint16_T_im :: Int16_T
    {- ^ __C declaration:__ @im@

         __defined at:__ @edge-cases\/spec_examples.h 16:11@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Cint16_T where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Cint16_T where

  readRaw =
    \ptr0 ->
          pure Cint16_T
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"cint16_T_re") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"cint16_T_im") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Cint16_T where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Cint16_T cint16_T_re2 cint16_T_im3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"cint16_T_re") ptr0 cint16_T_re2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"cint16_T_im") ptr0 cint16_T_im3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Cint16_T instance F.Storable Cint16_T

instance Data.Primitive.Types.Prim Cint16_T where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (2#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Cint16_T (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Cint16_T v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Cint16_T cint16_T_re4 cint16_T_im5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) cint16_T_re4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) cint16_T_im5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Cint16_T (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Cint16_T v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Cint16_T cint16_T_re4 cint16_T_im5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) cint16_T_re4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) cint16_T_im5 s6

instance HsBindgen.Runtime.HasCField.HasCField Cint16_T "cint16_T_re" where

  type CFieldType Cint16_T "cint16_T_re" = Int16_T

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Cint16_T) "cint16_T_re")
         ) => GHC.Records.HasField "cint16_T_re" (Ptr.Ptr Cint16_T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"cint16_T_re")

instance HsBindgen.Runtime.HasCField.HasCField Cint16_T "cint16_T_im" where

  type CFieldType Cint16_T "cint16_T_im" = Int16_T

  offset# = \_ -> \_ -> 2

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Cint16_T) "cint16_T_im")
         ) => GHC.Records.HasField "cint16_T_im" (Ptr.Ptr Cint16_T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"cint16_T_im")

{-| __C declaration:__ @struct B@

    __defined at:__ @edge-cases\/spec_examples.h 19:8@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data B = B
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize B where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw B where

  readRaw = \ptr0 -> pure B

instance HsBindgen.Runtime.Marshal.WriteRaw B where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable B instance F.Storable B

instance Data.Primitive.Types.Prim B where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> B

  readByteArray# = \arr0 -> \i1 -> \s2 -> (# s2, B #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              B -> s3

  indexOffAddr# = \addr0 -> \i1 -> B

  readOffAddr# = \addr0 -> \i1 -> \s2 -> (# s2, B #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              B -> s3

{-| __C declaration:__ @struct A@

    __defined at:__ @edge-cases\/spec_examples.h 23:8@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data A = A
  { a_x :: FC.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/spec_examples.h 24:10@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_label :: Ptr.Ptr FC.CChar
    {- ^ __C declaration:__ @label@

         __defined at:__ @edge-cases\/spec_examples.h 25:9@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_samples :: (HsBindgen.Runtime.ConstantArray.ConstantArray 128) FC.CChar
    {- ^ __C declaration:__ @samples@

         __defined at:__ @edge-cases\/spec_examples.h 26:8@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_b :: B
    {- ^ __C declaration:__ @b@

         __defined at:__ @edge-cases\/spec_examples.h 27:12@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_c :: Ptr.Ptr C
    {- ^ __C declaration:__ @c@

         __defined at:__ @edge-cases\/spec_examples.h 28:13@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize A where

  staticSizeOf = \_ -> (152 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a_label") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a_samples") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a_b") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a_c") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_x2 a_label3 a_samples4 a_b5 a_c6 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a_x") ptr0 a_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a_label") ptr0 a_label3
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a_samples") ptr0 a_samples4
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a_b") ptr0 a_b5
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a_c") ptr0 a_c6

deriving via HsBindgen.Runtime.Marshal.EquivStorable A instance F.Storable A

instance HsBindgen.Runtime.HasCField.HasCField A "a_x" where

  type CFieldType A "a_x" = FC.CDouble

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "a_x")
         ) => GHC.Records.HasField "a_x" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a_x")

instance HsBindgen.Runtime.HasCField.HasCField A "a_label" where

  type CFieldType A "a_label" = Ptr.Ptr FC.CChar

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "a_label")
         ) => GHC.Records.HasField "a_label" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a_label")

instance HsBindgen.Runtime.HasCField.HasCField A "a_samples" where

  type CFieldType A "a_samples" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 128) FC.CChar

  offset# = \_ -> \_ -> 16

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "a_samples")
         ) => GHC.Records.HasField "a_samples" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a_samples")

instance HsBindgen.Runtime.HasCField.HasCField A "a_b" where

  type CFieldType A "a_b" = B

  offset# = \_ -> \_ -> 144

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "a_b")
         ) => GHC.Records.HasField "a_b" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a_b")

instance HsBindgen.Runtime.HasCField.HasCField A "a_c" where

  type CFieldType A "a_c" = Ptr.Ptr C

  offset# = \_ -> \_ -> 144

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "a_c")
         ) => GHC.Records.HasField "a_c" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a_c")

{-| __C declaration:__ @struct C@

    __defined at:__ @edge-cases\/spec_examples.h 28:10@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data C
