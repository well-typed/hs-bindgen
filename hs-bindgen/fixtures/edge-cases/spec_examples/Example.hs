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

{-| Examples from the initial specification

__C declaration:__ @int16_T@

__defined at:__ @edge-cases\/spec_examples.h 10:15@

__exported by:__ @edge-cases\/spec_examples.h@
-}
newtype Int16_T = Int16_T
  { unwrapInt16_T :: RIP.CShort
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
         ) => RIP.HasField "unwrapInt16_T" (RIP.Ptr Int16_T) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapInt16_T")

instance HasCField.HasCField Int16_T "unwrapInt16_T" where

  type CFieldType Int16_T "unwrapInt16_T" = RIP.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int32_T@

    __defined at:__ @edge-cases\/spec_examples.h 11:13@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
newtype Int32_T = Int32_T
  { unwrapInt32_T :: RIP.CInt
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
         ) => RIP.HasField "unwrapInt32_T" (RIP.Ptr Int32_T) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapInt32_T")

instance HasCField.HasCField Int32_T "unwrapInt32_T" where

  type CFieldType Int32_T "unwrapInt32_T" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int64_T@

    __defined at:__ @edge-cases\/spec_examples.h 12:19@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
newtype Int64_T = Int64_T
  { unwrapInt64_T :: RIP.CLLong
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

instance ( ((~) ty) RIP.CLLong
         ) => RIP.HasField "unwrapInt64_T" (RIP.Ptr Int64_T) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapInt64_T")

instance HasCField.HasCField Int64_T "unwrapInt64_T" where

  type CFieldType Int64_T "unwrapInt64_T" = RIP.CLLong

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Cint16_T where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Cint16_T where

  readRaw =
    \ptr0 ->
          pure Cint16_T
      <*> HasCField.readRaw (RIP.Proxy @"cint16_T_re") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"cint16_T_im") ptr0

instance Marshal.WriteRaw Cint16_T where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Cint16_T cint16_T_re2 cint16_T_im3 ->
               HasCField.writeRaw (RIP.Proxy @"cint16_T_re") ptr0 cint16_T_re2
            >> HasCField.writeRaw (RIP.Proxy @"cint16_T_im") ptr0 cint16_T_im3

deriving via Marshal.EquivStorable Cint16_T instance RIP.Storable Cint16_T

instance HasCField.HasCField Cint16_T "cint16_T_re" where

  type CFieldType Cint16_T "cint16_T_re" = Int16_T

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Int16_T
         ) => RIP.HasField "cint16_T_re" (RIP.Ptr Cint16_T) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"cint16_T_re")

instance HasCField.HasCField Cint16_T "cint16_T_im" where

  type CFieldType Cint16_T "cint16_T_im" = Int16_T

  offset# = \_ -> \_ -> 2

instance ( ((~) ty) Int16_T
         ) => RIP.HasField "cint16_T_im" (RIP.Ptr Cint16_T) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"cint16_T_im")

{-| __C declaration:__ @struct B@

    __defined at:__ @edge-cases\/spec_examples.h 19:8@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data B = B
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize B where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw B where

  readRaw = \ptr0 -> pure B

instance Marshal.WriteRaw B where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B -> return ()

deriving via Marshal.EquivStorable B instance RIP.Storable B

{-| __C declaration:__ @struct A@

    __defined at:__ @edge-cases\/spec_examples.h 23:8@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data A = A
  { a_x :: RIP.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/spec_examples.h 24:10@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_label :: RIP.Ptr RIP.CChar
    {- ^ __C declaration:__ @label@

         __defined at:__ @edge-cases\/spec_examples.h 25:9@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_samples :: (CA.ConstantArray 128) RIP.CChar
    {- ^ __C declaration:__ @samples@

         __defined at:__ @edge-cases\/spec_examples.h 26:8@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_b :: B
    {- ^ __C declaration:__ @b@

         __defined at:__ @edge-cases\/spec_examples.h 27:12@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_c :: RIP.Ptr C
    {- ^ __C declaration:__ @c@

         __defined at:__ @edge-cases\/spec_examples.h 28:13@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize A where

  staticSizeOf = \_ -> (152 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> HasCField.readRaw (RIP.Proxy @"a_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_label") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_samples") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_b") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"a_c") ptr0

instance Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_x2 a_label3 a_samples4 a_b5 a_c6 ->
               HasCField.writeRaw (RIP.Proxy @"a_x") ptr0 a_x2
            >> HasCField.writeRaw (RIP.Proxy @"a_label") ptr0 a_label3
            >> HasCField.writeRaw (RIP.Proxy @"a_samples") ptr0 a_samples4
            >> HasCField.writeRaw (RIP.Proxy @"a_b") ptr0 a_b5
            >> HasCField.writeRaw (RIP.Proxy @"a_c") ptr0 a_c6

deriving via Marshal.EquivStorable A instance RIP.Storable A

instance HasCField.HasCField A "a_x" where

  type CFieldType A "a_x" = RIP.CDouble

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "a_x" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a_x")

instance HasCField.HasCField A "a_label" where

  type CFieldType A "a_label" = RIP.Ptr RIP.CChar

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr RIP.CChar)
         ) => RIP.HasField "a_label" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a_label")

instance HasCField.HasCField A "a_samples" where

  type CFieldType A "a_samples" =
    (CA.ConstantArray 128) RIP.CChar

  offset# = \_ -> \_ -> 16

instance ( ((~) ty) ((CA.ConstantArray 128) RIP.CChar)
         ) => RIP.HasField "a_samples" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a_samples")

instance HasCField.HasCField A "a_b" where

  type CFieldType A "a_b" = B

  offset# = \_ -> \_ -> 144

instance (((~) ty) B) => RIP.HasField "a_b" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a_b")

instance HasCField.HasCField A "a_c" where

  type CFieldType A "a_c" = RIP.Ptr C

  offset# = \_ -> \_ -> 144

instance ( ((~) ty) (RIP.Ptr C)
         ) => RIP.HasField "a_c" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a_c")

{-| __C declaration:__ @struct C@

    __defined at:__ @edge-cases\/spec_examples.h 28:10@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data C
