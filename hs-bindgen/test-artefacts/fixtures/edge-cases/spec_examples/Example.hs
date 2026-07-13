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
    ( Example.Int16_T(..)
    , Example.Int32_T(..)
    , Example.Int64_T(..)
    , Example.Cint16_T(..)
    , Example.B(..)
    , Example.A(..)
    , Example.C
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @int16_T@

    __defined at:__ @edge-cases\/spec_examples.h 10:15@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
newtype Int16_T = Int16_T
  { unwrapInt16_T :: BG.CShort
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
         ) => BG.CompatHasField.HasField "unwrapInt16_T" Int16_T ty where

  hasField =
    \x0 ->
      (\y1 ->
         Int16_T {unwrapInt16_T = y1}, BG.getField @"unwrapInt16_T" x0)

instance ( ty ~ BG.CShort
         ) => BG.HasField "unwrapInt16_T" (BG.Ptr Int16_T) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInt16_T")

instance HasCField.HasCField Int16_T "unwrapInt16_T" where

  type CFieldType Int16_T "unwrapInt16_T" = BG.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int32_T@

    __defined at:__ @edge-cases\/spec_examples.h 11:13@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
newtype Int32_T = Int32_T
  { unwrapInt32_T :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapInt32_T" Int32_T ty where

  hasField =
    \x0 ->
      (\y1 ->
         Int32_T {unwrapInt32_T = y1}, BG.getField @"unwrapInt32_T" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapInt32_T" (BG.Ptr Int32_T) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInt32_T")

instance HasCField.HasCField Int32_T "unwrapInt32_T" where

  type CFieldType Int32_T "unwrapInt32_T" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int64_T@

    __defined at:__ @edge-cases\/spec_examples.h 12:19@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
newtype Int64_T = Int64_T
  { unwrapInt64_T :: BG.CLLong
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

instance ( ty ~ BG.CLLong
         ) => BG.CompatHasField.HasField "unwrapInt64_T" Int64_T ty where

  hasField =
    \x0 ->
      (\y1 ->
         Int64_T {unwrapInt64_T = y1}, BG.getField @"unwrapInt64_T" x0)

instance ( ty ~ BG.CLLong
         ) => BG.HasField "unwrapInt64_T" (BG.Ptr Int64_T) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapInt64_T")

instance HasCField.HasCField Int64_T "unwrapInt64_T" where

  type CFieldType Int64_T "unwrapInt64_T" = BG.CLLong

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
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Cint16_T where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Cint16_T where

  readRaw =
    \ptr0 ->
          pure Cint16_T
      <*> HasCField.readRaw (BG.Proxy @"cint16_T_re") ptr0
      <*> HasCField.readRaw (BG.Proxy @"cint16_T_im") ptr0

instance Marshal.WriteRaw Cint16_T where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Cint16_T cint16_T_re2 cint16_T_im3 ->
               HasCField.writeRaw (BG.Proxy @"cint16_T_re") ptr0 cint16_T_re2
            >> HasCField.writeRaw (BG.Proxy @"cint16_T_im") ptr0 cint16_T_im3

deriving via Marshal.EquivStorable Cint16_T instance BG.Storable Cint16_T

instance ( ty ~ Int16_T
         ) => BG.CompatHasField.HasField "cint16_T_re" Cint16_T ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cint16_T {cint16_T_re = y1, cint16_T_im = BG.getField @"cint16_T_im" x0}
      , BG.getField @"cint16_T_re" x0
      )

instance ( ty ~ Int16_T
         ) => BG.HasField "cint16_T_re" (BG.Ptr Cint16_T) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"cint16_T_re")

instance HasCField.HasCField Cint16_T "cint16_T_re" where

  type CFieldType Cint16_T "cint16_T_re" = Int16_T

  offset# = \_ -> \_ -> 0

instance ( ty ~ Int16_T
         ) => BG.CompatHasField.HasField "cint16_T_im" Cint16_T ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Cint16_T {cint16_T_im = y1, cint16_T_re = BG.getField @"cint16_T_re" x0}
      , BG.getField @"cint16_T_im" x0
      )

instance ( ty ~ Int16_T
         ) => BG.HasField "cint16_T_im" (BG.Ptr Cint16_T) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"cint16_T_im")

instance HasCField.HasCField Cint16_T "cint16_T_im" where

  type CFieldType Cint16_T "cint16_T_im" = Int16_T

  offset# = \_ -> \_ -> 2

{-| __C declaration:__ @struct B@

    __defined at:__ @edge-cases\/spec_examples.h 19:8@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data B = B
  {}
  deriving stock (Eq, BG.Generic, Show)

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

deriving via Marshal.EquivStorable B instance BG.Storable B

{-| __C declaration:__ @struct A@

    __defined at:__ @edge-cases\/spec_examples.h 23:8@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data A = A
  { a_x :: BG.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/spec_examples.h 24:10@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_label :: BG.Ptr BG.CChar
    {- ^ __C declaration:__ @label@

         __defined at:__ @edge-cases\/spec_examples.h 25:9@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_samples :: CA.ConstantArray 128 BG.CChar
    {- ^ __C declaration:__ @samples@

         __defined at:__ @edge-cases\/spec_examples.h 26:8@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_b :: B
    {- ^ __C declaration:__ @b@

         __defined at:__ @edge-cases\/spec_examples.h 27:12@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_c :: BG.Ptr C
    {- ^ __C declaration:__ @c@

         __defined at:__ @edge-cases\/spec_examples.h 28:13@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize A where

  staticSizeOf = \_ -> (152 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> HasCField.readRaw (BG.Proxy @"a_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_label") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_samples") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_b") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_c") ptr0

instance Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_x2 a_label3 a_samples4 a_b5 a_c6 ->
               HasCField.writeRaw (BG.Proxy @"a_x") ptr0 a_x2
            >> HasCField.writeRaw (BG.Proxy @"a_label") ptr0 a_label3
            >> HasCField.writeRaw (BG.Proxy @"a_samples") ptr0 a_samples4
            >> HasCField.writeRaw (BG.Proxy @"a_b") ptr0 a_b5
            >> HasCField.writeRaw (BG.Proxy @"a_c") ptr0 a_c6

deriving via Marshal.EquivStorable A instance BG.Storable A

instance (ty ~ BG.CDouble) => BG.CompatHasField.HasField "a_x" A ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A { a_x = y1
            , a_label = BG.getField @"a_label" x0
            , a_samples = BG.getField @"a_samples" x0
            , a_b = BG.getField @"a_b" x0
            , a_c = BG.getField @"a_c" x0
            }
      , BG.getField @"a_x" x0
      )

instance (ty ~ BG.CDouble) => BG.HasField "a_x" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a_x")

instance HasCField.HasCField A "a_x" where

  type CFieldType A "a_x" = BG.CDouble

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.Ptr BG.CChar
         ) => BG.CompatHasField.HasField "a_label" A ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A { a_label = y1
            , a_x = BG.getField @"a_x" x0
            , a_samples = BG.getField @"a_samples" x0
            , a_b = BG.getField @"a_b" x0
            , a_c = BG.getField @"a_c" x0
            }
      , BG.getField @"a_label" x0
      )

instance ( ty ~ BG.Ptr BG.CChar
         ) => BG.HasField "a_label" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a_label")

instance HasCField.HasCField A "a_label" where

  type CFieldType A "a_label" = BG.Ptr BG.CChar

  offset# = \_ -> \_ -> 8

instance ( ty ~ CA.ConstantArray 128 BG.CChar
         ) => BG.CompatHasField.HasField "a_samples" A ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A { a_samples = y1
            , a_x = BG.getField @"a_x" x0
            , a_label = BG.getField @"a_label" x0
            , a_b = BG.getField @"a_b" x0
            , a_c = BG.getField @"a_c" x0
            }
      , BG.getField @"a_samples" x0
      )

instance ( ty ~ CA.ConstantArray 128 BG.CChar
         ) => BG.HasField "a_samples" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a_samples")

instance HasCField.HasCField A "a_samples" where

  type CFieldType A "a_samples" =
    CA.ConstantArray 128 BG.CChar

  offset# = \_ -> \_ -> 16

instance (ty ~ B) => BG.CompatHasField.HasField "a_b" A ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A { a_b = y1
            , a_x = BG.getField @"a_x" x0
            , a_label = BG.getField @"a_label" x0
            , a_samples = BG.getField @"a_samples" x0
            , a_c = BG.getField @"a_c" x0
            }
      , BG.getField @"a_b" x0
      )

instance (ty ~ B) => BG.HasField "a_b" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a_b")

instance HasCField.HasCField A "a_b" where

  type CFieldType A "a_b" = B

  offset# = \_ -> \_ -> 144

instance (ty ~ BG.Ptr C) => BG.CompatHasField.HasField "a_c" A ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A { a_c = y1
            , a_x = BG.getField @"a_x" x0
            , a_label = BG.getField @"a_label" x0
            , a_samples = BG.getField @"a_samples" x0
            , a_b = BG.getField @"a_b" x0
            }
      , BG.getField @"a_c" x0
      )

instance (ty ~ BG.Ptr C) => BG.HasField "a_c" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a_c")

instance HasCField.HasCField A "a_c" where

  type CFieldType A "a_c" = BG.Ptr C

  offset# = \_ -> \_ -> 144

{-| __C declaration:__ @struct C@

    __defined at:__ @edge-cases\/spec_examples.h 28:10@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data C
