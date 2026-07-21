{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
    ( Example.T1(..)
    , Example.T2(..)
    , Example.M1(..)
    , Example.M2(..)
    , Example.M3(..)
    , Example.ExampleStruct(..)
    , Example.Uint64_t(..)
    , Example.Foo(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @T1@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 1:13@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
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

{-| __C declaration:__ @T2@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 2:14@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype T2 = T2
  { unwrapT2 :: BG.CChar
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

instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "unwrapT2" T2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         T2 {unwrapT2 = y1}, BG.getField @"unwrapT2" x0)

instance (ty ~ BG.CChar) => BG.HasField "unwrapT2" (BG.Ptr T2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT2")

instance HasCField.HasCField T2 "unwrapT2" where

  type CFieldType T2 "unwrapT2" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro M1@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 4:9@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype M1 = M1
  { unwrapM1 :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapM1" M1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         M1 {unwrapM1 = y1}, BG.getField @"unwrapM1" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapM1" (BG.Ptr M1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapM1")

instance HasCField.HasCField M1 "unwrapM1" where

  type CFieldType M1 "unwrapM1" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro M2@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 5:9@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype M2 = M2
  { unwrapM2 :: BG.CChar
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

instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "unwrapM2" M2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         M2 {unwrapM2 = y1}, BG.getField @"unwrapM2" x0)

instance (ty ~ BG.CChar) => BG.HasField "unwrapM2" (BG.Ptr M2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapM2")

instance HasCField.HasCField M2 "unwrapM2" where

  type CFieldType M2 "unwrapM2" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro M3@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 6:9@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype M3 = M3
  { unwrapM3 :: BG.Ptr BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.CompatHasField.HasField "unwrapM3" M3 ty where

  hasField =
    \x0 ->
      (\y1 ->
         M3 {unwrapM3 = y1}, BG.getField @"unwrapM3" x0)

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "unwrapM3" (BG.Ptr M3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapM3")

instance HasCField.HasCField M3 "unwrapM3" where

  type CFieldType M3 "unwrapM3" = BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct ExampleStruct@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 8:8@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
data ExampleStruct = ExampleStruct
  { exampleStruct_t1 :: T1
    {- ^ __C declaration:__ @t1@

         __defined at:__ @types\/typedefs\/typedef_vs_macro.h 9:6@

         __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
    -}
  , exampleStruct_t2 :: T2
    {- ^ __C declaration:__ @t2@

         __defined at:__ @types\/typedefs\/typedef_vs_macro.h 10:6@

         __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
    -}
  , exampleStruct_m1 :: M1
    {- ^ __C declaration:__ @m1@

         __defined at:__ @types\/typedefs\/typedef_vs_macro.h 11:6@

         __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
    -}
  , exampleStruct_m2 :: M2
    {- ^ __C declaration:__ @m2@

         __defined at:__ @types\/typedefs\/typedef_vs_macro.h 12:6@

         __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize ExampleStruct where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw ExampleStruct where

  readRaw =
    \ptr0 ->
          pure ExampleStruct
      <*> HasCField.readRaw (BG.Proxy @"exampleStruct_t1") ptr0
      <*> HasCField.readRaw (BG.Proxy @"exampleStruct_t2") ptr0
      <*> HasCField.readRaw (BG.Proxy @"exampleStruct_m1") ptr0
      <*> HasCField.readRaw (BG.Proxy @"exampleStruct_m2") ptr0

instance Marshal.WriteRaw ExampleStruct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExampleStruct
            exampleStruct_t12
            exampleStruct_t23
            exampleStruct_m14
            exampleStruct_m25 ->
                 HasCField.writeRaw (BG.Proxy @"exampleStruct_t1") ptr0 exampleStruct_t12
              >> HasCField.writeRaw (BG.Proxy @"exampleStruct_t2") ptr0 exampleStruct_t23
              >> HasCField.writeRaw (BG.Proxy @"exampleStruct_m1") ptr0 exampleStruct_m14
              >> HasCField.writeRaw (BG.Proxy @"exampleStruct_m2") ptr0 exampleStruct_m25

deriving via Marshal.EquivStorable ExampleStruct instance BG.Storable ExampleStruct

{-| __C declaration:__ @t1@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 9:6@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
instance ( ty ~ T1
         ) => BG.CompatHasField.HasField "exampleStruct_t1" ExampleStruct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          ExampleStruct { exampleStruct_t1 = y1
                        , exampleStruct_t2 = BG.getField @"exampleStruct_t2" x0
                        , exampleStruct_m1 = BG.getField @"exampleStruct_m1" x0
                        , exampleStruct_m2 = BG.getField @"exampleStruct_m2" x0
                        }
      , BG.getField @"exampleStruct_t1" x0
      )

instance ( ty ~ T1
         ) => BG.HasField "exampleStruct_t1" (BG.Ptr ExampleStruct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"exampleStruct_t1")

instance HasCField.HasCField ExampleStruct "exampleStruct_t1" where

  type CFieldType ExampleStruct "exampleStruct_t1" = T1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @t2@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 10:6@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
instance ( ty ~ T2
         ) => BG.CompatHasField.HasField "exampleStruct_t2" ExampleStruct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          ExampleStruct { exampleStruct_t2 = y1
                        , exampleStruct_t1 = BG.getField @"exampleStruct_t1" x0
                        , exampleStruct_m1 = BG.getField @"exampleStruct_m1" x0
                        , exampleStruct_m2 = BG.getField @"exampleStruct_m2" x0
                        }
      , BG.getField @"exampleStruct_t2" x0
      )

instance ( ty ~ T2
         ) => BG.HasField "exampleStruct_t2" (BG.Ptr ExampleStruct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"exampleStruct_t2")

instance HasCField.HasCField ExampleStruct "exampleStruct_t2" where

  type CFieldType ExampleStruct "exampleStruct_t2" = T2

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @m1@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 11:6@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
instance ( ty ~ M1
         ) => BG.CompatHasField.HasField "exampleStruct_m1" ExampleStruct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          ExampleStruct { exampleStruct_m1 = y1
                        , exampleStruct_t1 = BG.getField @"exampleStruct_t1" x0
                        , exampleStruct_t2 = BG.getField @"exampleStruct_t2" x0
                        , exampleStruct_m2 = BG.getField @"exampleStruct_m2" x0
                        }
      , BG.getField @"exampleStruct_m1" x0
      )

instance ( ty ~ M1
         ) => BG.HasField "exampleStruct_m1" (BG.Ptr ExampleStruct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"exampleStruct_m1")

instance HasCField.HasCField ExampleStruct "exampleStruct_m1" where

  type CFieldType ExampleStruct "exampleStruct_m1" = M1

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @m2@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 12:6@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
instance ( ty ~ M2
         ) => BG.CompatHasField.HasField "exampleStruct_m2" ExampleStruct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          ExampleStruct { exampleStruct_m2 = y1
                        , exampleStruct_t1 = BG.getField @"exampleStruct_t1" x0
                        , exampleStruct_t2 = BG.getField @"exampleStruct_t2" x0
                        , exampleStruct_m1 = BG.getField @"exampleStruct_m1" x0
                        }
      , BG.getField @"exampleStruct_m2" x0
      )

instance ( ty ~ M2
         ) => BG.HasField "exampleStruct_m2" (BG.Ptr ExampleStruct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"exampleStruct_m2")

instance HasCField.HasCField ExampleStruct "exampleStruct_m2" where

  type CFieldType ExampleStruct "exampleStruct_m2" = M2

  offset# = \_ -> \_ -> 12

{-| __C declaration:__ @macro uint64_t@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 15:9@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype Uint64_t = Uint64_t
  { unwrapUint64_t :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapUint64_t" Uint64_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         Uint64_t {unwrapUint64_t = y1}, BG.getField @"unwrapUint64_t" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapUint64_t" (BG.Ptr Uint64_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapUint64_t")

instance HasCField.HasCField Uint64_t "unwrapUint64_t" where

  type CFieldType Uint64_t "unwrapUint64_t" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct foo@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 17:8@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
data Foo = Foo
  { foo_a :: BG.Ptr Uint64_t
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/typedefs\/typedef_vs_macro.h 18:13@

         __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (BG.Proxy @"foo_a") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_a2 ->
            HasCField.writeRaw (BG.Proxy @"foo_a") ptr0 foo_a2

deriving via Marshal.EquivStorable Foo instance BG.Storable Foo

{-| __C declaration:__ @a@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 18:13@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
instance ( ty ~ BG.Ptr Uint64_t
         ) => BG.CompatHasField.HasField "foo_a" Foo ty where

  hasField =
    \x0 ->
      (\y1 -> Foo {foo_a = y1}, BG.getField @"foo_a" x0)

instance ( ty ~ BG.Ptr Uint64_t
         ) => BG.HasField "foo_a" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_a")

instance HasCField.HasCField Foo "foo_a" where

  type CFieldType Foo "foo_a" = BG.Ptr Uint64_t

  offset# = \_ -> \_ -> 0
