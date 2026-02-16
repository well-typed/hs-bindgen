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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @T1@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 1:13@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
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

{-| __C declaration:__ @T2@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 2:14@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype T2 = T2
  { unwrapT2 :: RIP.CChar
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

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "unwrapT2" (RIP.Ptr T2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT2")

instance HasCField.HasCField T2 "unwrapT2" where

  type CFieldType T2 "unwrapT2" = RIP.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @M1@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 4:9@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype M1 = M1
  { unwrapM1 :: RIP.CInt
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
         ) => RIP.HasField "unwrapM1" (RIP.Ptr M1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapM1")

instance HasCField.HasCField M1 "unwrapM1" where

  type CFieldType M1 "unwrapM1" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @M2@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 5:9@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype M2 = M2
  { unwrapM2 :: RIP.CChar
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

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "unwrapM2" (RIP.Ptr M2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapM2")

instance HasCField.HasCField M2 "unwrapM2" where

  type CFieldType M2 "unwrapM2" = RIP.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @M3@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 6:9@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype M3 = M3
  { unwrapM3 :: RIP.Ptr RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr RIP.CInt)
         ) => RIP.HasField "unwrapM3" (RIP.Ptr M3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapM3")

instance HasCField.HasCField M3 "unwrapM3" where

  type CFieldType M3 "unwrapM3" = RIP.Ptr RIP.CInt

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize ExampleStruct where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw ExampleStruct where

  readRaw =
    \ptr0 ->
          pure ExampleStruct
      <*> HasCField.readRaw (RIP.Proxy @"exampleStruct_t1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"exampleStruct_t2") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"exampleStruct_m1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"exampleStruct_m2") ptr0

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
                 HasCField.writeRaw (RIP.Proxy @"exampleStruct_t1") ptr0 exampleStruct_t12
              >> HasCField.writeRaw (RIP.Proxy @"exampleStruct_t2") ptr0 exampleStruct_t23
              >> HasCField.writeRaw (RIP.Proxy @"exampleStruct_m1") ptr0 exampleStruct_m14
              >> HasCField.writeRaw (RIP.Proxy @"exampleStruct_m2") ptr0 exampleStruct_m25

deriving via Marshal.EquivStorable ExampleStruct instance RIP.Storable ExampleStruct

instance HasCField.HasCField ExampleStruct "exampleStruct_t1" where

  type CFieldType ExampleStruct "exampleStruct_t1" = T1

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) T1
         ) => RIP.HasField "exampleStruct_t1" (RIP.Ptr ExampleStruct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"exampleStruct_t1")

instance HasCField.HasCField ExampleStruct "exampleStruct_t2" where

  type CFieldType ExampleStruct "exampleStruct_t2" = T2

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) T2
         ) => RIP.HasField "exampleStruct_t2" (RIP.Ptr ExampleStruct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"exampleStruct_t2")

instance HasCField.HasCField ExampleStruct "exampleStruct_m1" where

  type CFieldType ExampleStruct "exampleStruct_m1" = M1

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) M1
         ) => RIP.HasField "exampleStruct_m1" (RIP.Ptr ExampleStruct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"exampleStruct_m1")

instance HasCField.HasCField ExampleStruct "exampleStruct_m2" where

  type CFieldType ExampleStruct "exampleStruct_m2" = M2

  offset# = \_ -> \_ -> 12

instance ( ((~) ty) M2
         ) => RIP.HasField "exampleStruct_m2" (RIP.Ptr ExampleStruct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"exampleStruct_m2")

{-| __C declaration:__ @uint64_t@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 15:9@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype Uint64_t = Uint64_t
  { unwrapUint64_t :: RIP.CInt
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
         ) => RIP.HasField "unwrapUint64_t" (RIP.Ptr Uint64_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapUint64_t")

instance HasCField.HasCField Uint64_t "unwrapUint64_t" where

  type CFieldType Uint64_t "unwrapUint64_t" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct foo@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 17:8@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
data Foo = Foo
  { foo_a :: RIP.Ptr Uint64_t
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/typedefs\/typedef_vs_macro.h 18:13@

         __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (RIP.Proxy @"foo_a") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_a2 ->
            HasCField.writeRaw (RIP.Proxy @"foo_a") ptr0 foo_a2

deriving via Marshal.EquivStorable Foo instance RIP.Storable Foo

instance HasCField.HasCField Foo "foo_a" where

  type CFieldType Foo "foo_a" = RIP.Ptr Uint64_t

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.Ptr Uint64_t)
         ) => RIP.HasField "foo_a" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"foo_a")
