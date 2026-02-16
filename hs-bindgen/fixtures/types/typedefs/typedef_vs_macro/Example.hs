{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @T1@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 1:13@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype T1 = T1
  { unwrapT1 :: FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
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

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "unwrapT1" (Ptr.Ptr T1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapT1")

instance HsBindgen.Runtime.HasCField.HasCField T1 "unwrapT1" where

  type CFieldType T1 "unwrapT1" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T2@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 2:14@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype T2 = T2
  { unwrapT2 :: FC.CChar
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
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

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "unwrapT2" (Ptr.Ptr T2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapT2")

instance HsBindgen.Runtime.HasCField.HasCField T2 "unwrapT2" where

  type CFieldType T2 "unwrapT2" = FC.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @M1@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 4:9@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype M1 = M1
  { unwrapM1 :: FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
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

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "unwrapM1" (Ptr.Ptr M1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapM1")

instance HsBindgen.Runtime.HasCField.HasCField M1 "unwrapM1" where

  type CFieldType M1 "unwrapM1" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @M2@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 5:9@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype M2 = M2
  { unwrapM2 :: FC.CChar
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
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

instance ( TyEq ty FC.CChar
         ) => GHC.Records.HasField "unwrapM2" (Ptr.Ptr M2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapM2")

instance HsBindgen.Runtime.HasCField.HasCField M2 "unwrapM2" where

  type CFieldType M2 "unwrapM2" = FC.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @M3@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 6:9@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype M3 = M3
  { unwrapM3 :: Ptr.Ptr FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.Ptr FC.CInt)
         ) => GHC.Records.HasField "unwrapM3" (Ptr.Ptr M3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapM3")

instance HsBindgen.Runtime.HasCField.HasCField M3 "unwrapM3" where

  type CFieldType M3 "unwrapM3" = Ptr.Ptr FC.CInt

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
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize ExampleStruct where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw ExampleStruct where

  readRaw =
    \ptr0 ->
          pure ExampleStruct
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"exampleStruct_t1") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"exampleStruct_t2") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"exampleStruct_m1") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"exampleStruct_m2") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw ExampleStruct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExampleStruct
            exampleStruct_t12
            exampleStruct_t23
            exampleStruct_m14
            exampleStruct_m25 ->
                 HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"exampleStruct_t1") ptr0 exampleStruct_t12
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"exampleStruct_t2") ptr0 exampleStruct_t23
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"exampleStruct_m1") ptr0 exampleStruct_m14
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"exampleStruct_m2") ptr0 exampleStruct_m25

deriving via HsBindgen.Runtime.Marshal.EquivStorable ExampleStruct instance F.Storable ExampleStruct

instance HsBindgen.Runtime.HasCField.HasCField ExampleStruct "exampleStruct_t1" where

  type CFieldType ExampleStruct "exampleStruct_t1" = T1

  offset# = \_ -> \_ -> 0

instance ( TyEq ty T1
         ) => GHC.Records.HasField "exampleStruct_t1" (Ptr.Ptr ExampleStruct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"exampleStruct_t1")

instance HsBindgen.Runtime.HasCField.HasCField ExampleStruct "exampleStruct_t2" where

  type CFieldType ExampleStruct "exampleStruct_t2" = T2

  offset# = \_ -> \_ -> 4

instance ( TyEq ty T2
         ) => GHC.Records.HasField "exampleStruct_t2" (Ptr.Ptr ExampleStruct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"exampleStruct_t2")

instance HsBindgen.Runtime.HasCField.HasCField ExampleStruct "exampleStruct_m1" where

  type CFieldType ExampleStruct "exampleStruct_m1" = M1

  offset# = \_ -> \_ -> 8

instance ( TyEq ty M1
         ) => GHC.Records.HasField "exampleStruct_m1" (Ptr.Ptr ExampleStruct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"exampleStruct_m1")

instance HsBindgen.Runtime.HasCField.HasCField ExampleStruct "exampleStruct_m2" where

  type CFieldType ExampleStruct "exampleStruct_m2" = M2

  offset# = \_ -> \_ -> 12

instance ( TyEq ty M2
         ) => GHC.Records.HasField "exampleStruct_m2" (Ptr.Ptr ExampleStruct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"exampleStruct_m2")

{-| __C declaration:__ @uint64_t@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 15:9@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
newtype Uint64_t = Uint64_t
  { unwrapUint64_t :: FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
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

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "unwrapUint64_t" (Ptr.Ptr Uint64_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapUint64_t")

instance HsBindgen.Runtime.HasCField.HasCField Uint64_t "unwrapUint64_t" where

  type CFieldType Uint64_t "unwrapUint64_t" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct foo@

    __defined at:__ @types\/typedefs\/typedef_vs_macro.h 17:8@

    __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
-}
data Foo = Foo
  { foo_a :: Ptr.Ptr Uint64_t
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/typedefs\/typedef_vs_macro.h 18:13@

         __exported by:__ @types\/typedefs\/typedef_vs_macro.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"foo_a") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_a2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"foo_a") ptr0 foo_a2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Foo instance F.Storable Foo

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_a" where

  type CFieldType Foo "foo_a" = Ptr.Ptr Uint64_t

  offset# = \_ -> \_ -> 0

instance ( TyEq ty (Ptr.Ptr Uint64_t)
         ) => GHC.Records.HasField "foo_a" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"foo_a")
