{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @T1@

    __defined at:__ @types\/typedef_vs_macro.h:1:13@

    __exported by:__ @types\/typedef_vs_macro.h@
-}
newtype T1 = T1
  { un_T1 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType T1) "un_T1")
         ) => GHC.Records.HasField "un_T1" (Ptr.Ptr T1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_T1")

instance HsBindgen.Runtime.HasCField.HasCField T1 "un_T1" where

  type CFieldType T1 "un_T1" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T2@

    __defined at:__ @types\/typedef_vs_macro.h:2:14@

    __exported by:__ @types\/typedef_vs_macro.h@
-}
newtype T2 = T2
  { un_T2 :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType T2) "un_T2")
         ) => GHC.Records.HasField "un_T2" (Ptr.Ptr T2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_T2")

instance HsBindgen.Runtime.HasCField.HasCField T2 "un_T2" where

  type CFieldType T2 "un_T2" = FC.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @M1@

    __defined at:__ @types\/typedef_vs_macro.h:4:9@

    __exported by:__ @types\/typedef_vs_macro.h@
-}
newtype M1 = M1
  { un_M1 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @M2@

    __defined at:__ @types\/typedef_vs_macro.h:5:9@

    __exported by:__ @types\/typedef_vs_macro.h@
-}
newtype M2 = M2
  { un_M2 :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @M3@

    __defined at:__ @types\/typedef_vs_macro.h:6:9@

    __exported by:__ @types\/typedef_vs_macro.h@
-}
newtype M3 = M3
  { un_M3 :: Ptr.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @ExampleStruct@

    __defined at:__ @types\/typedef_vs_macro.h:8:8@

    __exported by:__ @types\/typedef_vs_macro.h@
-}
data ExampleStruct = ExampleStruct
  { exampleStruct_t1 :: T1
    {- ^ __C declaration:__ @t1@

         __defined at:__ @types\/typedef_vs_macro.h:9:6@

         __exported by:__ @types\/typedef_vs_macro.h@
    -}
  , exampleStruct_t2 :: T2
    {- ^ __C declaration:__ @t2@

         __defined at:__ @types\/typedef_vs_macro.h:10:6@

         __exported by:__ @types\/typedef_vs_macro.h@
    -}
  , exampleStruct_m1 :: M1
    {- ^ __C declaration:__ @m1@

         __defined at:__ @types\/typedef_vs_macro.h:11:6@

         __exported by:__ @types\/typedef_vs_macro.h@
    -}
  , exampleStruct_m2 :: M2
    {- ^ __C declaration:__ @m2@

         __defined at:__ @types\/typedef_vs_macro.h:12:6@

         __exported by:__ @types\/typedef_vs_macro.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable ExampleStruct where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExampleStruct
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"exampleStruct_t1") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"exampleStruct_t2") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"exampleStruct_m1") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"exampleStruct_m2") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExampleStruct
            exampleStruct_t12
            exampleStruct_t23
            exampleStruct_m14
            exampleStruct_m25 ->
                 HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"exampleStruct_t1") ptr0 exampleStruct_t12
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"exampleStruct_t2") ptr0 exampleStruct_t23
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"exampleStruct_m1") ptr0 exampleStruct_m14
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"exampleStruct_m2") ptr0 exampleStruct_m25

instance HsBindgen.Runtime.HasCField.HasCField ExampleStruct "exampleStruct_t1" where

  type CFieldType ExampleStruct "exampleStruct_t1" = T1

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExampleStruct) "exampleStruct_t1")
         ) => GHC.Records.HasField "exampleStruct_t1" (Ptr.Ptr ExampleStruct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"exampleStruct_t1")

instance HsBindgen.Runtime.HasCField.HasCField ExampleStruct "exampleStruct_t2" where

  type CFieldType ExampleStruct "exampleStruct_t2" = T2

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExampleStruct) "exampleStruct_t2")
         ) => GHC.Records.HasField "exampleStruct_t2" (Ptr.Ptr ExampleStruct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"exampleStruct_t2")

instance HsBindgen.Runtime.HasCField.HasCField ExampleStruct "exampleStruct_m1" where

  type CFieldType ExampleStruct "exampleStruct_m1" = M1

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExampleStruct) "exampleStruct_m1")
         ) => GHC.Records.HasField "exampleStruct_m1" (Ptr.Ptr ExampleStruct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"exampleStruct_m1")

instance HsBindgen.Runtime.HasCField.HasCField ExampleStruct "exampleStruct_m2" where

  type CFieldType ExampleStruct "exampleStruct_m2" = M2

  offset# = \_ -> \_ -> 12

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExampleStruct) "exampleStruct_m2")
         ) => GHC.Records.HasField "exampleStruct_m2" (Ptr.Ptr ExampleStruct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"exampleStruct_m2")

{-| __C declaration:__ @uint64_t@

    __defined at:__ @types\/typedef_vs_macro.h:15:9@

    __exported by:__ @types\/typedef_vs_macro.h@
-}
newtype Uint64_t = Uint64_t
  { un_Uint64_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @foo@

    __defined at:__ @types\/typedef_vs_macro.h:17:8@

    __exported by:__ @types\/typedef_vs_macro.h@
-}
data Foo = Foo
  { foo_a :: Ptr.Ptr Uint64_t
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/typedef_vs_macro.h:18:13@

         __exported by:__ @types\/typedef_vs_macro.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Foo where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"foo_a") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_a2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"foo_a") ptr0 foo_a2

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_a" where

  type CFieldType Foo "foo_a" = Ptr.Ptr Uint64_t

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_a")
         ) => GHC.Records.HasField "foo_a" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"foo_a")
