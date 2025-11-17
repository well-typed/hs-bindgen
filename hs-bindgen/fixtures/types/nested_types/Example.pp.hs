{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @foo@

    __defined at:__ @types\/nested_types.h:1:8@

    __exported by:__ @types\/nested_types.h@
-}
data Foo = Foo
  { foo_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @types\/nested_types.h:2:9@

         __exported by:__ @types\/nested_types.h@
    -}
  , foo_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/nested_types.h:3:10@

         __exported by:__ @types\/nested_types.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Foo where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"foo_i") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"foo_c") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_i2 foo_c3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"foo_i") ptr0 foo_i2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"foo_c") ptr0 foo_c3

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_i" where

  type CFieldType Foo "foo_i" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_i")
         ) => GHC.Records.HasField "foo_i" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"foo_i")

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_c" where

  type CFieldType Foo "foo_c" = FC.CChar

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_c")
         ) => GHC.Records.HasField "foo_c" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"foo_c")

{-| __C declaration:__ @bar@

    __defined at:__ @types\/nested_types.h:6:8@

    __exported by:__ @types\/nested_types.h@
-}
data Bar = Bar
  { bar_foo1 :: Foo
    {- ^ __C declaration:__ @foo1@

         __defined at:__ @types\/nested_types.h:7:16@

         __exported by:__ @types\/nested_types.h@
    -}
  , bar_foo2 :: Foo
    {- ^ __C declaration:__ @foo2@

         __defined at:__ @types\/nested_types.h:8:16@

         __exported by:__ @types\/nested_types.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bar where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bar_foo1") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bar_foo2") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_foo12 bar_foo23 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bar_foo1") ptr0 bar_foo12
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bar_foo2") ptr0 bar_foo23

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_foo1" where

  type CFieldType Bar "bar_foo1" = Foo

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_foo1")
         ) => GHC.Records.HasField "bar_foo1" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bar_foo1")

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_foo2" where

  type CFieldType Bar "bar_foo2" = Foo

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_foo2")
         ) => GHC.Records.HasField "bar_foo2" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bar_foo2")

{-| __defined at:__ @types\/nested_types.h:12:5@

    __exported by:__ @types\/nested_types.h@
-}
data Ex3_ex3_struct = Ex3_ex3_struct
  { ex3_ex3_struct_ex3_a :: FC.CInt
    {- ^ __C declaration:__ @ex3_a@

         __defined at:__ @types\/nested_types.h:13:13@

         __exported by:__ @types\/nested_types.h@
    -}
  , ex3_ex3_struct_ex3_b :: FC.CChar
    {- ^ __C declaration:__ @ex3_b@

         __defined at:__ @types\/nested_types.h:14:14@

         __exported by:__ @types\/nested_types.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Ex3_ex3_struct where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Ex3_ex3_struct
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"ex3_ex3_struct_ex3_a") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"ex3_ex3_struct_ex3_b") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex3_ex3_struct ex3_ex3_struct_ex3_a2 ex3_ex3_struct_ex3_b3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"ex3_ex3_struct_ex3_a") ptr0 ex3_ex3_struct_ex3_a2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"ex3_ex3_struct_ex3_b") ptr0 ex3_ex3_struct_ex3_b3

instance HsBindgen.Runtime.HasCField.HasCField Ex3_ex3_struct "ex3_ex3_struct_ex3_a" where

  type CFieldType Ex3_ex3_struct "ex3_ex3_struct_ex3_a" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Ex3_ex3_struct) "ex3_ex3_struct_ex3_a")
         ) => GHC.Records.HasField "ex3_ex3_struct_ex3_a" (Ptr.Ptr Ex3_ex3_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"ex3_ex3_struct_ex3_a")

instance HsBindgen.Runtime.HasCField.HasCField Ex3_ex3_struct "ex3_ex3_struct_ex3_b" where

  type CFieldType Ex3_ex3_struct "ex3_ex3_struct_ex3_b" =
    FC.CChar

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Ex3_ex3_struct) "ex3_ex3_struct_ex3_b")
         ) => GHC.Records.HasField "ex3_ex3_struct_ex3_b" (Ptr.Ptr Ex3_ex3_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"ex3_ex3_struct_ex3_b")

{-| __C declaration:__ @ex3@

    __defined at:__ @types\/nested_types.h:11:8@

    __exported by:__ @types\/nested_types.h@
-}
data Ex3 = Ex3
  { ex3_ex3_struct :: Ex3_ex3_struct
    {- ^ __C declaration:__ @ex3_struct@

         __defined at:__ @types\/nested_types.h:15:7@

         __exported by:__ @types\/nested_types.h@
    -}
  , ex3_ex3_c :: FC.CFloat
    {- ^ __C declaration:__ @ex3_c@

         __defined at:__ @types\/nested_types.h:16:11@

         __exported by:__ @types\/nested_types.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Ex3 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Ex3
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"ex3_ex3_struct") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"ex3_ex3_c") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex3 ex3_ex3_struct2 ex3_ex3_c3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"ex3_ex3_struct") ptr0 ex3_ex3_struct2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"ex3_ex3_c") ptr0 ex3_ex3_c3

instance HsBindgen.Runtime.HasCField.HasCField Ex3 "ex3_ex3_struct" where

  type CFieldType Ex3 "ex3_ex3_struct" = Ex3_ex3_struct

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Ex3) "ex3_ex3_struct")
         ) => GHC.Records.HasField "ex3_ex3_struct" (Ptr.Ptr Ex3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"ex3_ex3_struct")

instance HsBindgen.Runtime.HasCField.HasCField Ex3 "ex3_ex3_c" where

  type CFieldType Ex3 "ex3_ex3_c" = FC.CFloat

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Ex3) "ex3_ex3_c")
         ) => GHC.Records.HasField "ex3_ex3_c" (Ptr.Ptr Ex3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"ex3_ex3_c")

{-| __C declaration:__ @ex4_odd@

    __defined at:__ @types\/nested_types.h:22:8@

    __exported by:__ @types\/nested_types.h@
-}
data Ex4_odd = Ex4_odd
  { ex4_odd_value :: FC.CInt
    {- ^ __C declaration:__ @value@

         __defined at:__ @types\/nested_types.h:23:9@

         __exported by:__ @types\/nested_types.h@
    -}
  , ex4_odd_next :: Ptr.Ptr Ex4_even
    {- ^ __C declaration:__ @next@

         __defined at:__ @types\/nested_types.h:27:8@

         __exported by:__ @types\/nested_types.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Ex4_odd where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Ex4_odd
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"ex4_odd_value") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"ex4_odd_next") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex4_odd ex4_odd_value2 ex4_odd_next3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"ex4_odd_value") ptr0 ex4_odd_value2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"ex4_odd_next") ptr0 ex4_odd_next3

instance HsBindgen.Runtime.HasCField.HasCField Ex4_odd "ex4_odd_value" where

  type CFieldType Ex4_odd "ex4_odd_value" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Ex4_odd) "ex4_odd_value")
         ) => GHC.Records.HasField "ex4_odd_value" (Ptr.Ptr Ex4_odd) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"ex4_odd_value")

instance HsBindgen.Runtime.HasCField.HasCField Ex4_odd "ex4_odd_next" where

  type CFieldType Ex4_odd "ex4_odd_next" =
    Ptr.Ptr Ex4_even

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Ex4_odd) "ex4_odd_next")
         ) => GHC.Records.HasField "ex4_odd_next" (Ptr.Ptr Ex4_odd) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"ex4_odd_next")

{-| __C declaration:__ @ex4_even@

    __defined at:__ @types\/nested_types.h:24:12@

    __exported by:__ @types\/nested_types.h@
-}
data Ex4_even = Ex4_even
  { ex4_even_value :: FC.CDouble
    {- ^ __C declaration:__ @value@

         __defined at:__ @types\/nested_types.h:25:16@

         __exported by:__ @types\/nested_types.h@
    -}
  , ex4_even_next :: Ptr.Ptr Ex4_odd
    {- ^ __C declaration:__ @next@

         __defined at:__ @types\/nested_types.h:26:25@

         __exported by:__ @types\/nested_types.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Ex4_even where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Ex4_even
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"ex4_even_value") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"ex4_even_next") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex4_even ex4_even_value2 ex4_even_next3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"ex4_even_value") ptr0 ex4_even_value2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"ex4_even_next") ptr0 ex4_even_next3

instance HsBindgen.Runtime.HasCField.HasCField Ex4_even "ex4_even_value" where

  type CFieldType Ex4_even "ex4_even_value" =
    FC.CDouble

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Ex4_even) "ex4_even_value")
         ) => GHC.Records.HasField "ex4_even_value" (Ptr.Ptr Ex4_even) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"ex4_even_value")

instance HsBindgen.Runtime.HasCField.HasCField Ex4_even "ex4_even_next" where

  type CFieldType Ex4_even "ex4_even_next" =
    Ptr.Ptr Ex4_odd

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Ex4_even) "ex4_even_next")
         ) => GHC.Records.HasField "ex4_even_next" (Ptr.Ptr Ex4_even) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"ex4_even_next")
