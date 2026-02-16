{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct foo@

    __defined at:__ @types\/nested\/nested_types.h 1:8@

    __exported by:__ @types\/nested\/nested_types.h@
-}
data Foo = Foo
  { foo_i :: RIP.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @types\/nested\/nested_types.h 2:9@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  , foo_c :: RIP.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/nested\/nested_types.h 3:10@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (RIP.Proxy @"foo_i") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"foo_c") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_i2 foo_c3 ->
               HasCField.writeRaw (RIP.Proxy @"foo_i") ptr0 foo_i2
            >> HasCField.writeRaw (RIP.Proxy @"foo_c") ptr0 foo_c3

deriving via Marshal.EquivStorable Foo instance RIP.Storable Foo

instance HasCField.HasCField Foo "foo_i" where

  type CFieldType Foo "foo_i" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "foo_i" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"foo_i")

instance HasCField.HasCField Foo "foo_c" where

  type CFieldType Foo "foo_c" = RIP.CChar

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "foo_c" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"foo_c")

{-| __C declaration:__ @struct bar@

    __defined at:__ @types\/nested\/nested_types.h 6:8@

    __exported by:__ @types\/nested\/nested_types.h@
-}
data Bar = Bar
  { bar_foo1 :: Foo
    {- ^ __C declaration:__ @foo1@

         __defined at:__ @types\/nested\/nested_types.h 7:16@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  , bar_foo2 :: Foo
    {- ^ __C declaration:__ @foo2@

         __defined at:__ @types\/nested\/nested_types.h 8:16@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCField.readRaw (RIP.Proxy @"bar_foo1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"bar_foo2") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_foo12 bar_foo23 ->
               HasCField.writeRaw (RIP.Proxy @"bar_foo1") ptr0 bar_foo12
            >> HasCField.writeRaw (RIP.Proxy @"bar_foo2") ptr0 bar_foo23

deriving via Marshal.EquivStorable Bar instance RIP.Storable Bar

instance HasCField.HasCField Bar "bar_foo1" where

  type CFieldType Bar "bar_foo1" = Foo

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Foo
         ) => RIP.HasField "bar_foo1" (RIP.Ptr Bar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bar_foo1")

instance HasCField.HasCField Bar "bar_foo2" where

  type CFieldType Bar "bar_foo2" = Foo

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) Foo
         ) => RIP.HasField "bar_foo2" (RIP.Ptr Bar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bar_foo2")

{-| __C declaration:__ @struct \@ex3_ex3_struct@

    __defined at:__ @types\/nested\/nested_types.h 12:5@

    __exported by:__ @types\/nested\/nested_types.h@
-}
data Ex3_ex3_struct = Ex3_ex3_struct
  { ex3_ex3_struct_ex3_a :: RIP.CInt
    {- ^ __C declaration:__ @ex3_a@

         __defined at:__ @types\/nested\/nested_types.h 13:13@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  , ex3_ex3_struct_ex3_b :: RIP.CChar
    {- ^ __C declaration:__ @ex3_b@

         __defined at:__ @types\/nested\/nested_types.h 14:14@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ex3_ex3_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ex3_ex3_struct where

  readRaw =
    \ptr0 ->
          pure Ex3_ex3_struct
      <*> HasCField.readRaw (RIP.Proxy @"ex3_ex3_struct_ex3_a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"ex3_ex3_struct_ex3_b") ptr0

instance Marshal.WriteRaw Ex3_ex3_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex3_ex3_struct ex3_ex3_struct_ex3_a2 ex3_ex3_struct_ex3_b3 ->
               HasCField.writeRaw (RIP.Proxy @"ex3_ex3_struct_ex3_a") ptr0 ex3_ex3_struct_ex3_a2
            >> HasCField.writeRaw (RIP.Proxy @"ex3_ex3_struct_ex3_b") ptr0 ex3_ex3_struct_ex3_b3

deriving via Marshal.EquivStorable Ex3_ex3_struct instance RIP.Storable Ex3_ex3_struct

instance HasCField.HasCField Ex3_ex3_struct "ex3_ex3_struct_ex3_a" where

  type CFieldType Ex3_ex3_struct "ex3_ex3_struct_ex3_a" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "ex3_ex3_struct_ex3_a" (RIP.Ptr Ex3_ex3_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ex3_ex3_struct_ex3_a")

instance HasCField.HasCField Ex3_ex3_struct "ex3_ex3_struct_ex3_b" where

  type CFieldType Ex3_ex3_struct "ex3_ex3_struct_ex3_b" =
    RIP.CChar

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "ex3_ex3_struct_ex3_b" (RIP.Ptr Ex3_ex3_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ex3_ex3_struct_ex3_b")

{-| __C declaration:__ @struct ex3@

    __defined at:__ @types\/nested\/nested_types.h 11:8@

    __exported by:__ @types\/nested\/nested_types.h@
-}
data Ex3 = Ex3
  { ex3_ex3_struct :: Ex3_ex3_struct
    {- ^ __C declaration:__ @ex3_struct@

         __defined at:__ @types\/nested\/nested_types.h 15:7@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  , ex3_ex3_c :: RIP.CFloat
    {- ^ __C declaration:__ @ex3_c@

         __defined at:__ @types\/nested\/nested_types.h 16:11@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ex3 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ex3 where

  readRaw =
    \ptr0 ->
          pure Ex3
      <*> HasCField.readRaw (RIP.Proxy @"ex3_ex3_struct") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"ex3_ex3_c") ptr0

instance Marshal.WriteRaw Ex3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex3 ex3_ex3_struct2 ex3_ex3_c3 ->
               HasCField.writeRaw (RIP.Proxy @"ex3_ex3_struct") ptr0 ex3_ex3_struct2
            >> HasCField.writeRaw (RIP.Proxy @"ex3_ex3_c") ptr0 ex3_ex3_c3

deriving via Marshal.EquivStorable Ex3 instance RIP.Storable Ex3

instance HasCField.HasCField Ex3 "ex3_ex3_struct" where

  type CFieldType Ex3 "ex3_ex3_struct" = Ex3_ex3_struct

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Ex3_ex3_struct
         ) => RIP.HasField "ex3_ex3_struct" (RIP.Ptr Ex3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ex3_ex3_struct")

instance HasCField.HasCField Ex3 "ex3_ex3_c" where

  type CFieldType Ex3 "ex3_ex3_c" = RIP.CFloat

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CFloat
         ) => RIP.HasField "ex3_ex3_c" (RIP.Ptr Ex3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"ex3_ex3_c")

{-| __C declaration:__ @struct ex4_odd@

    __defined at:__ @types\/nested\/nested_types.h 22:8@

    __exported by:__ @types\/nested\/nested_types.h@
-}
data Ex4_odd = Ex4_odd
  { ex4_odd_value :: RIP.CInt
    {- ^ __C declaration:__ @value@

         __defined at:__ @types\/nested\/nested_types.h 23:9@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  , ex4_odd_next :: RIP.Ptr Ex4_even
    {- ^ __C declaration:__ @next@

         __defined at:__ @types\/nested\/nested_types.h 27:8@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ex4_odd where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ex4_odd where

  readRaw =
    \ptr0 ->
          pure Ex4_odd
      <*> HasCField.readRaw (RIP.Proxy @"ex4_odd_value") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"ex4_odd_next") ptr0

instance Marshal.WriteRaw Ex4_odd where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex4_odd ex4_odd_value2 ex4_odd_next3 ->
               HasCField.writeRaw (RIP.Proxy @"ex4_odd_value") ptr0 ex4_odd_value2
            >> HasCField.writeRaw (RIP.Proxy @"ex4_odd_next") ptr0 ex4_odd_next3

deriving via Marshal.EquivStorable Ex4_odd instance RIP.Storable Ex4_odd

instance HasCField.HasCField Ex4_odd "ex4_odd_value" where

  type CFieldType Ex4_odd "ex4_odd_value" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "ex4_odd_value" (RIP.Ptr Ex4_odd) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ex4_odd_value")

instance HasCField.HasCField Ex4_odd "ex4_odd_next" where

  type CFieldType Ex4_odd "ex4_odd_next" =
    RIP.Ptr Ex4_even

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr Ex4_even)
         ) => RIP.HasField "ex4_odd_next" (RIP.Ptr Ex4_odd) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ex4_odd_next")

{-| __C declaration:__ @struct ex4_even@

    __defined at:__ @types\/nested\/nested_types.h 24:12@

    __exported by:__ @types\/nested\/nested_types.h@
-}
data Ex4_even = Ex4_even
  { ex4_even_value :: RIP.CDouble
    {- ^ __C declaration:__ @value@

         __defined at:__ @types\/nested\/nested_types.h 25:16@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  , ex4_even_next :: RIP.Ptr Ex4_odd
    {- ^ __C declaration:__ @next@

         __defined at:__ @types\/nested\/nested_types.h 26:25@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Ex4_even where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ex4_even where

  readRaw =
    \ptr0 ->
          pure Ex4_even
      <*> HasCField.readRaw (RIP.Proxy @"ex4_even_value") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"ex4_even_next") ptr0

instance Marshal.WriteRaw Ex4_even where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex4_even ex4_even_value2 ex4_even_next3 ->
               HasCField.writeRaw (RIP.Proxy @"ex4_even_value") ptr0 ex4_even_value2
            >> HasCField.writeRaw (RIP.Proxy @"ex4_even_next") ptr0 ex4_even_next3

deriving via Marshal.EquivStorable Ex4_even instance RIP.Storable Ex4_even

instance HasCField.HasCField Ex4_even "ex4_even_value" where

  type CFieldType Ex4_even "ex4_even_value" =
    RIP.CDouble

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "ex4_even_value" (RIP.Ptr Ex4_even) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ex4_even_value")

instance HasCField.HasCField Ex4_even "ex4_even_next" where

  type CFieldType Ex4_even "ex4_even_next" =
    RIP.Ptr Ex4_odd

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr Ex4_odd)
         ) => RIP.HasField "ex4_even_next" (RIP.Ptr Ex4_even) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"ex4_even_next")
