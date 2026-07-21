{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Foo(..)
    , Example.Bar(..)
    , Example.Ex3_ex3_struct(..)
    , Example.Ex3(..)
    , Example.Ex4_odd(..)
    , Example.Ex4_even(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct foo@

    __defined at:__ @types\/nested\/nested_types.h 1:8@

    __exported by:__ @types\/nested\/nested_types.h@
-}
data Foo = Foo
  { foo_i :: BG.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @types\/nested\/nested_types.h 2:9@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  , foo_c :: BG.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/nested\/nested_types.h 3:10@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (BG.Proxy @"foo_i") ptr0
      <*> HasCField.readRaw (BG.Proxy @"foo_c") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_i2 foo_c3 ->
               HasCField.writeRaw (BG.Proxy @"foo_i") ptr0 foo_i2
            >> HasCField.writeRaw (BG.Proxy @"foo_c") ptr0 foo_c3

deriving via Marshal.EquivStorable Foo instance BG.Storable Foo

{-| __C declaration:__ @i@

    __defined at:__ @types\/nested\/nested_types.h 2:9@

    __exported by:__ @types\/nested\/nested_types.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_i" Foo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo {foo_i = y1, foo_c = BG.getField @"foo_c" x0}
      , BG.getField @"foo_i" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "foo_i" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_i")

instance HasCField.HasCField Foo "foo_i" where

  type CFieldType Foo "foo_i" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @c@

    __defined at:__ @types\/nested\/nested_types.h 3:10@

    __exported by:__ @types\/nested\/nested_types.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "foo_c" Foo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo {foo_c = y1, foo_i = BG.getField @"foo_i" x0}
      , BG.getField @"foo_c" x0
      )

instance (ty ~ BG.CChar) => BG.HasField "foo_c" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_c")

instance HasCField.HasCField Foo "foo_c" where

  type CFieldType Foo "foo_c" = BG.CChar

  offset# = \_ -> \_ -> 4

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
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCField.readRaw (BG.Proxy @"bar_foo1") ptr0
      <*> HasCField.readRaw (BG.Proxy @"bar_foo2") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_foo12 bar_foo23 ->
               HasCField.writeRaw (BG.Proxy @"bar_foo1") ptr0 bar_foo12
            >> HasCField.writeRaw (BG.Proxy @"bar_foo2") ptr0 bar_foo23

deriving via Marshal.EquivStorable Bar instance BG.Storable Bar

{-| __C declaration:__ @foo1@

    __defined at:__ @types\/nested\/nested_types.h 7:16@

    __exported by:__ @types\/nested\/nested_types.h@
-}
instance (ty ~ Foo) => BG.CompatHasField.HasField "bar_foo1" Bar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar {bar_foo1 = y1, bar_foo2 = BG.getField @"bar_foo2" x0}
      , BG.getField @"bar_foo1" x0
      )

instance (ty ~ Foo) => BG.HasField "bar_foo1" (BG.Ptr Bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bar_foo1")

instance HasCField.HasCField Bar "bar_foo1" where

  type CFieldType Bar "bar_foo1" = Foo

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @foo2@

    __defined at:__ @types\/nested\/nested_types.h 8:16@

    __exported by:__ @types\/nested\/nested_types.h@
-}
instance (ty ~ Foo) => BG.CompatHasField.HasField "bar_foo2" Bar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar {bar_foo2 = y1, bar_foo1 = BG.getField @"bar_foo1" x0}
      , BG.getField @"bar_foo2" x0
      )

instance (ty ~ Foo) => BG.HasField "bar_foo2" (BG.Ptr Bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bar_foo2")

instance HasCField.HasCField Bar "bar_foo2" where

  type CFieldType Bar "bar_foo2" = Foo

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct \@ex3_ex3_struct@

    __defined at:__ @types\/nested\/nested_types.h 12:5@

    __exported by:__ @types\/nested\/nested_types.h@
-}
data Ex3_ex3_struct = Ex3_ex3_struct
  { ex3_ex3_struct_ex3_a :: BG.CInt
    {- ^ __C declaration:__ @ex3_a@

         __defined at:__ @types\/nested\/nested_types.h 13:13@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  , ex3_ex3_struct_ex3_b :: BG.CChar
    {- ^ __C declaration:__ @ex3_b@

         __defined at:__ @types\/nested\/nested_types.h 14:14@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ex3_ex3_struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ex3_ex3_struct where

  readRaw =
    \ptr0 ->
          pure Ex3_ex3_struct
      <*> HasCField.readRaw (BG.Proxy @"ex3_ex3_struct_ex3_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"ex3_ex3_struct_ex3_b") ptr0

instance Marshal.WriteRaw Ex3_ex3_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex3_ex3_struct ex3_ex3_struct_ex3_a2 ex3_ex3_struct_ex3_b3 ->
               HasCField.writeRaw (BG.Proxy @"ex3_ex3_struct_ex3_a") ptr0 ex3_ex3_struct_ex3_a2
            >> HasCField.writeRaw (BG.Proxy @"ex3_ex3_struct_ex3_b") ptr0 ex3_ex3_struct_ex3_b3

deriving via Marshal.EquivStorable Ex3_ex3_struct instance BG.Storable Ex3_ex3_struct

{-| __C declaration:__ @ex3_a@

    __defined at:__ @types\/nested\/nested_types.h 13:13@

    __exported by:__ @types\/nested\/nested_types.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "ex3_ex3_struct_ex3_a" Ex3_ex3_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ex3_ex3_struct { ex3_ex3_struct_ex3_a = y1
                         , ex3_ex3_struct_ex3_b = BG.getField @"ex3_ex3_struct_ex3_b" x0
                         }
      , BG.getField @"ex3_ex3_struct_ex3_a" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "ex3_ex3_struct_ex3_a" (BG.Ptr Ex3_ex3_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ex3_ex3_struct_ex3_a")

instance HasCField.HasCField Ex3_ex3_struct "ex3_ex3_struct_ex3_a" where

  type CFieldType Ex3_ex3_struct "ex3_ex3_struct_ex3_a" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @ex3_b@

    __defined at:__ @types\/nested\/nested_types.h 14:14@

    __exported by:__ @types\/nested\/nested_types.h@
-}
instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "ex3_ex3_struct_ex3_b" Ex3_ex3_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ex3_ex3_struct { ex3_ex3_struct_ex3_b = y1
                         , ex3_ex3_struct_ex3_a = BG.getField @"ex3_ex3_struct_ex3_a" x0
                         }
      , BG.getField @"ex3_ex3_struct_ex3_b" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "ex3_ex3_struct_ex3_b" (BG.Ptr Ex3_ex3_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ex3_ex3_struct_ex3_b")

instance HasCField.HasCField Ex3_ex3_struct "ex3_ex3_struct_ex3_b" where

  type CFieldType Ex3_ex3_struct "ex3_ex3_struct_ex3_b" =
    BG.CChar

  offset# = \_ -> \_ -> 4

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
  , ex3_ex3_c :: BG.CFloat
    {- ^ __C declaration:__ @ex3_c@

         __defined at:__ @types\/nested\/nested_types.h 16:11@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ex3 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ex3 where

  readRaw =
    \ptr0 ->
          pure Ex3
      <*> HasCField.readRaw (BG.Proxy @"ex3_ex3_struct") ptr0
      <*> HasCField.readRaw (BG.Proxy @"ex3_ex3_c") ptr0

instance Marshal.WriteRaw Ex3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex3 ex3_ex3_struct2 ex3_ex3_c3 ->
               HasCField.writeRaw (BG.Proxy @"ex3_ex3_struct") ptr0 ex3_ex3_struct2
            >> HasCField.writeRaw (BG.Proxy @"ex3_ex3_c") ptr0 ex3_ex3_c3

deriving via Marshal.EquivStorable Ex3 instance BG.Storable Ex3

{-| __C declaration:__ @ex3_struct@

    __defined at:__ @types\/nested\/nested_types.h 15:7@

    __exported by:__ @types\/nested\/nested_types.h@
-}
instance ( ty ~ Ex3_ex3_struct
         ) => BG.CompatHasField.HasField "ex3_ex3_struct" Ex3 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ex3 {ex3_ex3_struct = y1, ex3_ex3_c = BG.getField @"ex3_ex3_c" x0}
      , BG.getField @"ex3_ex3_struct" x0
      )

instance ( ty ~ Ex3_ex3_struct
         ) => BG.HasField "ex3_ex3_struct" (BG.Ptr Ex3) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ex3_ex3_struct")

instance HasCField.HasCField Ex3 "ex3_ex3_struct" where

  type CFieldType Ex3 "ex3_ex3_struct" = Ex3_ex3_struct

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @ex3_c@

    __defined at:__ @types\/nested\/nested_types.h 16:11@

    __exported by:__ @types\/nested\/nested_types.h@
-}
instance (ty ~ BG.CFloat) => BG.CompatHasField.HasField "ex3_ex3_c" Ex3 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ex3 {ex3_ex3_c = y1, ex3_ex3_struct = BG.getField @"ex3_ex3_struct" x0}
      , BG.getField @"ex3_ex3_c" x0
      )

instance ( ty ~ BG.CFloat
         ) => BG.HasField "ex3_ex3_c" (BG.Ptr Ex3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"ex3_ex3_c")

instance HasCField.HasCField Ex3 "ex3_ex3_c" where

  type CFieldType Ex3 "ex3_ex3_c" = BG.CFloat

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct ex4_odd@

    __defined at:__ @types\/nested\/nested_types.h 22:8@

    __exported by:__ @types\/nested\/nested_types.h@
-}
data Ex4_odd = Ex4_odd
  { ex4_odd_value :: BG.CInt
    {- ^ __C declaration:__ @value@

         __defined at:__ @types\/nested\/nested_types.h 23:9@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  , ex4_odd_next :: BG.Ptr Ex4_even
    {- ^ __C declaration:__ @next@

         __defined at:__ @types\/nested\/nested_types.h 27:8@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ex4_odd where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ex4_odd where

  readRaw =
    \ptr0 ->
          pure Ex4_odd
      <*> HasCField.readRaw (BG.Proxy @"ex4_odd_value") ptr0
      <*> HasCField.readRaw (BG.Proxy @"ex4_odd_next") ptr0

instance Marshal.WriteRaw Ex4_odd where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex4_odd ex4_odd_value2 ex4_odd_next3 ->
               HasCField.writeRaw (BG.Proxy @"ex4_odd_value") ptr0 ex4_odd_value2
            >> HasCField.writeRaw (BG.Proxy @"ex4_odd_next") ptr0 ex4_odd_next3

deriving via Marshal.EquivStorable Ex4_odd instance BG.Storable Ex4_odd

{-| __C declaration:__ @value@

    __defined at:__ @types\/nested\/nested_types.h 23:9@

    __exported by:__ @types\/nested\/nested_types.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "ex4_odd_value" Ex4_odd ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ex4_odd {ex4_odd_value = y1, ex4_odd_next = BG.getField @"ex4_odd_next" x0}
      , BG.getField @"ex4_odd_value" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "ex4_odd_value" (BG.Ptr Ex4_odd) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ex4_odd_value")

instance HasCField.HasCField Ex4_odd "ex4_odd_value" where

  type CFieldType Ex4_odd "ex4_odd_value" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @next@

    __defined at:__ @types\/nested\/nested_types.h 27:8@

    __exported by:__ @types\/nested\/nested_types.h@
-}
instance ( ty ~ BG.Ptr Ex4_even
         ) => BG.CompatHasField.HasField "ex4_odd_next" Ex4_odd ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ex4_odd {ex4_odd_next = y1, ex4_odd_value = BG.getField @"ex4_odd_value" x0}
      , BG.getField @"ex4_odd_next" x0
      )

instance ( ty ~ BG.Ptr Ex4_even
         ) => BG.HasField "ex4_odd_next" (BG.Ptr Ex4_odd) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ex4_odd_next")

instance HasCField.HasCField Ex4_odd "ex4_odd_next" where

  type CFieldType Ex4_odd "ex4_odd_next" =
    BG.Ptr Ex4_even

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct ex4_even@

    __defined at:__ @types\/nested\/nested_types.h 24:12@

    __exported by:__ @types\/nested\/nested_types.h@
-}
data Ex4_even = Ex4_even
  { ex4_even_value :: BG.CDouble
    {- ^ __C declaration:__ @value@

         __defined at:__ @types\/nested\/nested_types.h 25:16@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  , ex4_even_next :: BG.Ptr Ex4_odd
    {- ^ __C declaration:__ @next@

         __defined at:__ @types\/nested\/nested_types.h 26:25@

         __exported by:__ @types\/nested\/nested_types.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Ex4_even where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Ex4_even where

  readRaw =
    \ptr0 ->
          pure Ex4_even
      <*> HasCField.readRaw (BG.Proxy @"ex4_even_value") ptr0
      <*> HasCField.readRaw (BG.Proxy @"ex4_even_next") ptr0

instance Marshal.WriteRaw Ex4_even where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ex4_even ex4_even_value2 ex4_even_next3 ->
               HasCField.writeRaw (BG.Proxy @"ex4_even_value") ptr0 ex4_even_value2
            >> HasCField.writeRaw (BG.Proxy @"ex4_even_next") ptr0 ex4_even_next3

deriving via Marshal.EquivStorable Ex4_even instance BG.Storable Ex4_even

{-| __C declaration:__ @value@

    __defined at:__ @types\/nested\/nested_types.h 25:16@

    __exported by:__ @types\/nested\/nested_types.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "ex4_even_value" Ex4_even ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ex4_even {ex4_even_value = y1, ex4_even_next = BG.getField @"ex4_even_next" x0}
      , BG.getField @"ex4_even_value" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "ex4_even_value" (BG.Ptr Ex4_even) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ex4_even_value")

instance HasCField.HasCField Ex4_even "ex4_even_value" where

  type CFieldType Ex4_even "ex4_even_value" =
    BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @next@

    __defined at:__ @types\/nested\/nested_types.h 26:25@

    __exported by:__ @types\/nested\/nested_types.h@
-}
instance ( ty ~ BG.Ptr Ex4_odd
         ) => BG.CompatHasField.HasField "ex4_even_next" Ex4_even ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Ex4_even {ex4_even_next = y1, ex4_even_value = BG.getField @"ex4_even_value" x0}
      , BG.getField @"ex4_even_next" x0
      )

instance ( ty ~ BG.Ptr Ex4_odd
         ) => BG.HasField "ex4_even_next" (BG.Ptr Ex4_even) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"ex4_even_next")

instance HasCField.HasCField Ex4_even "ex4_even_next" where

  type CFieldType Ex4_even "ex4_even_next" =
    BG.Ptr Ex4_odd

  offset# = \_ -> \_ -> 8
