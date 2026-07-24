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
    )
  where

import qualified HsBindgen.Runtime.BitfieldPtr as BitfieldPtr
import qualified HsBindgen.Runtime.HasCBitfield as HasCBitfield
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct foo@

    __defined at:__ @types\/structs\/padding.h 1:8@

    __exported by:__ @types\/structs\/padding.h@
-}
data Foo = Foo
  { foo_a :: BG.CSChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/padding.h 2:15@

         __exported by:__ @types\/structs\/padding.h@
    -}
  , foo_b :: BG.CSChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/padding.h 3:15@

         __exported by:__ @types\/structs\/padding.h@
    -}
  , foo_c :: BG.CSChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/padding.h 5:15@

         __exported by:__ @types\/structs\/padding.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCBitfield.peek (BG.Proxy @"foo_a") ptr0
      <*> HasCBitfield.peek (BG.Proxy @"foo_b") ptr0
      <*> HasCBitfield.peek (BG.Proxy @"foo_c") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_a2 foo_b3 foo_c4 ->
               HasCBitfield.poke (BG.Proxy @"foo_a") ptr0 foo_a2
            >> HasCBitfield.poke (BG.Proxy @"foo_b") ptr0 foo_b3
            >> HasCBitfield.poke (BG.Proxy @"foo_c") ptr0 foo_c4

deriving via Marshal.EquivStorable Foo instance BG.Storable Foo

deriving via Struct.IsStructViaStorable Foo instance Struct.IsStruct Foo

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/padding.h 2:15@

    __exported by:__ @types\/structs\/padding.h@
-}
instance (ty ~ BG.CSChar) => BG.CompatHasField.HasField "foo_a" Foo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo {foo_a = y1, foo_b = BG.getField @"foo_b" x0, foo_c = BG.getField @"foo_c" x0}
      , BG.getField @"foo_a" x0
      )

instance ( ty ~ BG.CSChar
         ) => BG.HasField "foo_a" (BG.Ptr Foo) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_a")

instance HasCBitfield.HasCBitfield Foo "foo_a" where

  type CBitfieldType Foo "foo_a" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 3

{-| __C declaration:__ @b@

    __defined at:__ @types\/structs\/padding.h 3:15@

    __exported by:__ @types\/structs\/padding.h@
-}
instance (ty ~ BG.CSChar) => BG.CompatHasField.HasField "foo_b" Foo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo {foo_b = y1, foo_a = BG.getField @"foo_a" x0, foo_c = BG.getField @"foo_c" x0}
      , BG.getField @"foo_b" x0
      )

instance ( ty ~ BG.CSChar
         ) => BG.HasField "foo_b" (BG.Ptr Foo) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_b")

instance HasCBitfield.HasCBitfield Foo "foo_b" where

  type CBitfieldType Foo "foo_b" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 3

  bitfieldWidth# = \_ -> \_ -> 3

{-| __C declaration:__ @c@

    __defined at:__ @types\/structs\/padding.h 5:15@

    __exported by:__ @types\/structs\/padding.h@
-}
instance (ty ~ BG.CSChar) => BG.CompatHasField.HasField "foo_c" Foo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo {foo_c = y1, foo_a = BG.getField @"foo_a" x0, foo_b = BG.getField @"foo_b" x0}
      , BG.getField @"foo_c" x0
      )

instance ( ty ~ BG.CSChar
         ) => BG.HasField "foo_c" (BG.Ptr Foo) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"foo_c")

instance HasCBitfield.HasCBitfield Foo "foo_c" where

  type CBitfieldType Foo "foo_c" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 8

  bitfieldWidth# = \_ -> \_ -> 2

{-| __C declaration:__ @struct bar@

    __defined at:__ @types\/structs\/padding.h 8:8@

    __exported by:__ @types\/structs\/padding.h@
-}
data Bar = Bar
  { bar_x :: BG.CSChar
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/padding.h 9:15@

         __exported by:__ @types\/structs\/padding.h@
    -}
  , bar_y :: BG.CSChar
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/padding.h 11:15@

         __exported by:__ @types\/structs\/padding.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCBitfield.peek (BG.Proxy @"bar_x") ptr0
      <*> HasCBitfield.peek (BG.Proxy @"bar_y") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_x2 bar_y3 ->
               HasCBitfield.poke (BG.Proxy @"bar_x") ptr0 bar_x2
            >> HasCBitfield.poke (BG.Proxy @"bar_y") ptr0 bar_y3

deriving via Marshal.EquivStorable Bar instance BG.Storable Bar

deriving via Struct.IsStructViaStorable Bar instance Struct.IsStruct Bar

{-| __C declaration:__ @x@

    __defined at:__ @types\/structs\/padding.h 9:15@

    __exported by:__ @types\/structs\/padding.h@
-}
instance (ty ~ BG.CSChar) => BG.CompatHasField.HasField "bar_x" Bar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar {bar_x = y1, bar_y = BG.getField @"bar_y" x0}
      , BG.getField @"bar_x" x0
      )

instance ( ty ~ BG.CSChar
         ) => BG.HasField "bar_x" (BG.Ptr Bar) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"bar_x")

instance HasCBitfield.HasCBitfield Bar "bar_x" where

  type CBitfieldType Bar "bar_x" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 3

{-| __C declaration:__ @y@

    __defined at:__ @types\/structs\/padding.h 11:15@

    __exported by:__ @types\/structs\/padding.h@
-}
instance (ty ~ BG.CSChar) => BG.CompatHasField.HasField "bar_y" Bar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar {bar_y = y1, bar_x = BG.getField @"bar_x" x0}
      , BG.getField @"bar_y" x0
      )

instance ( ty ~ BG.CSChar
         ) => BG.HasField "bar_y" (BG.Ptr Bar) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (BG.Proxy @"bar_y")

instance HasCBitfield.HasCBitfield Bar "bar_y" where

  type CBitfieldType Bar "bar_y" = BG.CSChar

  bitfieldOffset# = \_ -> \_ -> 6

  bitfieldWidth# = \_ -> \_ -> 2
