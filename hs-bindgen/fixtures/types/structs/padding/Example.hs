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

module Example
    ( Example.Foo(..)
    , Example.Bar(..)
    )
  where

import qualified HsBindgen.Runtime.BitfieldPtr as BitfieldPtr
import qualified HsBindgen.Runtime.HasCBitfield as HasCBitfield
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct foo@

    __defined at:__ @types\/structs\/padding.h 1:8@

    __exported by:__ @types\/structs\/padding.h@
-}
data Foo = Foo
  { foo_a :: RIP.CSChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/padding.h 2:15@

         __exported by:__ @types\/structs\/padding.h@
    -}
  , foo_b :: RIP.CSChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/padding.h 3:15@

         __exported by:__ @types\/structs\/padding.h@
    -}
  , foo_c :: RIP.CSChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/padding.h 5:15@

         __exported by:__ @types\/structs\/padding.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCBitfield.peek (RIP.Proxy @"foo_a") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_b") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"foo_c") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_a2 foo_b3 foo_c4 ->
               HasCBitfield.poke (RIP.Proxy @"foo_a") ptr0 foo_a2
            >> HasCBitfield.poke (RIP.Proxy @"foo_b") ptr0 foo_b3
            >> HasCBitfield.poke (RIP.Proxy @"foo_c") ptr0 foo_c4

deriving via Marshal.EquivStorable Foo instance RIP.Storable Foo

instance HasCBitfield.HasCBitfield Foo "foo_a" where

  type CBitfieldType Foo "foo_a" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 3

instance ( ((~) ty) RIP.CSChar
         ) => RIP.HasField "foo_a" (RIP.Ptr Foo) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_a")

instance HasCBitfield.HasCBitfield Foo "foo_b" where

  type CBitfieldType Foo "foo_b" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 3

  bitfieldWidth# = \_ -> \_ -> 3

instance ( ((~) ty) RIP.CSChar
         ) => RIP.HasField "foo_b" (RIP.Ptr Foo) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_b")

instance HasCBitfield.HasCBitfield Foo "foo_c" where

  type CBitfieldType Foo "foo_c" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 8

  bitfieldWidth# = \_ -> \_ -> 2

instance ( ((~) ty) RIP.CSChar
         ) => RIP.HasField "foo_c" (RIP.Ptr Foo) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"foo_c")

{-| __C declaration:__ @struct bar@

    __defined at:__ @types\/structs\/padding.h 8:8@

    __exported by:__ @types\/structs\/padding.h@
-}
data Bar = Bar
  { bar_x :: RIP.CSChar
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/padding.h 9:15@

         __exported by:__ @types\/structs\/padding.h@
    -}
  , bar_y :: RIP.CSChar
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/structs\/padding.h 11:15@

         __exported by:__ @types\/structs\/padding.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCBitfield.peek (RIP.Proxy @"bar_x") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bar_y") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_x2 bar_y3 ->
               HasCBitfield.poke (RIP.Proxy @"bar_x") ptr0 bar_x2
            >> HasCBitfield.poke (RIP.Proxy @"bar_y") ptr0 bar_y3

deriving via Marshal.EquivStorable Bar instance RIP.Storable Bar

instance HasCBitfield.HasCBitfield Bar "bar_x" where

  type CBitfieldType Bar "bar_x" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 3

instance ( ((~) ty) RIP.CSChar
         ) => RIP.HasField "bar_x" (RIP.Ptr Bar) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"bar_x")

instance HasCBitfield.HasCBitfield Bar "bar_y" where

  type CBitfieldType Bar "bar_y" = RIP.CSChar

  bitfieldOffset# = \_ -> \_ -> 6

  bitfieldWidth# = \_ -> \_ -> 2

instance ( ((~) ty) RIP.CSChar
         ) => RIP.HasField "bar_y" (RIP.Ptr Bar) (BitfieldPtr.BitfieldPtr ty) where

  getField = HasCBitfield.toPtr (RIP.Proxy @"bar_y")
