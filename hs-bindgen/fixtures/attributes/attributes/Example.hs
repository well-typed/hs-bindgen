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

    __defined at:__ @attributes\/attributes.h 10:36@

    __exported by:__ @attributes\/attributes.h@
-}
data Foo = Foo
  { foo_c :: RIP.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/attributes.h 11:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  , foo_i :: RIP.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/attributes.h 12:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (5 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (RIP.Proxy @"foo_c") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"foo_i") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_c2 foo_i3 ->
               HasCField.writeRaw (RIP.Proxy @"foo_c") ptr0 foo_c2
            >> HasCField.writeRaw (RIP.Proxy @"foo_i") ptr0 foo_i3

deriving via Marshal.EquivStorable Foo instance RIP.Storable Foo

instance HasCField.HasCField Foo "foo_c" where

  type CFieldType Foo "foo_c" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "foo_c" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"foo_c")

instance HasCField.HasCField Foo "foo_i" where

  type CFieldType Foo "foo_i" = RIP.CInt

  offset# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "foo_i" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"foo_i")

{-| __C declaration:__ @struct bar@

    __defined at:__ @attributes\/attributes.h 16:15@

    __exported by:__ @attributes\/attributes.h@
-}
data Bar = Bar
  { bar_c :: RIP.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/attributes.h 17:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  , bar_i :: RIP.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/attributes.h 18:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (5 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCField.readRaw (RIP.Proxy @"bar_c") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"bar_i") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_c2 bar_i3 ->
               HasCField.writeRaw (RIP.Proxy @"bar_c") ptr0 bar_c2
            >> HasCField.writeRaw (RIP.Proxy @"bar_i") ptr0 bar_i3

deriving via Marshal.EquivStorable Bar instance RIP.Storable Bar

instance HasCField.HasCField Bar "bar_c" where

  type CFieldType Bar "bar_c" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "bar_c" (RIP.Ptr Bar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bar_c")

instance HasCField.HasCField Bar "bar_i" where

  type CFieldType Bar "bar_i" = RIP.CInt

  offset# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "bar_i" (RIP.Ptr Bar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bar_i")

{-| __C declaration:__ @struct baz@

    __defined at:__ @attributes\/attributes.h 22:9@

    __exported by:__ @attributes\/attributes.h@
-}
data Baz = Baz
  { baz_c :: RIP.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/attributes.h 23:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  , baz_i :: RIP.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/attributes.h 24:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Baz where

  staticSizeOf = \_ -> (5 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Baz where

  readRaw =
    \ptr0 ->
          pure Baz
      <*> HasCField.readRaw (RIP.Proxy @"baz_c") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"baz_i") ptr0

instance Marshal.WriteRaw Baz where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz baz_c2 baz_i3 ->
               HasCField.writeRaw (RIP.Proxy @"baz_c") ptr0 baz_c2
            >> HasCField.writeRaw (RIP.Proxy @"baz_i") ptr0 baz_i3

deriving via Marshal.EquivStorable Baz instance RIP.Storable Baz

instance HasCField.HasCField Baz "baz_c" where

  type CFieldType Baz "baz_c" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "baz_c" (RIP.Ptr Baz) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"baz_c")

instance HasCField.HasCField Baz "baz_i" where

  type CFieldType Baz "baz_i" = RIP.CInt

  offset# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "baz_i" (RIP.Ptr Baz) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"baz_i")

{-| __C declaration:__ @struct qux@

    __defined at:__ @attributes\/attributes.h 28:9@

    __exported by:__ @attributes\/attributes.h@
-}
data Qux = Qux
  { qux_c :: RIP.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/attributes.h 29:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  , qux_i :: RIP.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/attributes.h 30:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Qux where

  staticSizeOf = \_ -> (5 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Qux where

  readRaw =
    \ptr0 ->
          pure Qux
      <*> HasCField.readRaw (RIP.Proxy @"qux_c") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"qux_i") ptr0

instance Marshal.WriteRaw Qux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Qux qux_c2 qux_i3 ->
               HasCField.writeRaw (RIP.Proxy @"qux_c") ptr0 qux_c2
            >> HasCField.writeRaw (RIP.Proxy @"qux_i") ptr0 qux_i3

deriving via Marshal.EquivStorable Qux instance RIP.Storable Qux

instance HasCField.HasCField Qux "qux_c" where

  type CFieldType Qux "qux_c" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "qux_c" (RIP.Ptr Qux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"qux_c")

instance HasCField.HasCField Qux "qux_i" where

  type CFieldType Qux "qux_i" = RIP.CInt

  offset# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "qux_i" (RIP.Ptr Qux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"qux_i")

{-| __C declaration:__ @struct __sFILE@

    __defined at:__ @attributes\/attributes.h 34:16@

    __exported by:__ @attributes\/attributes.h@
-}
data FILE = FILE
  { fILE__r :: RIP.CInt
    {- ^ __C declaration:__ @_r@

         __defined at:__ @attributes\/attributes.h 35:9@

         __exported by:__ @attributes\/attributes.h@
    -}
  , fILE__w :: RIP.CInt
    {- ^ __C declaration:__ @_w@

         __defined at:__ @attributes\/attributes.h 36:9@

         __exported by:__ @attributes\/attributes.h@
    -}
  , fILE__close :: RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO RIP.CInt)
    {- ^ __C declaration:__ @_close@

         __defined at:__ @attributes\/attributes.h 37:22@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize FILE where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw FILE where

  readRaw =
    \ptr0 ->
          pure FILE
      <*> HasCField.readRaw (RIP.Proxy @"fILE__r") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"fILE__w") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"fILE__close") ptr0

instance Marshal.WriteRaw FILE where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          FILE fILE__r2 fILE__w3 fILE__close4 ->
               HasCField.writeRaw (RIP.Proxy @"fILE__r") ptr0 fILE__r2
            >> HasCField.writeRaw (RIP.Proxy @"fILE__w") ptr0 fILE__w3
            >> HasCField.writeRaw (RIP.Proxy @"fILE__close") ptr0 fILE__close4

deriving via Marshal.EquivStorable FILE instance RIP.Storable FILE

instance HasCField.HasCField FILE "fILE__r" where

  type CFieldType FILE "fILE__r" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "fILE__r" (RIP.Ptr FILE) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"fILE__r")

instance HasCField.HasCField FILE "fILE__w" where

  type CFieldType FILE "fILE__w" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "fILE__w" (RIP.Ptr FILE) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"fILE__w")

instance HasCField.HasCField FILE "fILE__close" where

  type CFieldType FILE "fILE__close" =
    RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO RIP.CInt)

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO RIP.CInt))
         ) => RIP.HasField "fILE__close" (RIP.Ptr FILE) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"fILE__close")
