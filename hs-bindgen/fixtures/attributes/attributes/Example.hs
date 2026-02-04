{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import qualified HsBindgen.Runtime.Marshal
import Data.Void (Void)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

{-| __C declaration:__ @struct foo@

    __defined at:__ @attributes\/attributes.h 10:36@

    __exported by:__ @attributes\/attributes.h@
-}
data Foo = Foo
  { foo_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/attributes.h 11:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  , foo_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/attributes.h 12:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (5 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"foo_c") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"foo_i") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_c2 foo_i3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"foo_c") ptr0 foo_c2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"foo_i") ptr0 foo_i3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Foo instance F.Storable Foo

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_c" where

  type CFieldType Foo "foo_c" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_c")
         ) => GHC.Records.HasField "foo_c" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"foo_c")

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_i" where

  type CFieldType Foo "foo_i" = FC.CInt

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_i")
         ) => GHC.Records.HasField "foo_i" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"foo_i")

{-| __C declaration:__ @struct bar@

    __defined at:__ @attributes\/attributes.h 16:15@

    __exported by:__ @attributes\/attributes.h@
-}
data Bar = Bar
  { bar_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/attributes.h 17:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  , bar_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/attributes.h 18:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (5 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"bar_c") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"bar_i") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_c2 bar_i3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"bar_c") ptr0 bar_c2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"bar_i") ptr0 bar_i3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Bar instance F.Storable Bar

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_c" where

  type CFieldType Bar "bar_c" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_c")
         ) => GHC.Records.HasField "bar_c" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"bar_c")

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_i" where

  type CFieldType Bar "bar_i" = FC.CInt

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_i")
         ) => GHC.Records.HasField "bar_i" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"bar_i")

{-| __C declaration:__ @struct baz@

    __defined at:__ @attributes\/attributes.h 22:9@

    __exported by:__ @attributes\/attributes.h@
-}
data Baz = Baz
  { baz_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/attributes.h 23:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  , baz_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/attributes.h 24:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Baz where

  staticSizeOf = \_ -> (5 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Baz where

  readRaw =
    \ptr0 ->
          pure Baz
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"baz_c") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"baz_i") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Baz where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz baz_c2 baz_i3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"baz_c") ptr0 baz_c2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"baz_i") ptr0 baz_i3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Baz instance F.Storable Baz

instance HsBindgen.Runtime.HasCField.HasCField Baz "baz_c" where

  type CFieldType Baz "baz_c" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Baz) "baz_c")
         ) => GHC.Records.HasField "baz_c" (Ptr.Ptr Baz) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"baz_c")

instance HsBindgen.Runtime.HasCField.HasCField Baz "baz_i" where

  type CFieldType Baz "baz_i" = FC.CInt

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Baz) "baz_i")
         ) => GHC.Records.HasField "baz_i" (Ptr.Ptr Baz) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"baz_i")

{-| __C declaration:__ @struct qux@

    __defined at:__ @attributes\/attributes.h 28:9@

    __exported by:__ @attributes\/attributes.h@
-}
data Qux = Qux
  { qux_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/attributes.h 29:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  , qux_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/attributes.h 30:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Qux where

  staticSizeOf = \_ -> (5 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Qux where

  readRaw =
    \ptr0 ->
          pure Qux
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"qux_c") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"qux_i") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Qux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Qux qux_c2 qux_i3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"qux_c") ptr0 qux_c2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"qux_i") ptr0 qux_i3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Qux instance F.Storable Qux

instance HsBindgen.Runtime.HasCField.HasCField Qux "qux_c" where

  type CFieldType Qux "qux_c" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Qux) "qux_c")
         ) => GHC.Records.HasField "qux_c" (Ptr.Ptr Qux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"qux_c")

instance HsBindgen.Runtime.HasCField.HasCField Qux "qux_i" where

  type CFieldType Qux "qux_i" = FC.CInt

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Qux) "qux_i")
         ) => GHC.Records.HasField "qux_i" (Ptr.Ptr Qux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"qux_i")

{-| __C declaration:__ @struct __sFILE@

    __defined at:__ @attributes\/attributes.h 34:16@

    __exported by:__ @attributes\/attributes.h@
-}
data FILE = FILE
  { fILE__r :: FC.CInt
    {- ^ __C declaration:__ @_r@

         __defined at:__ @attributes\/attributes.h 35:9@

         __exported by:__ @attributes\/attributes.h@
    -}
  , fILE__w :: FC.CInt
    {- ^ __C declaration:__ @_w@

         __defined at:__ @attributes\/attributes.h 36:9@

         __exported by:__ @attributes\/attributes.h@
    -}
  , fILE__close :: Ptr.FunPtr ((Ptr.Ptr Void) -> IO FC.CInt)
    {- ^ __C declaration:__ @_close@

         __defined at:__ @attributes\/attributes.h 37:22@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize FILE where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw FILE where

  readRaw =
    \ptr0 ->
          pure FILE
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"fILE__r") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"fILE__w") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"fILE__close") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw FILE where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          FILE fILE__r2 fILE__w3 fILE__close4 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"fILE__r") ptr0 fILE__r2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"fILE__w") ptr0 fILE__w3
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"fILE__close") ptr0 fILE__close4

deriving via HsBindgen.Runtime.Marshal.EquivStorable FILE instance F.Storable FILE

instance HsBindgen.Runtime.HasCField.HasCField FILE "fILE__r" where

  type CFieldType FILE "fILE__r" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FILE) "fILE__r")
         ) => GHC.Records.HasField "fILE__r" (Ptr.Ptr FILE) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"fILE__r")

instance HsBindgen.Runtime.HasCField.HasCField FILE "fILE__w" where

  type CFieldType FILE "fILE__w" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FILE) "fILE__w")
         ) => GHC.Records.HasField "fILE__w" (Ptr.Ptr FILE) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"fILE__w")

instance HsBindgen.Runtime.HasCField.HasCField FILE "fILE__close" where

  type CFieldType FILE "fILE__close" =
    Ptr.FunPtr ((Ptr.Ptr Void) -> IO FC.CInt)

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FILE) "fILE__close")
         ) => GHC.Records.HasField "fILE__close" (Ptr.Ptr FILE) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"fILE__close")
