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
import Data.Void (Void)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

{-| __C declaration:__ @foo@

    __defined at:__ @attributes\/attributes.h:10:36@

    __exported by:__ @attributes\/attributes.h@
-}
data Foo = Foo
  { foo_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/attributes.h:11:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  , foo_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/attributes.h:12:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Foo where

  sizeOf = \_ -> (5 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"foo_c") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"foo_i") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_c2 foo_i3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"foo_c") ptr0 foo_c2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"foo_i") ptr0 foo_i3

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_c" where

  type CFieldType Foo "foo_c" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_c")
         ) => GHC.Records.HasField "foo_c" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"foo_c")

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_i" where

  type CFieldType Foo "foo_i" = FC.CInt

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_i")
         ) => GHC.Records.HasField "foo_i" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"foo_i")

{-| __C declaration:__ @bar@

    __defined at:__ @attributes\/attributes.h:16:15@

    __exported by:__ @attributes\/attributes.h@
-}
data Bar = Bar
  { bar_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/attributes.h:17:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  , bar_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/attributes.h:18:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bar where

  sizeOf = \_ -> (5 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bar_c") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bar_i") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_c2 bar_i3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bar_c") ptr0 bar_c2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bar_i") ptr0 bar_i3

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_c" where

  type CFieldType Bar "bar_c" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_c")
         ) => GHC.Records.HasField "bar_c" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bar_c")

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_i" where

  type CFieldType Bar "bar_i" = FC.CInt

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_i")
         ) => GHC.Records.HasField "bar_i" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bar_i")

{-| __defined at:__ @attributes\/attributes.h:22:9@

    __exported by:__ @attributes\/attributes.h@
-}
data Baz = Baz
  { baz_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/attributes.h:23:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  , baz_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/attributes.h:24:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Baz where

  sizeOf = \_ -> (5 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Baz
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"baz_c") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"baz_i") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz baz_c2 baz_i3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"baz_c") ptr0 baz_c2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"baz_i") ptr0 baz_i3

instance HsBindgen.Runtime.HasCField.HasCField Baz "baz_c" where

  type CFieldType Baz "baz_c" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Baz) "baz_c")
         ) => GHC.Records.HasField "baz_c" (Ptr.Ptr Baz) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"baz_c")

instance HsBindgen.Runtime.HasCField.HasCField Baz "baz_i" where

  type CFieldType Baz "baz_i" = FC.CInt

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Baz) "baz_i")
         ) => GHC.Records.HasField "baz_i" (Ptr.Ptr Baz) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"baz_i")

{-| __defined at:__ @attributes\/attributes.h:28:9@

    __exported by:__ @attributes\/attributes.h@
-}
data Qux = Qux
  { qux_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes\/attributes.h:29:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  , qux_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes\/attributes.h:30:10@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Qux where

  sizeOf = \_ -> (5 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Qux
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"qux_c") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"qux_i") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Qux qux_c2 qux_i3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"qux_c") ptr0 qux_c2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"qux_i") ptr0 qux_i3

instance HsBindgen.Runtime.HasCField.HasCField Qux "qux_c" where

  type CFieldType Qux "qux_c" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Qux) "qux_c")
         ) => GHC.Records.HasField "qux_c" (Ptr.Ptr Qux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"qux_c")

instance HsBindgen.Runtime.HasCField.HasCField Qux "qux_i" where

  type CFieldType Qux "qux_i" = FC.CInt

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Qux) "qux_i")
         ) => GHC.Records.HasField "qux_i" (Ptr.Ptr Qux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"qux_i")

{-| __C declaration:__ @FILE@

    __defined at:__ @attributes\/attributes.h:34:16@

    __exported by:__ @attributes\/attributes.h@
-}
data FILE = FILE
  { fILE__r :: FC.CInt
    {- ^ __C declaration:__ @_r@

         __defined at:__ @attributes\/attributes.h:35:9@

         __exported by:__ @attributes\/attributes.h@
    -}
  , fILE__w :: FC.CInt
    {- ^ __C declaration:__ @_w@

         __defined at:__ @attributes\/attributes.h:36:9@

         __exported by:__ @attributes\/attributes.h@
    -}
  , fILE__close :: Ptr.FunPtr ((Ptr.Ptr Void) -> IO FC.CInt)
    {- ^ __C declaration:__ @_close@

         __defined at:__ @attributes\/attributes.h:37:22@

         __exported by:__ @attributes\/attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable FILE where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure FILE
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"fILE__r") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"fILE__w") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"fILE__close") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          FILE fILE__r2 fILE__w3 fILE__close4 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"fILE__r") ptr0 fILE__r2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"fILE__w") ptr0 fILE__w3
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"fILE__close") ptr0 fILE__close4

instance HsBindgen.Runtime.HasCField.HasCField FILE "fILE__r" where

  type CFieldType FILE "fILE__r" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FILE) "fILE__r")
         ) => GHC.Records.HasField "fILE__r" (Ptr.Ptr FILE) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"fILE__r")

instance HsBindgen.Runtime.HasCField.HasCField FILE "fILE__w" where

  type CFieldType FILE "fILE__w" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FILE) "fILE__w")
         ) => GHC.Records.HasField "fILE__w" (Ptr.Ptr FILE) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"fILE__w")

instance HsBindgen.Runtime.HasCField.HasCField FILE "fILE__close" where

  type CFieldType FILE "fILE__close" =
    Ptr.FunPtr ((Ptr.Ptr Void) -> IO FC.CInt)

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FILE) "fILE__close")
         ) => GHC.Records.HasField "fILE__close" (Ptr.Ptr FILE) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"fILE__close")
