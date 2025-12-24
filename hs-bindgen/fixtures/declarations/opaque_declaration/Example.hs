{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure, return)

{-| __C declaration:__ @struct foo@

    __defined at:__ @declarations\/opaque_declaration.h:1:8@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Foo

{-| __C declaration:__ @struct bar@

    __defined at:__ @declarations\/opaque_declaration.h:4:8@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Bar = Bar
  { bar_ptrA :: Ptr.Ptr Foo
    {- ^ __C declaration:__ @ptrA@

         __defined at:__ @declarations\/opaque_declaration.h:5:17@

         __exported by:__ @declarations\/opaque_declaration.h@
    -}
  , bar_ptrB :: Ptr.Ptr Bar
    {- ^ __C declaration:__ @ptrB@

         __defined at:__ @declarations\/opaque_declaration.h:6:17@

         __exported by:__ @declarations\/opaque_declaration.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bar where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bar_ptrA") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bar_ptrB") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_ptrA2 bar_ptrB3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bar_ptrA") ptr0 bar_ptrA2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bar_ptrB") ptr0 bar_ptrB3

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_ptrA" where

  type CFieldType Bar "bar_ptrA" = Ptr.Ptr Foo

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_ptrA")
         ) => GHC.Records.HasField "bar_ptrA" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bar_ptrA")

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_ptrB" where

  type CFieldType Bar "bar_ptrB" = Ptr.Ptr Bar

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_ptrB")
         ) => GHC.Records.HasField "bar_ptrB" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bar_ptrB")

{-| __C declaration:__ @struct baz@

    __defined at:__ @declarations\/opaque_declaration.h:9:8@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Baz = Baz
  {}
  deriving stock (Eq, Show)

instance F.Storable Baz where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Baz

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz -> return ()

instance Data.Primitive.Types.Prim Baz where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> Baz

  readByteArray# = \arr0 -> \i1 -> \s2 -> (# s2, Baz #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Baz -> s3

  indexOffAddr# = \addr0 -> \i1 -> Baz

  readOffAddr# = \addr0 -> \i1 -> \s2 -> (# s2, Baz #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Baz -> s3

{-| __C declaration:__ @enum quu@

    __defined at:__ @declarations\/opaque_declaration.h:11:6@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Quu

{-| __C declaration:__ @union opaque_union@

    __defined at:__ @declarations\/opaque_declaration.h:13:7@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Opaque_union
