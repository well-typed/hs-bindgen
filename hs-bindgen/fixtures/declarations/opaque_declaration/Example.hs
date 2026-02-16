{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure, return)

{-| __C declaration:__ @struct foo@

    __defined at:__ @declarations\/opaque_declaration.h 1:8@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Foo

{-| __C declaration:__ @struct bar@

    __defined at:__ @declarations\/opaque_declaration.h 4:8@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Bar = Bar
  { bar_ptrA :: Ptr.Ptr Foo
    {- ^ __C declaration:__ @ptrA@

         __defined at:__ @declarations\/opaque_declaration.h 5:17@

         __exported by:__ @declarations\/opaque_declaration.h@
    -}
  , bar_ptrB :: Ptr.Ptr Bar
    {- ^ __C declaration:__ @ptrB@

         __defined at:__ @declarations\/opaque_declaration.h 6:17@

         __exported by:__ @declarations\/opaque_declaration.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"bar_ptrA") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"bar_ptrB") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_ptrA2 bar_ptrB3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"bar_ptrA") ptr0 bar_ptrA2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"bar_ptrB") ptr0 bar_ptrB3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Bar instance F.Storable Bar

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_ptrA" where

  type CFieldType Bar "bar_ptrA" = Ptr.Ptr Foo

  offset# = \_ -> \_ -> 0

instance ( TyEq ty (Ptr.Ptr Foo)
         ) => GHC.Records.HasField "bar_ptrA" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"bar_ptrA")

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_ptrB" where

  type CFieldType Bar "bar_ptrB" = Ptr.Ptr Bar

  offset# = \_ -> \_ -> 8

instance ( TyEq ty (Ptr.Ptr Bar)
         ) => GHC.Records.HasField "bar_ptrB" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"bar_ptrB")

{-| __C declaration:__ @struct baz@

    __defined at:__ @declarations\/opaque_declaration.h 9:8@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Baz = Baz
  {}
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Baz where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Baz where

  readRaw = \ptr0 -> pure Baz

instance HsBindgen.Runtime.Marshal.WriteRaw Baz where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable Baz instance F.Storable Baz

{-| __C declaration:__ @enum quu@

    __defined at:__ @declarations\/opaque_declaration.h 11:6@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Quu

{-| __C declaration:__ @union opaque_union@

    __defined at:__ @declarations\/opaque_declaration.h 13:7@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Opaque_union
