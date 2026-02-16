{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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

    __defined at:__ @declarations\/opaque_declaration.h 1:8@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Foo

{-| __C declaration:__ @struct bar@

    __defined at:__ @declarations\/opaque_declaration.h 4:8@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Bar = Bar
  { bar_ptrA :: RIP.Ptr Foo
    {- ^ __C declaration:__ @ptrA@

         __defined at:__ @declarations\/opaque_declaration.h 5:17@

         __exported by:__ @declarations\/opaque_declaration.h@
    -}
  , bar_ptrB :: RIP.Ptr Bar
    {- ^ __C declaration:__ @ptrB@

         __defined at:__ @declarations\/opaque_declaration.h 6:17@

         __exported by:__ @declarations\/opaque_declaration.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCField.readRaw (RIP.Proxy @"bar_ptrA") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"bar_ptrB") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_ptrA2 bar_ptrB3 ->
               HasCField.writeRaw (RIP.Proxy @"bar_ptrA") ptr0 bar_ptrA2
            >> HasCField.writeRaw (RIP.Proxy @"bar_ptrB") ptr0 bar_ptrB3

deriving via Marshal.EquivStorable Bar instance RIP.Storable Bar

instance HasCField.HasCField Bar "bar_ptrA" where

  type CFieldType Bar "bar_ptrA" = RIP.Ptr Foo

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.Ptr Foo)
         ) => RIP.HasField "bar_ptrA" (RIP.Ptr Bar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bar_ptrA")

instance HasCField.HasCField Bar "bar_ptrB" where

  type CFieldType Bar "bar_ptrB" = RIP.Ptr Bar

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr Bar)
         ) => RIP.HasField "bar_ptrB" (RIP.Ptr Bar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bar_ptrB")

{-| __C declaration:__ @struct baz@

    __defined at:__ @declarations\/opaque_declaration.h 9:8@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Baz = Baz
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Baz where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Baz where

  readRaw = \ptr0 -> pure Baz

instance Marshal.WriteRaw Baz where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz -> return ()

deriving via Marshal.EquivStorable Baz instance RIP.Storable Baz

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
