{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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
    ( Example.Foo
    , Example.Bar(..)
    , Example.Baz(..)
    , Example.Quu
    , Example.Opaque_union
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

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
  { bar_ptrA :: BG.Ptr Foo
    {- ^ __C declaration:__ @ptrA@

         __defined at:__ @declarations\/opaque_declaration.h 5:17@

         __exported by:__ @declarations\/opaque_declaration.h@
    -}
  , bar_ptrB :: BG.Ptr Bar
    {- ^ __C declaration:__ @ptrB@

         __defined at:__ @declarations\/opaque_declaration.h 6:17@

         __exported by:__ @declarations\/opaque_declaration.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCField.readRaw (BG.Proxy @"bar_ptrA") ptr0
      <*> HasCField.readRaw (BG.Proxy @"bar_ptrB") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_ptrA2 bar_ptrB3 ->
               HasCField.writeRaw (BG.Proxy @"bar_ptrA") ptr0 bar_ptrA2
            >> HasCField.writeRaw (BG.Proxy @"bar_ptrB") ptr0 bar_ptrB3

deriving via Marshal.EquivStorable Bar instance BG.Storable Bar

deriving via Struct.IsStructViaStorable Bar instance Struct.IsStruct Bar

{-| __C declaration:__ @ptrA@

    __defined at:__ @declarations\/opaque_declaration.h 5:17@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
instance (ty ~ BG.Ptr Foo) => BG.CompatHasField.HasField "bar_ptrA" Bar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar {bar_ptrA = y1, bar_ptrB = BG.getField @"bar_ptrB" x0}
      , BG.getField @"bar_ptrA" x0
      )

instance ( ty ~ BG.Ptr Foo
         ) => BG.HasField "bar_ptrA" (BG.Ptr Bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bar_ptrA")

instance HasCField.HasCField Bar "bar_ptrA" where

  type CFieldType Bar "bar_ptrA" = BG.Ptr Foo

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @ptrB@

    __defined at:__ @declarations\/opaque_declaration.h 6:17@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
instance (ty ~ BG.Ptr Bar) => BG.CompatHasField.HasField "bar_ptrB" Bar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar {bar_ptrB = y1, bar_ptrA = BG.getField @"bar_ptrA" x0}
      , BG.getField @"bar_ptrB" x0
      )

instance ( ty ~ BG.Ptr Bar
         ) => BG.HasField "bar_ptrB" (BG.Ptr Bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bar_ptrB")

instance HasCField.HasCField Bar "bar_ptrB" where

  type CFieldType Bar "bar_ptrB" = BG.Ptr Bar

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct baz@

    __defined at:__ @declarations\/opaque_declaration.h 9:8@

    __exported by:__ @declarations\/opaque_declaration.h@
-}
data Baz = Baz
  {}
  deriving stock (Eq, BG.Generic, Show)

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

deriving via Marshal.EquivStorable Baz instance BG.Storable Baz

deriving via Struct.IsStructViaStorable Baz instance Struct.IsStruct Baz

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
