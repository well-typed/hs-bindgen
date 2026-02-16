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

    __defined at:__ @binding-specs\/rep\/emptydata\/opaque.h 1:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/opaque.h@
-}
data Foo

{-| __C declaration:__ @struct bar@

    __defined at:__ @binding-specs\/rep\/emptydata\/opaque.h 3:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/opaque.h@
-}
data Bar = Bar
  { bar_a :: RIP.Ptr Foo
    {- ^ __C declaration:__ @a@

         __defined at:__ @binding-specs\/rep\/emptydata\/opaque.h 4:15@

         __exported by:__ @binding-specs\/rep\/emptydata\/opaque.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCField.readRaw (RIP.Proxy @"bar_a") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_a2 ->
            HasCField.writeRaw (RIP.Proxy @"bar_a") ptr0 bar_a2

deriving via Marshal.EquivStorable Bar instance RIP.Storable Bar

instance HasCField.HasCField Bar "bar_a" where

  type CFieldType Bar "bar_a" = RIP.Ptr Foo

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.Ptr Foo)
         ) => RIP.HasField "bar_a" (RIP.Ptr Bar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bar_a")
