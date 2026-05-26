{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.S(..)
    , Example.T(..)
    , Example.Foo(..)
    , Example.Bar(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct S@

    __defined at:__ @macros\/reparse\/defer_wrong.h 11:8@

    __exported by:__ @macros\/reparse\/defer_wrong.h@
-}
data S = S
  { s_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/defer_wrong.h 11:16@

         __exported by:__ @macros\/reparse\/defer_wrong.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (RIP.Proxy @"s_x") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s_x") ptr0 s_x2

deriving via Marshal.EquivStorable S instance RIP.Storable S

instance HasCField.HasCField S "s_x" where

  type CFieldType S "s_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.HasField "s_x" (RIP.Ptr S) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s_x")

{-| __C declaration:__ @macro T@

    __defined at:__ @macros\/reparse\/defer_wrong.h 13:9@

    __exported by:__ @macros\/reparse\/defer_wrong.h@
-}
newtype T = T
  { unwrapT :: S
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ S) => RIP.HasField "unwrapT" (RIP.Ptr T) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = S

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/defer_wrong.h 15:11@

    __exported by:__ @macros\/reparse\/defer_wrong.h@
-}
newtype Foo = Foo
  { unwrapFoo :: S
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ S) => RIP.HasField "unwrapFoo" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapFoo")

instance HasCField.HasCField Foo "unwrapFoo" where

  type CFieldType Foo "unwrapFoo" = S

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/defer_wrong.h 16:13@

    __exported by:__ @macros\/reparse\/defer_wrong.h@
-}
newtype Bar = Bar
  { unwrapBar :: RIP.Ptr T
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.Ptr T
         ) => RIP.HasField "unwrapBar" (RIP.Ptr Bar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapBar")

instance HasCField.HasCField Bar "unwrapBar" where

  type CFieldType Bar "unwrapBar" = RIP.Ptr T

  offset# = \_ -> \_ -> 0
