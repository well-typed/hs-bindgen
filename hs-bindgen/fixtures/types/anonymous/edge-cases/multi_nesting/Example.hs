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
    ( Example.SSS_x_x(..)
    , Example.SSS_x(..)
    , Example.SSS(..)
    , Example.USS_x_x(..)
    , Example.USS_x(..)
    , Example.USS(..)
    , Example.get_uSS_x
    , Example.set_uSS_x
    , Example.SUS_x_x(..)
    , Example.SUS_x(..)
    , Example.get_sUS_x_x
    , Example.set_sUS_x_x
    , Example.SUS(..)
    , Example.UUS_x_x(..)
    , Example.UUS_x(..)
    , Example.get_uUS_x_x
    , Example.set_uUS_x_x
    , Example.UUS(..)
    , Example.get_uUS_x
    , Example.set_uUS_x
    , Example.SSU_x_x(..)
    , Example.get_sSU_x_x_x
    , Example.set_sSU_x_x_x
    , Example.SSU_x(..)
    , Example.SSU(..)
    , Example.USU_x_x(..)
    , Example.get_uSU_x_x_x
    , Example.set_uSU_x_x_x
    , Example.USU_x(..)
    , Example.USU(..)
    , Example.get_uSU_x
    , Example.set_uSU_x
    , Example.SUU_x_x(..)
    , Example.get_sUU_x_x_x
    , Example.set_sUU_x_x_x
    , Example.SUU_x(..)
    , Example.get_sUU_x_x
    , Example.set_sUU_x_x
    , Example.SUU(..)
    , Example.UUU_x_x(..)
    , Example.get_uUU_x_x_x
    , Example.set_uUU_x_x_x
    , Example.UUU_x(..)
    , Example.get_uUU_x_x
    , Example.set_uUU_x_x
    , Example.UUU(..)
    , Example.get_uUU_x
    , Example.set_uUU_x
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct \@SSS_x_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 17:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSS_x_x = SSS_x_x
  { sSS_x_x_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 18:11@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize SSS_x_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSS_x_x where

  readRaw =
    \ptr0 ->
          pure SSS_x_x
      <*> HasCField.readRaw (RIP.Proxy @"sSS_x_x_x") ptr0

instance Marshal.WriteRaw SSS_x_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSS_x_x sSS_x_x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"sSS_x_x_x") ptr0 sSS_x_x_x2

deriving via Marshal.EquivStorable SSS_x_x instance RIP.Storable SSS_x_x

instance HasCField.HasCField SSS_x_x "sSS_x_x_x" where

  type CFieldType SSS_x_x "sSS_x_x_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "sSS_x_x_x" (RIP.Ptr SSS_x_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sSS_x_x_x")

{-| __C declaration:__ @struct \@SSS_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 16:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSS_x = SSS_x
  { sSS_x_x :: SSS_x_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 17:5@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize SSS_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSS_x where

  readRaw =
    \ptr0 ->
          pure SSS_x
      <*> HasCField.readRaw (RIP.Proxy @"sSS_x_x") ptr0

instance Marshal.WriteRaw SSS_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSS_x sSS_x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"sSS_x_x") ptr0 sSS_x_x2

deriving via Marshal.EquivStorable SSS_x instance RIP.Storable SSS_x

instance HasCField.HasCField SSS_x "sSS_x_x" where

  type CFieldType SSS_x "sSS_x_x" = SSS_x_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) SSS_x_x
         ) => RIP.HasField "sSS_x_x" (RIP.Ptr SSS_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sSS_x_x")

{-| __C declaration:__ @struct SSS@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 15:8@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSS = SSS
  { sSS_x :: SSS_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 16:3@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize SSS where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSS where

  readRaw =
    \ptr0 ->
          pure SSS
      <*> HasCField.readRaw (RIP.Proxy @"sSS_x") ptr0

instance Marshal.WriteRaw SSS where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSS sSS_x2 ->
            HasCField.writeRaw (RIP.Proxy @"sSS_x") ptr0 sSS_x2

deriving via Marshal.EquivStorable SSS instance RIP.Storable SSS

instance HasCField.HasCField SSS "sSS_x" where

  type CFieldType SSS "sSS_x" = SSS_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) SSS_x
         ) => RIP.HasField "sSS_x" (RIP.Ptr SSS) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sSS_x")

{-| __C declaration:__ @struct \@USS_x_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 25:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data USS_x_x = USS_x_x
  { uSS_x_x_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 26:11@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize USS_x_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw USS_x_x where

  readRaw =
    \ptr0 ->
          pure USS_x_x
      <*> HasCField.readRaw (RIP.Proxy @"uSS_x_x_x") ptr0

instance Marshal.WriteRaw USS_x_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          USS_x_x uSS_x_x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"uSS_x_x_x") ptr0 uSS_x_x_x2

deriving via Marshal.EquivStorable USS_x_x instance RIP.Storable USS_x_x

instance HasCField.HasCField USS_x_x "uSS_x_x_x" where

  type CFieldType USS_x_x "uSS_x_x_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "uSS_x_x_x" (RIP.Ptr USS_x_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uSS_x_x_x")

{-| __C declaration:__ @struct \@USS_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 24:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data USS_x = USS_x
  { uSS_x_x :: USS_x_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 25:5@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize USS_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw USS_x where

  readRaw =
    \ptr0 ->
          pure USS_x
      <*> HasCField.readRaw (RIP.Proxy @"uSS_x_x") ptr0

instance Marshal.WriteRaw USS_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          USS_x uSS_x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"uSS_x_x") ptr0 uSS_x_x2

deriving via Marshal.EquivStorable USS_x instance RIP.Storable USS_x

instance HasCField.HasCField USS_x "uSS_x_x" where

  type CFieldType USS_x "uSS_x_x" = USS_x_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) USS_x_x
         ) => RIP.HasField "uSS_x_x" (RIP.Ptr USS_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uSS_x_x")

{-| __C declaration:__ @union USS@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 23:7@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype USS = USS
  { unwrapUSS :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize USS

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw USS

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw USS

deriving via Marshal.EquivStorable USS instance RIP.Storable USS

{-|

  __See:__ 'set_uSS_x'

__C declaration:__ @x@

__defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 24:3@

__exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uSS_x ::
     USS
  -> USS_x
get_uSS_x = RIP.getUnionPayload

{-|

  __See:__ 'get_uSS_x'

-}
set_uSS_x ::
     USS_x
  -> USS
set_uSS_x = RIP.setUnionPayload

instance HasCField.HasCField USS "uSS_x" where

  type CFieldType USS "uSS_x" = USS_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) USS_x
         ) => RIP.HasField "uSS_x" (RIP.Ptr USS) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uSS_x")

{-| __C declaration:__ @struct \@SUS_x_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 33:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SUS_x_x = SUS_x_x
  { sUS_x_x_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 34:11@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize SUS_x_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SUS_x_x where

  readRaw =
    \ptr0 ->
          pure SUS_x_x
      <*> HasCField.readRaw (RIP.Proxy @"sUS_x_x_x") ptr0

instance Marshal.WriteRaw SUS_x_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SUS_x_x sUS_x_x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"sUS_x_x_x") ptr0 sUS_x_x_x2

deriving via Marshal.EquivStorable SUS_x_x instance RIP.Storable SUS_x_x

instance HasCField.HasCField SUS_x_x "sUS_x_x_x" where

  type CFieldType SUS_x_x "sUS_x_x_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "sUS_x_x_x" (RIP.Ptr SUS_x_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sUS_x_x_x")

{-| __C declaration:__ @union \@SUS_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 32:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype SUS_x = SUS_x
  { unwrapSUS_x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize SUS_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw SUS_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw SUS_x

deriving via Marshal.EquivStorable SUS_x instance RIP.Storable SUS_x

{-|

  __See:__ 'set_sUS_x_x'

__C declaration:__ @x@

__defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 33:5@

__exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_sUS_x_x ::
     SUS_x
  -> SUS_x_x
get_sUS_x_x = RIP.getUnionPayload

{-|

  __See:__ 'get_sUS_x_x'

-}
set_sUS_x_x ::
     SUS_x_x
  -> SUS_x
set_sUS_x_x = RIP.setUnionPayload

instance HasCField.HasCField SUS_x "sUS_x_x" where

  type CFieldType SUS_x "sUS_x_x" = SUS_x_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) SUS_x_x
         ) => RIP.HasField "sUS_x_x" (RIP.Ptr SUS_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sUS_x_x")

{-| __C declaration:__ @struct SUS@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 31:8@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SUS = SUS
  { sUS_x :: SUS_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 32:3@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (RIP.Generic)

instance Marshal.StaticSize SUS where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SUS where

  readRaw =
    \ptr0 ->
          pure SUS
      <*> HasCField.readRaw (RIP.Proxy @"sUS_x") ptr0

instance Marshal.WriteRaw SUS where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SUS sUS_x2 ->
            HasCField.writeRaw (RIP.Proxy @"sUS_x") ptr0 sUS_x2

deriving via Marshal.EquivStorable SUS instance RIP.Storable SUS

instance HasCField.HasCField SUS "sUS_x" where

  type CFieldType SUS "sUS_x" = SUS_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) SUS_x
         ) => RIP.HasField "sUS_x" (RIP.Ptr SUS) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sUS_x")

{-| __C declaration:__ @struct \@UUS_x_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 41:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data UUS_x_x = UUS_x_x
  { uUS_x_x_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 42:11@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize UUS_x_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UUS_x_x where

  readRaw =
    \ptr0 ->
          pure UUS_x_x
      <*> HasCField.readRaw (RIP.Proxy @"uUS_x_x_x") ptr0

instance Marshal.WriteRaw UUS_x_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UUS_x_x uUS_x_x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"uUS_x_x_x") ptr0 uUS_x_x_x2

deriving via Marshal.EquivStorable UUS_x_x instance RIP.Storable UUS_x_x

instance HasCField.HasCField UUS_x_x "uUS_x_x_x" where

  type CFieldType UUS_x_x "uUS_x_x_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "uUS_x_x_x" (RIP.Ptr UUS_x_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uUS_x_x_x")

{-| __C declaration:__ @union \@UUS_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 40:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUS_x = UUS_x
  { unwrapUUS_x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize UUS_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw UUS_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw UUS_x

deriving via Marshal.EquivStorable UUS_x instance RIP.Storable UUS_x

{-|

  __See:__ 'set_uUS_x_x'

__C declaration:__ @x@

__defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 41:5@

__exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uUS_x_x ::
     UUS_x
  -> UUS_x_x
get_uUS_x_x = RIP.getUnionPayload

{-|

  __See:__ 'get_uUS_x_x'

-}
set_uUS_x_x ::
     UUS_x_x
  -> UUS_x
set_uUS_x_x = RIP.setUnionPayload

instance HasCField.HasCField UUS_x "uUS_x_x" where

  type CFieldType UUS_x "uUS_x_x" = UUS_x_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) UUS_x_x
         ) => RIP.HasField "uUS_x_x" (RIP.Ptr UUS_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uUS_x_x")

{-| __C declaration:__ @union UUS@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 39:7@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUS = UUS
  { unwrapUUS :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize UUS

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw UUS

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw UUS

deriving via Marshal.EquivStorable UUS instance RIP.Storable UUS

{-|

  __See:__ 'set_uUS_x'

__C declaration:__ @x@

__defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 40:3@

__exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uUS_x ::
     UUS
  -> UUS_x
get_uUS_x = RIP.getUnionPayload

{-|

  __See:__ 'get_uUS_x'

-}
set_uUS_x ::
     UUS_x
  -> UUS
set_uUS_x = RIP.setUnionPayload

instance HasCField.HasCField UUS "uUS_x" where

  type CFieldType UUS "uUS_x" = UUS_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) UUS_x
         ) => RIP.HasField "uUS_x" (RIP.Ptr UUS) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uUS_x")

{-| __C declaration:__ @union \@SSU_x_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 49:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype SSU_x_x = SSU_x_x
  { unwrapSSU_x_x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize SSU_x_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw SSU_x_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw SSU_x_x

deriving via Marshal.EquivStorable SSU_x_x instance RIP.Storable SSU_x_x

{-|

  __See:__ 'set_sSU_x_x_x'

__C declaration:__ @x@

__defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 50:11@

__exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_sSU_x_x_x ::
     SSU_x_x
  -> RIP.CInt
get_sSU_x_x_x = RIP.getUnionPayload

{-|

  __See:__ 'get_sSU_x_x_x'

-}
set_sSU_x_x_x ::
     RIP.CInt
  -> SSU_x_x
set_sSU_x_x_x = RIP.setUnionPayload

instance HasCField.HasCField SSU_x_x "sSU_x_x_x" where

  type CFieldType SSU_x_x "sSU_x_x_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "sSU_x_x_x" (RIP.Ptr SSU_x_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sSU_x_x_x")

{-| __C declaration:__ @struct \@SSU_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 48:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSU_x = SSU_x
  { sSU_x_x :: SSU_x_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 49:5@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (RIP.Generic)

instance Marshal.StaticSize SSU_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSU_x where

  readRaw =
    \ptr0 ->
          pure SSU_x
      <*> HasCField.readRaw (RIP.Proxy @"sSU_x_x") ptr0

instance Marshal.WriteRaw SSU_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSU_x sSU_x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"sSU_x_x") ptr0 sSU_x_x2

deriving via Marshal.EquivStorable SSU_x instance RIP.Storable SSU_x

instance HasCField.HasCField SSU_x "sSU_x_x" where

  type CFieldType SSU_x "sSU_x_x" = SSU_x_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) SSU_x_x
         ) => RIP.HasField "sSU_x_x" (RIP.Ptr SSU_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sSU_x_x")

{-| __C declaration:__ @struct SSU@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 47:8@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSU = SSU
  { sSU_x :: SSU_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 48:3@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (RIP.Generic)

instance Marshal.StaticSize SSU where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSU where

  readRaw =
    \ptr0 ->
          pure SSU
      <*> HasCField.readRaw (RIP.Proxy @"sSU_x") ptr0

instance Marshal.WriteRaw SSU where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSU sSU_x2 ->
            HasCField.writeRaw (RIP.Proxy @"sSU_x") ptr0 sSU_x2

deriving via Marshal.EquivStorable SSU instance RIP.Storable SSU

instance HasCField.HasCField SSU "sSU_x" where

  type CFieldType SSU "sSU_x" = SSU_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) SSU_x
         ) => RIP.HasField "sSU_x" (RIP.Ptr SSU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sSU_x")

{-| __C declaration:__ @union \@USU_x_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 57:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype USU_x_x = USU_x_x
  { unwrapUSU_x_x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize USU_x_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw USU_x_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw USU_x_x

deriving via Marshal.EquivStorable USU_x_x instance RIP.Storable USU_x_x

{-|

  __See:__ 'set_uSU_x_x_x'

__C declaration:__ @x@

__defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 58:11@

__exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uSU_x_x_x ::
     USU_x_x
  -> RIP.CInt
get_uSU_x_x_x = RIP.getUnionPayload

{-|

  __See:__ 'get_uSU_x_x_x'

-}
set_uSU_x_x_x ::
     RIP.CInt
  -> USU_x_x
set_uSU_x_x_x = RIP.setUnionPayload

instance HasCField.HasCField USU_x_x "uSU_x_x_x" where

  type CFieldType USU_x_x "uSU_x_x_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "uSU_x_x_x" (RIP.Ptr USU_x_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uSU_x_x_x")

{-| __C declaration:__ @struct \@USU_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 56:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data USU_x = USU_x
  { uSU_x_x :: USU_x_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 57:5@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (RIP.Generic)

instance Marshal.StaticSize USU_x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw USU_x where

  readRaw =
    \ptr0 ->
          pure USU_x
      <*> HasCField.readRaw (RIP.Proxy @"uSU_x_x") ptr0

instance Marshal.WriteRaw USU_x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          USU_x uSU_x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"uSU_x_x") ptr0 uSU_x_x2

deriving via Marshal.EquivStorable USU_x instance RIP.Storable USU_x

instance HasCField.HasCField USU_x "uSU_x_x" where

  type CFieldType USU_x "uSU_x_x" = USU_x_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) USU_x_x
         ) => RIP.HasField "uSU_x_x" (RIP.Ptr USU_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uSU_x_x")

{-| __C declaration:__ @union USU@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 55:7@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype USU = USU
  { unwrapUSU :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize USU

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw USU

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw USU

deriving via Marshal.EquivStorable USU instance RIP.Storable USU

{-|

  __See:__ 'set_uSU_x'

__C declaration:__ @x@

__defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 56:3@

__exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uSU_x ::
     USU
  -> USU_x
get_uSU_x = RIP.getUnionPayload

{-|

  __See:__ 'get_uSU_x'

-}
set_uSU_x ::
     USU_x
  -> USU
set_uSU_x = RIP.setUnionPayload

instance HasCField.HasCField USU "uSU_x" where

  type CFieldType USU "uSU_x" = USU_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) USU_x
         ) => RIP.HasField "uSU_x" (RIP.Ptr USU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uSU_x")

{-| __C declaration:__ @union \@SUU_x_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 65:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype SUU_x_x = SUU_x_x
  { unwrapSUU_x_x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize SUU_x_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw SUU_x_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw SUU_x_x

deriving via Marshal.EquivStorable SUU_x_x instance RIP.Storable SUU_x_x

{-|

  __See:__ 'set_sUU_x_x_x'

__C declaration:__ @x@

__defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 66:11@

__exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_sUU_x_x_x ::
     SUU_x_x
  -> RIP.CInt
get_sUU_x_x_x = RIP.getUnionPayload

{-|

  __See:__ 'get_sUU_x_x_x'

-}
set_sUU_x_x_x ::
     RIP.CInt
  -> SUU_x_x
set_sUU_x_x_x = RIP.setUnionPayload

instance HasCField.HasCField SUU_x_x "sUU_x_x_x" where

  type CFieldType SUU_x_x "sUU_x_x_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "sUU_x_x_x" (RIP.Ptr SUU_x_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sUU_x_x_x")

{-| __C declaration:__ @union \@SUU_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 64:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype SUU_x = SUU_x
  { unwrapSUU_x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize SUU_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw SUU_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw SUU_x

deriving via Marshal.EquivStorable SUU_x instance RIP.Storable SUU_x

{-|

  __See:__ 'set_sUU_x_x'

__C declaration:__ @x@

__defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 65:5@

__exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_sUU_x_x ::
     SUU_x
  -> SUU_x_x
get_sUU_x_x = RIP.getUnionPayload

{-|

  __See:__ 'get_sUU_x_x'

-}
set_sUU_x_x ::
     SUU_x_x
  -> SUU_x
set_sUU_x_x = RIP.setUnionPayload

instance HasCField.HasCField SUU_x "sUU_x_x" where

  type CFieldType SUU_x "sUU_x_x" = SUU_x_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) SUU_x_x
         ) => RIP.HasField "sUU_x_x" (RIP.Ptr SUU_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sUU_x_x")

{-| __C declaration:__ @struct SUU@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 63:8@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SUU = SUU
  { sUU_x :: SUU_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 64:3@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (RIP.Generic)

instance Marshal.StaticSize SUU where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SUU where

  readRaw =
    \ptr0 ->
          pure SUU
      <*> HasCField.readRaw (RIP.Proxy @"sUU_x") ptr0

instance Marshal.WriteRaw SUU where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SUU sUU_x2 ->
            HasCField.writeRaw (RIP.Proxy @"sUU_x") ptr0 sUU_x2

deriving via Marshal.EquivStorable SUU instance RIP.Storable SUU

instance HasCField.HasCField SUU "sUU_x" where

  type CFieldType SUU "sUU_x" = SUU_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) SUU_x
         ) => RIP.HasField "sUU_x" (RIP.Ptr SUU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sUU_x")

{-| __C declaration:__ @union \@UUU_x_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 73:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUU_x_x = UUU_x_x
  { unwrapUUU_x_x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize UUU_x_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw UUU_x_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw UUU_x_x

deriving via Marshal.EquivStorable UUU_x_x instance RIP.Storable UUU_x_x

{-|

  __See:__ 'set_uUU_x_x_x'

__C declaration:__ @x@

__defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 74:11@

__exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uUU_x_x_x ::
     UUU_x_x
  -> RIP.CInt
get_uUU_x_x_x = RIP.getUnionPayload

{-|

  __See:__ 'get_uUU_x_x_x'

-}
set_uUU_x_x_x ::
     RIP.CInt
  -> UUU_x_x
set_uUU_x_x_x = RIP.setUnionPayload

instance HasCField.HasCField UUU_x_x "uUU_x_x_x" where

  type CFieldType UUU_x_x "uUU_x_x_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "uUU_x_x_x" (RIP.Ptr UUU_x_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uUU_x_x_x")

{-| __C declaration:__ @union \@UUU_x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 72:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUU_x = UUU_x
  { unwrapUUU_x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize UUU_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw UUU_x

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw UUU_x

deriving via Marshal.EquivStorable UUU_x instance RIP.Storable UUU_x

{-|

  __See:__ 'set_uUU_x_x'

__C declaration:__ @x@

__defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 73:5@

__exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uUU_x_x ::
     UUU_x
  -> UUU_x_x
get_uUU_x_x = RIP.getUnionPayload

{-|

  __See:__ 'get_uUU_x_x'

-}
set_uUU_x_x ::
     UUU_x_x
  -> UUU_x
set_uUU_x_x = RIP.setUnionPayload

instance HasCField.HasCField UUU_x "uUU_x_x" where

  type CFieldType UUU_x "uUU_x_x" = UUU_x_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) UUU_x_x
         ) => RIP.HasField "uUU_x_x" (RIP.Ptr UUU_x) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uUU_x_x")

{-| __C declaration:__ @union UUU@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 71:7@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUU = UUU
  { unwrapUUU :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize UUU

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw UUU

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw UUU

deriving via Marshal.EquivStorable UUU instance RIP.Storable UUU

{-|

  __See:__ 'set_uUU_x'

__C declaration:__ @x@

__defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 72:3@

__exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uUU_x ::
     UUU
  -> UUU_x
get_uUU_x = RIP.getUnionPayload

{-|

  __See:__ 'get_uUU_x'

-}
set_uUU_x ::
     UUU_x
  -> UUU
set_uUU_x = RIP.setUnionPayload

instance HasCField.HasCField UUU "uUU_x" where

  type CFieldType UUU "uUU_x" = UUU_x

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) UUU_x
         ) => RIP.HasField "uUU_x" (RIP.Ptr UUU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uUU_x")
