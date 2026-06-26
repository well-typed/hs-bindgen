{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
    ( Example.SSS_anon'anon'x_anon'x(..)
    , Example.SSS_anon'anon'x(..)
    , Example.SSS(..)
    , Example.USS_anon'anon'x_anon'x(..)
    , Example.USS_anon'anon'x(..)
    , Example.USS(..)
    , Example.get_uSS_anon'anon'x
    , Example.set_uSS_anon'anon'x
    , Example.SUS_anon'anon'x_anon'x(..)
    , Example.SUS_anon'anon'x(..)
    , Example.get_sUS_anon'anon'x_anon'x
    , Example.set_sUS_anon'anon'x_anon'x
    , Example.SUS(..)
    , Example.UUS_anon'anon'x_anon'x(..)
    , Example.UUS_anon'anon'x(..)
    , Example.get_uUS_anon'anon'x_anon'x
    , Example.set_uUS_anon'anon'x_anon'x
    , Example.UUS(..)
    , Example.get_uUS_anon'anon'x
    , Example.set_uUS_anon'anon'x
    , Example.SSU_anon'anon'x_anon'x(..)
    , Example.get_sSU_anon'anon'x_anon'x_x
    , Example.set_sSU_anon'anon'x_anon'x_x
    , Example.SSU_anon'anon'x(..)
    , Example.SSU(..)
    , Example.USU_anon'anon'x_anon'x(..)
    , Example.get_uSU_anon'anon'x_anon'x_x
    , Example.set_uSU_anon'anon'x_anon'x_x
    , Example.USU_anon'anon'x(..)
    , Example.USU(..)
    , Example.get_uSU_anon'anon'x
    , Example.set_uSU_anon'anon'x
    , Example.SUU_anon'anon'x_anon'x(..)
    , Example.get_sUU_anon'anon'x_anon'x_x
    , Example.set_sUU_anon'anon'x_anon'x_x
    , Example.SUU_anon'anon'x(..)
    , Example.get_sUU_anon'anon'x_anon'x
    , Example.set_sUU_anon'anon'x_anon'x
    , Example.SUU(..)
    , Example.UUU_anon'anon'x_anon'x(..)
    , Example.get_uUU_anon'anon'x_anon'x_x
    , Example.set_uUU_anon'anon'x_anon'x_x
    , Example.UUU_anon'anon'x(..)
    , Example.get_uUU_anon'anon'x_anon'x
    , Example.set_uUU_anon'anon'x_anon'x
    , Example.UUU(..)
    , Example.get_uUU_anon'anon'x
    , Example.set_uUU_anon'anon'x
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct \@SSS_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 17:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSS_anon'anon'x_anon'x = SSS_anon'anon'x_anon'x
  { sSS_anon'anon'x_anon'x_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 18:11@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize SSS_anon'anon'x_anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSS_anon'anon'x_anon'x where

  readRaw =
    \ptr0 ->
          pure SSS_anon'anon'x_anon'x
      <*> HasCField.readRaw (RIP.Proxy @"sSS_anon'anon'x_anon'x_x") ptr0

instance Marshal.WriteRaw SSS_anon'anon'x_anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSS_anon'anon'x_anon'x sSS_anon'anon'x_anon'x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"sSS_anon'anon'x_anon'x_x") ptr0 sSS_anon'anon'x_anon'x_x2

deriving via Marshal.EquivStorable SSS_anon'anon'x_anon'x instance RIP.Storable SSS_anon'anon'x_anon'x

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "sSS_anon'anon'x_anon'x_x" SSS_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SSS_anon'anon'x_anon'x {sSS_anon'anon'x_anon'x_x = y1}
      , RIP.getField @"sSS_anon'anon'x_anon'x_x" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "sSS_anon'anon'x_anon'x_x" (RIP.Ptr SSS_anon'anon'x_anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sSS_anon'anon'x_anon'x_x")

instance HasCField.HasCField SSS_anon'anon'x_anon'x "sSS_anon'anon'x_anon'x_x" where

  type CFieldType SSS_anon'anon'x_anon'x "sSS_anon'anon'x_anon'x_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@SSS_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 16:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSS_anon'anon'x = SSS_anon'anon'x
  { sSS_anon'anon'x_anon'x :: SSS_anon'anon'x_anon'x
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 17:5@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize SSS_anon'anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSS_anon'anon'x where

  readRaw =
    \ptr0 ->
          pure SSS_anon'anon'x
      <*> HasCField.readRaw (RIP.Proxy @"sSS_anon'anon'x_anon'x") ptr0

instance Marshal.WriteRaw SSS_anon'anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSS_anon'anon'x sSS_anon'anon'x_anon'x2 ->
            HasCField.writeRaw (RIP.Proxy @"sSS_anon'anon'x_anon'x") ptr0 sSS_anon'anon'x_anon'x2

deriving via Marshal.EquivStorable SSS_anon'anon'x instance RIP.Storable SSS_anon'anon'x

instance ( ty ~ SSS_anon'anon'x_anon'x
         ) => RIP.CompatHasField.HasField "sSS_anon'anon'x_anon'x" SSS_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SSS_anon'anon'x {sSS_anon'anon'x_anon'x = y1}
      , RIP.getField @"sSS_anon'anon'x_anon'x" x0
      )

instance ( ty ~ SSS_anon'anon'x_anon'x
         ) => RIP.HasField "sSS_anon'anon'x_anon'x" (RIP.Ptr SSS_anon'anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sSS_anon'anon'x_anon'x")

instance HasCField.HasCField SSS_anon'anon'x "sSS_anon'anon'x_anon'x" where

  type CFieldType SSS_anon'anon'x "sSS_anon'anon'x_anon'x" =
    SSS_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct SSS@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 15:8@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSS = SSS
  { sSS_anon'anon'x :: SSS_anon'anon'x
    {- ^ __C declaration:__ @anon\'anon\'x@

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
      <*> HasCField.readRaw (RIP.Proxy @"sSS_anon'anon'x") ptr0

instance Marshal.WriteRaw SSS where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSS sSS_anon'anon'x2 ->
            HasCField.writeRaw (RIP.Proxy @"sSS_anon'anon'x") ptr0 sSS_anon'anon'x2

deriving via Marshal.EquivStorable SSS instance RIP.Storable SSS

instance ( ty ~ SSS_anon'anon'x
         ) => RIP.CompatHasField.HasField "sSS_anon'anon'x" SSS ty where

  hasField =
    \x0 ->
      (\y1 ->
         SSS {sSS_anon'anon'x = y1}, RIP.getField @"sSS_anon'anon'x" x0)

instance ( ty ~ SSS_anon'anon'x
         ) => RIP.HasField "sSS_anon'anon'x" (RIP.Ptr SSS) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sSS_anon'anon'x")

instance HasCField.HasCField SSS "sSS_anon'anon'x" where

  type CFieldType SSS "sSS_anon'anon'x" =
    SSS_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@USS_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 25:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data USS_anon'anon'x_anon'x = USS_anon'anon'x_anon'x
  { uSS_anon'anon'x_anon'x_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 26:11@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize USS_anon'anon'x_anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw USS_anon'anon'x_anon'x where

  readRaw =
    \ptr0 ->
          pure USS_anon'anon'x_anon'x
      <*> HasCField.readRaw (RIP.Proxy @"uSS_anon'anon'x_anon'x_x") ptr0

instance Marshal.WriteRaw USS_anon'anon'x_anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          USS_anon'anon'x_anon'x uSS_anon'anon'x_anon'x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"uSS_anon'anon'x_anon'x_x") ptr0 uSS_anon'anon'x_anon'x_x2

deriving via Marshal.EquivStorable USS_anon'anon'x_anon'x instance RIP.Storable USS_anon'anon'x_anon'x

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "uSS_anon'anon'x_anon'x_x" USS_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          USS_anon'anon'x_anon'x {uSS_anon'anon'x_anon'x_x = y1}
      , RIP.getField @"uSS_anon'anon'x_anon'x_x" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "uSS_anon'anon'x_anon'x_x" (RIP.Ptr USS_anon'anon'x_anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uSS_anon'anon'x_anon'x_x")

instance HasCField.HasCField USS_anon'anon'x_anon'x "uSS_anon'anon'x_anon'x_x" where

  type CFieldType USS_anon'anon'x_anon'x "uSS_anon'anon'x_anon'x_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@USS_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 24:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data USS_anon'anon'x = USS_anon'anon'x
  { uSS_anon'anon'x_anon'x :: USS_anon'anon'x_anon'x
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 25:5@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize USS_anon'anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw USS_anon'anon'x where

  readRaw =
    \ptr0 ->
          pure USS_anon'anon'x
      <*> HasCField.readRaw (RIP.Proxy @"uSS_anon'anon'x_anon'x") ptr0

instance Marshal.WriteRaw USS_anon'anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          USS_anon'anon'x uSS_anon'anon'x_anon'x2 ->
            HasCField.writeRaw (RIP.Proxy @"uSS_anon'anon'x_anon'x") ptr0 uSS_anon'anon'x_anon'x2

deriving via Marshal.EquivStorable USS_anon'anon'x instance RIP.Storable USS_anon'anon'x

instance ( ty ~ USS_anon'anon'x_anon'x
         ) => RIP.CompatHasField.HasField "uSS_anon'anon'x_anon'x" USS_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          USS_anon'anon'x {uSS_anon'anon'x_anon'x = y1}
      , RIP.getField @"uSS_anon'anon'x_anon'x" x0
      )

instance ( ty ~ USS_anon'anon'x_anon'x
         ) => RIP.HasField "uSS_anon'anon'x_anon'x" (RIP.Ptr USS_anon'anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uSS_anon'anon'x_anon'x")

instance HasCField.HasCField USS_anon'anon'x "uSS_anon'anon'x_anon'x" where

  type CFieldType USS_anon'anon'x "uSS_anon'anon'x_anon'x" =
    USS_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union USS@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 23:7@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype USS = USS
  { unwrapUSS :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize USS

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw USS

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw USS

deriving via Marshal.EquivStorable USS instance RIP.Storable USS

{-|

    __See:__ 'set_uSS_anon'anon'x'

    __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 24:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uSS_anon'anon'x ::
     USS
  -> USS_anon'anon'x
get_uSS_anon'anon'x = RIP.getUnionPayload

{-|

    __See:__ 'get_uSS_anon'anon'x'

-}
set_uSS_anon'anon'x ::
     USS_anon'anon'x
  -> USS
set_uSS_anon'anon'x = RIP.setUnionPayload

instance ( ty ~ USS_anon'anon'x
         ) => RIP.HasField "uSS_anon'anon'x" (RIP.Ptr USS) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uSS_anon'anon'x")

instance HasCField.HasCField USS "uSS_anon'anon'x" where

  type CFieldType USS "uSS_anon'anon'x" =
    USS_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@SUS_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 33:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SUS_anon'anon'x_anon'x = SUS_anon'anon'x_anon'x
  { sUS_anon'anon'x_anon'x_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 34:11@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize SUS_anon'anon'x_anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SUS_anon'anon'x_anon'x where

  readRaw =
    \ptr0 ->
          pure SUS_anon'anon'x_anon'x
      <*> HasCField.readRaw (RIP.Proxy @"sUS_anon'anon'x_anon'x_x") ptr0

instance Marshal.WriteRaw SUS_anon'anon'x_anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SUS_anon'anon'x_anon'x sUS_anon'anon'x_anon'x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"sUS_anon'anon'x_anon'x_x") ptr0 sUS_anon'anon'x_anon'x_x2

deriving via Marshal.EquivStorable SUS_anon'anon'x_anon'x instance RIP.Storable SUS_anon'anon'x_anon'x

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "sUS_anon'anon'x_anon'x_x" SUS_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SUS_anon'anon'x_anon'x {sUS_anon'anon'x_anon'x_x = y1}
      , RIP.getField @"sUS_anon'anon'x_anon'x_x" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "sUS_anon'anon'x_anon'x_x" (RIP.Ptr SUS_anon'anon'x_anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sUS_anon'anon'x_anon'x_x")

instance HasCField.HasCField SUS_anon'anon'x_anon'x "sUS_anon'anon'x_anon'x_x" where

  type CFieldType SUS_anon'anon'x_anon'x "sUS_anon'anon'x_anon'x_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@SUS_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 32:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype SUS_anon'anon'x = SUS_anon'anon'x
  { unwrapSUS_anon'anon'x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize SUS_anon'anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw SUS_anon'anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw SUS_anon'anon'x

deriving via Marshal.EquivStorable SUS_anon'anon'x instance RIP.Storable SUS_anon'anon'x

{-|

    __See:__ 'set_sUS_anon'anon'x_anon'x'

    __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 33:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_sUS_anon'anon'x_anon'x ::
     SUS_anon'anon'x
  -> SUS_anon'anon'x_anon'x
get_sUS_anon'anon'x_anon'x = RIP.getUnionPayload

{-|

    __See:__ 'get_sUS_anon'anon'x_anon'x'

-}
set_sUS_anon'anon'x_anon'x ::
     SUS_anon'anon'x_anon'x
  -> SUS_anon'anon'x
set_sUS_anon'anon'x_anon'x = RIP.setUnionPayload

instance ( ty ~ SUS_anon'anon'x_anon'x
         ) => RIP.HasField "sUS_anon'anon'x_anon'x" (RIP.Ptr SUS_anon'anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sUS_anon'anon'x_anon'x")

instance HasCField.HasCField SUS_anon'anon'x "sUS_anon'anon'x_anon'x" where

  type CFieldType SUS_anon'anon'x "sUS_anon'anon'x_anon'x" =
    SUS_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct SUS@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 31:8@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SUS = SUS
  { sUS_anon'anon'x :: SUS_anon'anon'x
    {- ^ __C declaration:__ @anon\'anon\'x@

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
      <*> HasCField.readRaw (RIP.Proxy @"sUS_anon'anon'x") ptr0

instance Marshal.WriteRaw SUS where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SUS sUS_anon'anon'x2 ->
            HasCField.writeRaw (RIP.Proxy @"sUS_anon'anon'x") ptr0 sUS_anon'anon'x2

deriving via Marshal.EquivStorable SUS instance RIP.Storable SUS

instance ( ty ~ SUS_anon'anon'x
         ) => RIP.CompatHasField.HasField "sUS_anon'anon'x" SUS ty where

  hasField =
    \x0 ->
      (\y1 ->
         SUS {sUS_anon'anon'x = y1}, RIP.getField @"sUS_anon'anon'x" x0)

instance ( ty ~ SUS_anon'anon'x
         ) => RIP.HasField "sUS_anon'anon'x" (RIP.Ptr SUS) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sUS_anon'anon'x")

instance HasCField.HasCField SUS "sUS_anon'anon'x" where

  type CFieldType SUS "sUS_anon'anon'x" =
    SUS_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@UUS_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 41:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data UUS_anon'anon'x_anon'x = UUS_anon'anon'x_anon'x
  { uUS_anon'anon'x_anon'x_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 42:11@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize UUS_anon'anon'x_anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw UUS_anon'anon'x_anon'x where

  readRaw =
    \ptr0 ->
          pure UUS_anon'anon'x_anon'x
      <*> HasCField.readRaw (RIP.Proxy @"uUS_anon'anon'x_anon'x_x") ptr0

instance Marshal.WriteRaw UUS_anon'anon'x_anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UUS_anon'anon'x_anon'x uUS_anon'anon'x_anon'x_x2 ->
            HasCField.writeRaw (RIP.Proxy @"uUS_anon'anon'x_anon'x_x") ptr0 uUS_anon'anon'x_anon'x_x2

deriving via Marshal.EquivStorable UUS_anon'anon'x_anon'x instance RIP.Storable UUS_anon'anon'x_anon'x

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "uUS_anon'anon'x_anon'x_x" UUS_anon'anon'x_anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UUS_anon'anon'x_anon'x {uUS_anon'anon'x_anon'x_x = y1}
      , RIP.getField @"uUS_anon'anon'x_anon'x_x" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "uUS_anon'anon'x_anon'x_x" (RIP.Ptr UUS_anon'anon'x_anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uUS_anon'anon'x_anon'x_x")

instance HasCField.HasCField UUS_anon'anon'x_anon'x "uUS_anon'anon'x_anon'x_x" where

  type CFieldType UUS_anon'anon'x_anon'x "uUS_anon'anon'x_anon'x_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@UUS_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 40:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUS_anon'anon'x = UUS_anon'anon'x
  { unwrapUUS_anon'anon'x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize UUS_anon'anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw UUS_anon'anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw UUS_anon'anon'x

deriving via Marshal.EquivStorable UUS_anon'anon'x instance RIP.Storable UUS_anon'anon'x

{-|

    __See:__ 'set_uUS_anon'anon'x_anon'x'

    __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 41:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uUS_anon'anon'x_anon'x ::
     UUS_anon'anon'x
  -> UUS_anon'anon'x_anon'x
get_uUS_anon'anon'x_anon'x = RIP.getUnionPayload

{-|

    __See:__ 'get_uUS_anon'anon'x_anon'x'

-}
set_uUS_anon'anon'x_anon'x ::
     UUS_anon'anon'x_anon'x
  -> UUS_anon'anon'x
set_uUS_anon'anon'x_anon'x = RIP.setUnionPayload

instance ( ty ~ UUS_anon'anon'x_anon'x
         ) => RIP.HasField "uUS_anon'anon'x_anon'x" (RIP.Ptr UUS_anon'anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uUS_anon'anon'x_anon'x")

instance HasCField.HasCField UUS_anon'anon'x "uUS_anon'anon'x_anon'x" where

  type CFieldType UUS_anon'anon'x "uUS_anon'anon'x_anon'x" =
    UUS_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union UUS@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 39:7@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUS = UUS
  { unwrapUUS :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize UUS

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw UUS

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw UUS

deriving via Marshal.EquivStorable UUS instance RIP.Storable UUS

{-|

    __See:__ 'set_uUS_anon'anon'x'

    __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 40:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uUS_anon'anon'x ::
     UUS
  -> UUS_anon'anon'x
get_uUS_anon'anon'x = RIP.getUnionPayload

{-|

    __See:__ 'get_uUS_anon'anon'x'

-}
set_uUS_anon'anon'x ::
     UUS_anon'anon'x
  -> UUS
set_uUS_anon'anon'x = RIP.setUnionPayload

instance ( ty ~ UUS_anon'anon'x
         ) => RIP.HasField "uUS_anon'anon'x" (RIP.Ptr UUS) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uUS_anon'anon'x")

instance HasCField.HasCField UUS "uUS_anon'anon'x" where

  type CFieldType UUS "uUS_anon'anon'x" =
    UUS_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@SSU_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 49:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype SSU_anon'anon'x_anon'x = SSU_anon'anon'x_anon'x
  { unwrapSSU_anon'anon'x_anon'x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize SSU_anon'anon'x_anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw SSU_anon'anon'x_anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw SSU_anon'anon'x_anon'x

deriving via Marshal.EquivStorable SSU_anon'anon'x_anon'x instance RIP.Storable SSU_anon'anon'x_anon'x

{-|

    __See:__ 'set_sSU_anon'anon'x_anon'x_x'

    __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 50:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_sSU_anon'anon'x_anon'x_x ::
     SSU_anon'anon'x_anon'x
  -> RIP.CInt
get_sSU_anon'anon'x_anon'x_x = RIP.getUnionPayload

{-|

    __See:__ 'get_sSU_anon'anon'x_anon'x_x'

-}
set_sSU_anon'anon'x_anon'x_x ::
     RIP.CInt
  -> SSU_anon'anon'x_anon'x
set_sSU_anon'anon'x_anon'x_x = RIP.setUnionPayload

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "sSU_anon'anon'x_anon'x_x" (RIP.Ptr SSU_anon'anon'x_anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sSU_anon'anon'x_anon'x_x")

instance HasCField.HasCField SSU_anon'anon'x_anon'x "sSU_anon'anon'x_anon'x_x" where

  type CFieldType SSU_anon'anon'x_anon'x "sSU_anon'anon'x_anon'x_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@SSU_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 48:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSU_anon'anon'x = SSU_anon'anon'x
  { sSU_anon'anon'x_anon'x :: SSU_anon'anon'x_anon'x
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 49:5@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (RIP.Generic)

instance Marshal.StaticSize SSU_anon'anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SSU_anon'anon'x where

  readRaw =
    \ptr0 ->
          pure SSU_anon'anon'x
      <*> HasCField.readRaw (RIP.Proxy @"sSU_anon'anon'x_anon'x") ptr0

instance Marshal.WriteRaw SSU_anon'anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSU_anon'anon'x sSU_anon'anon'x_anon'x2 ->
            HasCField.writeRaw (RIP.Proxy @"sSU_anon'anon'x_anon'x") ptr0 sSU_anon'anon'x_anon'x2

deriving via Marshal.EquivStorable SSU_anon'anon'x instance RIP.Storable SSU_anon'anon'x

instance ( ty ~ SSU_anon'anon'x_anon'x
         ) => RIP.CompatHasField.HasField "sSU_anon'anon'x_anon'x" SSU_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SSU_anon'anon'x {sSU_anon'anon'x_anon'x = y1}
      , RIP.getField @"sSU_anon'anon'x_anon'x" x0
      )

instance ( ty ~ SSU_anon'anon'x_anon'x
         ) => RIP.HasField "sSU_anon'anon'x_anon'x" (RIP.Ptr SSU_anon'anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sSU_anon'anon'x_anon'x")

instance HasCField.HasCField SSU_anon'anon'x "sSU_anon'anon'x_anon'x" where

  type CFieldType SSU_anon'anon'x "sSU_anon'anon'x_anon'x" =
    SSU_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct SSU@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 47:8@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SSU = SSU
  { sSU_anon'anon'x :: SSU_anon'anon'x
    {- ^ __C declaration:__ @anon\'anon\'x@

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
      <*> HasCField.readRaw (RIP.Proxy @"sSU_anon'anon'x") ptr0

instance Marshal.WriteRaw SSU where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SSU sSU_anon'anon'x2 ->
            HasCField.writeRaw (RIP.Proxy @"sSU_anon'anon'x") ptr0 sSU_anon'anon'x2

deriving via Marshal.EquivStorable SSU instance RIP.Storable SSU

instance ( ty ~ SSU_anon'anon'x
         ) => RIP.CompatHasField.HasField "sSU_anon'anon'x" SSU ty where

  hasField =
    \x0 ->
      (\y1 ->
         SSU {sSU_anon'anon'x = y1}, RIP.getField @"sSU_anon'anon'x" x0)

instance ( ty ~ SSU_anon'anon'x
         ) => RIP.HasField "sSU_anon'anon'x" (RIP.Ptr SSU) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sSU_anon'anon'x")

instance HasCField.HasCField SSU "sSU_anon'anon'x" where

  type CFieldType SSU "sSU_anon'anon'x" =
    SSU_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@USU_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 57:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype USU_anon'anon'x_anon'x = USU_anon'anon'x_anon'x
  { unwrapUSU_anon'anon'x_anon'x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize USU_anon'anon'x_anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw USU_anon'anon'x_anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw USU_anon'anon'x_anon'x

deriving via Marshal.EquivStorable USU_anon'anon'x_anon'x instance RIP.Storable USU_anon'anon'x_anon'x

{-|

    __See:__ 'set_uSU_anon'anon'x_anon'x_x'

    __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 58:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uSU_anon'anon'x_anon'x_x ::
     USU_anon'anon'x_anon'x
  -> RIP.CInt
get_uSU_anon'anon'x_anon'x_x = RIP.getUnionPayload

{-|

    __See:__ 'get_uSU_anon'anon'x_anon'x_x'

-}
set_uSU_anon'anon'x_anon'x_x ::
     RIP.CInt
  -> USU_anon'anon'x_anon'x
set_uSU_anon'anon'x_anon'x_x = RIP.setUnionPayload

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "uSU_anon'anon'x_anon'x_x" (RIP.Ptr USU_anon'anon'x_anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uSU_anon'anon'x_anon'x_x")

instance HasCField.HasCField USU_anon'anon'x_anon'x "uSU_anon'anon'x_anon'x_x" where

  type CFieldType USU_anon'anon'x_anon'x "uSU_anon'anon'x_anon'x_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@USU_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 56:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data USU_anon'anon'x = USU_anon'anon'x
  { uSU_anon'anon'x_anon'x :: USU_anon'anon'x_anon'x
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 57:5@

         __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
    -}
  }
  deriving stock (RIP.Generic)

instance Marshal.StaticSize USU_anon'anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw USU_anon'anon'x where

  readRaw =
    \ptr0 ->
          pure USU_anon'anon'x
      <*> HasCField.readRaw (RIP.Proxy @"uSU_anon'anon'x_anon'x") ptr0

instance Marshal.WriteRaw USU_anon'anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          USU_anon'anon'x uSU_anon'anon'x_anon'x2 ->
            HasCField.writeRaw (RIP.Proxy @"uSU_anon'anon'x_anon'x") ptr0 uSU_anon'anon'x_anon'x2

deriving via Marshal.EquivStorable USU_anon'anon'x instance RIP.Storable USU_anon'anon'x

instance ( ty ~ USU_anon'anon'x_anon'x
         ) => RIP.CompatHasField.HasField "uSU_anon'anon'x_anon'x" USU_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          USU_anon'anon'x {uSU_anon'anon'x_anon'x = y1}
      , RIP.getField @"uSU_anon'anon'x_anon'x" x0
      )

instance ( ty ~ USU_anon'anon'x_anon'x
         ) => RIP.HasField "uSU_anon'anon'x_anon'x" (RIP.Ptr USU_anon'anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uSU_anon'anon'x_anon'x")

instance HasCField.HasCField USU_anon'anon'x "uSU_anon'anon'x_anon'x" where

  type CFieldType USU_anon'anon'x "uSU_anon'anon'x_anon'x" =
    USU_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union USU@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 55:7@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype USU = USU
  { unwrapUSU :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize USU

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw USU

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw USU

deriving via Marshal.EquivStorable USU instance RIP.Storable USU

{-|

    __See:__ 'set_uSU_anon'anon'x'

    __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 56:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uSU_anon'anon'x ::
     USU
  -> USU_anon'anon'x
get_uSU_anon'anon'x = RIP.getUnionPayload

{-|

    __See:__ 'get_uSU_anon'anon'x'

-}
set_uSU_anon'anon'x ::
     USU_anon'anon'x
  -> USU
set_uSU_anon'anon'x = RIP.setUnionPayload

instance ( ty ~ USU_anon'anon'x
         ) => RIP.HasField "uSU_anon'anon'x" (RIP.Ptr USU) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uSU_anon'anon'x")

instance HasCField.HasCField USU "uSU_anon'anon'x" where

  type CFieldType USU "uSU_anon'anon'x" =
    USU_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@SUU_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 65:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype SUU_anon'anon'x_anon'x = SUU_anon'anon'x_anon'x
  { unwrapSUU_anon'anon'x_anon'x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize SUU_anon'anon'x_anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw SUU_anon'anon'x_anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw SUU_anon'anon'x_anon'x

deriving via Marshal.EquivStorable SUU_anon'anon'x_anon'x instance RIP.Storable SUU_anon'anon'x_anon'x

{-|

    __See:__ 'set_sUU_anon'anon'x_anon'x_x'

    __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 66:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_sUU_anon'anon'x_anon'x_x ::
     SUU_anon'anon'x_anon'x
  -> RIP.CInt
get_sUU_anon'anon'x_anon'x_x = RIP.getUnionPayload

{-|

    __See:__ 'get_sUU_anon'anon'x_anon'x_x'

-}
set_sUU_anon'anon'x_anon'x_x ::
     RIP.CInt
  -> SUU_anon'anon'x_anon'x
set_sUU_anon'anon'x_anon'x_x = RIP.setUnionPayload

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "sUU_anon'anon'x_anon'x_x" (RIP.Ptr SUU_anon'anon'x_anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sUU_anon'anon'x_anon'x_x")

instance HasCField.HasCField SUU_anon'anon'x_anon'x "sUU_anon'anon'x_anon'x_x" where

  type CFieldType SUU_anon'anon'x_anon'x "sUU_anon'anon'x_anon'x_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@SUU_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 64:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype SUU_anon'anon'x = SUU_anon'anon'x
  { unwrapSUU_anon'anon'x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize SUU_anon'anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw SUU_anon'anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw SUU_anon'anon'x

deriving via Marshal.EquivStorable SUU_anon'anon'x instance RIP.Storable SUU_anon'anon'x

{-|

    __See:__ 'set_sUU_anon'anon'x_anon'x'

    __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 65:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_sUU_anon'anon'x_anon'x ::
     SUU_anon'anon'x
  -> SUU_anon'anon'x_anon'x
get_sUU_anon'anon'x_anon'x = RIP.getUnionPayload

{-|

    __See:__ 'get_sUU_anon'anon'x_anon'x'

-}
set_sUU_anon'anon'x_anon'x ::
     SUU_anon'anon'x_anon'x
  -> SUU_anon'anon'x
set_sUU_anon'anon'x_anon'x = RIP.setUnionPayload

instance ( ty ~ SUU_anon'anon'x_anon'x
         ) => RIP.HasField "sUU_anon'anon'x_anon'x" (RIP.Ptr SUU_anon'anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sUU_anon'anon'x_anon'x")

instance HasCField.HasCField SUU_anon'anon'x "sUU_anon'anon'x_anon'x" where

  type CFieldType SUU_anon'anon'x "sUU_anon'anon'x_anon'x" =
    SUU_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct SUU@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 63:8@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
data SUU = SUU
  { sUU_anon'anon'x :: SUU_anon'anon'x
    {- ^ __C declaration:__ @anon\'anon\'x@

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
      <*> HasCField.readRaw (RIP.Proxy @"sUU_anon'anon'x") ptr0

instance Marshal.WriteRaw SUU where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SUU sUU_anon'anon'x2 ->
            HasCField.writeRaw (RIP.Proxy @"sUU_anon'anon'x") ptr0 sUU_anon'anon'x2

deriving via Marshal.EquivStorable SUU instance RIP.Storable SUU

instance ( ty ~ SUU_anon'anon'x
         ) => RIP.CompatHasField.HasField "sUU_anon'anon'x" SUU ty where

  hasField =
    \x0 ->
      (\y1 ->
         SUU {sUU_anon'anon'x = y1}, RIP.getField @"sUU_anon'anon'x" x0)

instance ( ty ~ SUU_anon'anon'x
         ) => RIP.HasField "sUU_anon'anon'x" (RIP.Ptr SUU) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sUU_anon'anon'x")

instance HasCField.HasCField SUU "sUU_anon'anon'x" where

  type CFieldType SUU "sUU_anon'anon'x" =
    SUU_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@UUU_anon\'anon\'x_anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 73:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUU_anon'anon'x_anon'x = UUU_anon'anon'x_anon'x
  { unwrapUUU_anon'anon'x_anon'x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize UUU_anon'anon'x_anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw UUU_anon'anon'x_anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw UUU_anon'anon'x_anon'x

deriving via Marshal.EquivStorable UUU_anon'anon'x_anon'x instance RIP.Storable UUU_anon'anon'x_anon'x

{-|

    __See:__ 'set_uUU_anon'anon'x_anon'x_x'

    __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 74:11@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uUU_anon'anon'x_anon'x_x ::
     UUU_anon'anon'x_anon'x
  -> RIP.CInt
get_uUU_anon'anon'x_anon'x_x = RIP.getUnionPayload

{-|

    __See:__ 'get_uUU_anon'anon'x_anon'x_x'

-}
set_uUU_anon'anon'x_anon'x_x ::
     RIP.CInt
  -> UUU_anon'anon'x_anon'x
set_uUU_anon'anon'x_anon'x_x = RIP.setUnionPayload

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "uUU_anon'anon'x_anon'x_x" (RIP.Ptr UUU_anon'anon'x_anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uUU_anon'anon'x_anon'x_x")

instance HasCField.HasCField UUU_anon'anon'x_anon'x "uUU_anon'anon'x_anon'x_x" where

  type CFieldType UUU_anon'anon'x_anon'x "uUU_anon'anon'x_anon'x_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@UUU_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 72:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUU_anon'anon'x = UUU_anon'anon'x
  { unwrapUUU_anon'anon'x :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize UUU_anon'anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw UUU_anon'anon'x

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw UUU_anon'anon'x

deriving via Marshal.EquivStorable UUU_anon'anon'x instance RIP.Storable UUU_anon'anon'x

{-|

    __See:__ 'set_uUU_anon'anon'x_anon'x'

    __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 73:5@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uUU_anon'anon'x_anon'x ::
     UUU_anon'anon'x
  -> UUU_anon'anon'x_anon'x
get_uUU_anon'anon'x_anon'x = RIP.getUnionPayload

{-|

    __See:__ 'get_uUU_anon'anon'x_anon'x'

-}
set_uUU_anon'anon'x_anon'x ::
     UUU_anon'anon'x_anon'x
  -> UUU_anon'anon'x
set_uUU_anon'anon'x_anon'x = RIP.setUnionPayload

instance ( ty ~ UUU_anon'anon'x_anon'x
         ) => RIP.HasField "uUU_anon'anon'x_anon'x" (RIP.Ptr UUU_anon'anon'x) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uUU_anon'anon'x_anon'x")

instance HasCField.HasCField UUU_anon'anon'x "uUU_anon'anon'x_anon'x" where

  type CFieldType UUU_anon'anon'x "uUU_anon'anon'x_anon'x" =
    UUU_anon'anon'x_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union UUU@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 71:7@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
newtype UUU = UUU
  { unwrapUUU :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize UUU

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw UUU

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw UUU

deriving via Marshal.EquivStorable UUU instance RIP.Storable UUU

{-|

    __See:__ 'set_uUU_anon'anon'x'

    __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/multi_nesting.h 72:3@

    __exported by:__ @types\/anonymous\/edge-cases\/multi_nesting.h@
-}
get_uUU_anon'anon'x ::
     UUU
  -> UUU_anon'anon'x
get_uUU_anon'anon'x = RIP.getUnionPayload

{-|

    __See:__ 'get_uUU_anon'anon'x'

-}
set_uUU_anon'anon'x ::
     UUU_anon'anon'x
  -> UUU
set_uUU_anon'anon'x = RIP.setUnionPayload

instance ( ty ~ UUU_anon'anon'x
         ) => RIP.HasField "uUU_anon'anon'x" (RIP.Ptr UUU) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uUU_anon'anon'x")

instance HasCField.HasCField UUU "uUU_anon'anon'x" where

  type CFieldType UUU "uUU_anon'anon'x" =
    UUU_anon'anon'x

  offset# = \_ -> \_ -> 0
