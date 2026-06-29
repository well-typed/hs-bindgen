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
    ( Example.SS_anon'y(..)
    , Example.SS(..)
    , Example.SU_anon'y(..)
    , Example.SU(..)
    , Example.US_anon'y(..)
    , Example.US(..)
    , Example.UU_anon'y(..)
    , Example.UU(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @struct \@SS_anon\'y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 15:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
data SS_anon'y = SS_anon'y
  { sS_anon'y_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/padding.h 17:9@

         __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize SS_anon'y where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SS_anon'y where

  readRaw =
    \ptr0 ->
          pure SS_anon'y
      <*> HasCField.readRaw (RIP.Proxy @"sS_anon'y_y") ptr0

instance Marshal.WriteRaw SS_anon'y where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SS_anon'y sS_anon'y_y2 ->
            HasCField.writeRaw (RIP.Proxy @"sS_anon'y_y") ptr0 sS_anon'y_y2

deriving via Marshal.EquivStorable SS_anon'y instance RIP.Storable SS_anon'y

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "sS_anon'y_y" SS_anon'y ty where

  hasField =
    \x0 ->
      (\y1 ->
         SS_anon'y {sS_anon'y_y = y1}, RIP.getField @"sS_anon'y_y" x0)

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "sS_anon'y_y" (RIP.Ptr SS_anon'y) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sS_anon'y_y")

instance HasCField.HasCField SS_anon'y "sS_anon'y_y" where

  type CFieldType SS_anon'y "sS_anon'y_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct SS@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 13:8@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
data SS = SS
  { sS_x :: RIP.CChar
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/padding.h 14:8@

         __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
    -}
  , sS_anon'y :: SS_anon'y
    {- ^ __C declaration:__ @anon\'y@

         __defined at:__ @types\/anonymous\/edge-cases\/padding.h 15:3@

         __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
    -}
  , sS_z :: RIP.CInt
    {- ^ __C declaration:__ @z@

         __defined at:__ @types\/anonymous\/edge-cases\/padding.h 19:7@

         __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize SS where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SS where

  readRaw =
    \ptr0 ->
          pure SS
      <*> HasCField.readRaw (RIP.Proxy @"sS_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"sS_anon'y") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"sS_z") ptr0

instance Marshal.WriteRaw SS where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SS sS_x2 sS_anon'y3 sS_z4 ->
               HasCField.writeRaw (RIP.Proxy @"sS_x") ptr0 sS_x2
            >> HasCField.writeRaw (RIP.Proxy @"sS_anon'y") ptr0 sS_anon'y3
            >> HasCField.writeRaw (RIP.Proxy @"sS_z") ptr0 sS_z4

deriving via Marshal.EquivStorable SS instance RIP.Storable SS

instance (ty ~ RIP.CChar) => RIP.CompatHasField.HasField "sS_x" SS ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SS { sS_x = y1
             , sS_anon'y = RIP.getField @"sS_anon'y" x0
             , sS_z = RIP.getField @"sS_z" x0
             }
      , RIP.getField @"sS_x" x0
      )

instance (ty ~ RIP.CChar) => RIP.HasField "sS_x" (RIP.Ptr SS) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sS_x")

instance HasCField.HasCField SS "sS_x" where

  type CFieldType SS "sS_x" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance (ty ~ SS_anon'y) => RIP.CompatHasField.HasField "sS_anon'y" SS ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SS {sS_anon'y = y1, sS_x = RIP.getField @"sS_x" x0, sS_z = RIP.getField @"sS_z" x0}
      , RIP.getField @"sS_anon'y" x0
      )

instance ( ty ~ SS_anon'y
         ) => RIP.HasField "sS_anon'y" (RIP.Ptr SS) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sS_anon'y")

instance HasCField.HasCField SS "sS_anon'y" where

  type CFieldType SS "sS_anon'y" = SS_anon'y

  offset# = \_ -> \_ -> 4

instance (ty ~ RIP.CInt) => RIP.CompatHasField.HasField "sS_z" SS ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SS { sS_z = y1
             , sS_x = RIP.getField @"sS_x" x0
             , sS_anon'y = RIP.getField @"sS_anon'y" x0
             }
      , RIP.getField @"sS_z" x0
      )

instance (ty ~ RIP.CInt) => RIP.HasField "sS_z" (RIP.Ptr SS) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sS_z")

instance HasCField.HasCField SS "sS_z" where

  type CFieldType SS "sS_z" = RIP.CInt

  offset# = \_ -> \_ -> 12

{-| __C declaration:__ @union \@SU_anon\'y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 24:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
newtype SU_anon'y = SU_anon'y
  { unwrapSU_anon'y :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize SU_anon'y

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw SU_anon'y

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw SU_anon'y

deriving via Marshal.EquivStorable SU_anon'y instance RIP.Storable SU_anon'y

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion SU_anon'y

{-| __C declaration:__ @y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 26:9@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ RIP.CInt) => RIP.HasField "sU_anon'y_y" SU_anon'y ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 26:9@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "sU_anon'y_y" SU_anon'y ty where

  hasField =
    \x0 ->
      (RIP.setUnionPayload, RIP.getField @"sU_anon'y_y" x0)

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "sU_anon'y_y" (RIP.Ptr SU_anon'y) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"sU_anon'y_y")

instance HasCField.HasCField SU_anon'y "sU_anon'y_y" where

  type CFieldType SU_anon'y "sU_anon'y_y" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct SU@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 22:8@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
data SU = SU
  { sU_x :: RIP.CChar
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/edge-cases\/padding.h 23:8@

         __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
    -}
  , sU_anon'y :: SU_anon'y
    {- ^ __C declaration:__ @anon\'y@

         __defined at:__ @types\/anonymous\/edge-cases\/padding.h 24:3@

         __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
    -}
  , sU_z :: RIP.CInt
    {- ^ __C declaration:__ @z@

         __defined at:__ @types\/anonymous\/edge-cases\/padding.h 28:7@

         __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
    -}
  }
  deriving stock (RIP.Generic)

instance Marshal.StaticSize SU where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SU where

  readRaw =
    \ptr0 ->
          pure SU
      <*> HasCField.readRaw (RIP.Proxy @"sU_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"sU_anon'y") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"sU_z") ptr0

instance Marshal.WriteRaw SU where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SU sU_x2 sU_anon'y3 sU_z4 ->
               HasCField.writeRaw (RIP.Proxy @"sU_x") ptr0 sU_x2
            >> HasCField.writeRaw (RIP.Proxy @"sU_anon'y") ptr0 sU_anon'y3
            >> HasCField.writeRaw (RIP.Proxy @"sU_z") ptr0 sU_z4

deriving via Marshal.EquivStorable SU instance RIP.Storable SU

instance (ty ~ RIP.CChar) => RIP.CompatHasField.HasField "sU_x" SU ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SU { sU_x = y1
             , sU_anon'y = RIP.getField @"sU_anon'y" x0
             , sU_z = RIP.getField @"sU_z" x0
             }
      , RIP.getField @"sU_x" x0
      )

instance (ty ~ RIP.CChar) => RIP.HasField "sU_x" (RIP.Ptr SU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sU_x")

instance HasCField.HasCField SU "sU_x" where

  type CFieldType SU "sU_x" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance (ty ~ SU_anon'y) => RIP.CompatHasField.HasField "sU_anon'y" SU ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SU {sU_anon'y = y1, sU_x = RIP.getField @"sU_x" x0, sU_z = RIP.getField @"sU_z" x0}
      , RIP.getField @"sU_anon'y" x0
      )

instance ( ty ~ SU_anon'y
         ) => RIP.HasField "sU_anon'y" (RIP.Ptr SU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sU_anon'y")

instance HasCField.HasCField SU "sU_anon'y" where

  type CFieldType SU "sU_anon'y" = SU_anon'y

  offset# = \_ -> \_ -> 4

instance (ty ~ RIP.CInt) => RIP.CompatHasField.HasField "sU_z" SU ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SU { sU_z = y1
             , sU_x = RIP.getField @"sU_x" x0
             , sU_anon'y = RIP.getField @"sU_anon'y" x0
             }
      , RIP.getField @"sU_z" x0
      )

instance (ty ~ RIP.CInt) => RIP.HasField "sU_z" (RIP.Ptr SU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sU_z")

instance HasCField.HasCField SU "sU_z" where

  type CFieldType SU "sU_z" = RIP.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct \@US_anon\'y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 33:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
data US_anon'y = US_anon'y
  { uS_anon'y_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/padding.h 35:9@

         __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize US_anon'y where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw US_anon'y where

  readRaw =
    \ptr0 ->
          pure US_anon'y
      <*> HasCField.readRaw (RIP.Proxy @"uS_anon'y_y") ptr0

instance Marshal.WriteRaw US_anon'y where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          US_anon'y uS_anon'y_y2 ->
            HasCField.writeRaw (RIP.Proxy @"uS_anon'y_y") ptr0 uS_anon'y_y2

deriving via Marshal.EquivStorable US_anon'y instance RIP.Storable US_anon'y

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "uS_anon'y_y" US_anon'y ty where

  hasField =
    \x0 ->
      (\y1 ->
         US_anon'y {uS_anon'y_y = y1}, RIP.getField @"uS_anon'y_y" x0)

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "uS_anon'y_y" (RIP.Ptr US_anon'y) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uS_anon'y_y")

instance HasCField.HasCField US_anon'y "uS_anon'y_y" where

  type CFieldType US_anon'y "uS_anon'y_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @union US@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 31:7@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
newtype US = US
  { unwrapUS :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 8 4 instance Marshal.StaticSize US

deriving via RIP.SizedByteArray 8 4 instance Marshal.ReadRaw US

deriving via RIP.SizedByteArray 8 4 instance Marshal.WriteRaw US

deriving via Marshal.EquivStorable US instance RIP.Storable US

deriving via RIP.SizedByteArray 8 4 instance Union.IsUnion US

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 32:8@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ RIP.CChar) => RIP.HasField "uS_x" US ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 32:8@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ RIP.CChar) => RIP.CompatHasField.HasField "uS_x" US ty where

  hasField =
    \x0 -> (RIP.setUnionPayload, RIP.getField @"uS_x" x0)

instance (ty ~ RIP.CChar) => RIP.HasField "uS_x" (RIP.Ptr US) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uS_x")

instance HasCField.HasCField US "uS_x" where

  type CFieldType US "uS_x" = RIP.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @anon\'y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 33:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ US_anon'y) => RIP.HasField "uS_anon'y" US ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @anon\'y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 33:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ US_anon'y) => RIP.CompatHasField.HasField "uS_anon'y" US ty where

  hasField =
    \x0 ->
      (RIP.setUnionPayload, RIP.getField @"uS_anon'y" x0)

instance ( ty ~ US_anon'y
         ) => RIP.HasField "uS_anon'y" (RIP.Ptr US) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uS_anon'y")

instance HasCField.HasCField US "uS_anon'y" where

  type CFieldType US "uS_anon'y" = US_anon'y

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @z@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 37:7@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ RIP.CInt) => RIP.HasField "uS_z" US ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @z@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 37:7@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ RIP.CInt) => RIP.CompatHasField.HasField "uS_z" US ty where

  hasField =
    \x0 -> (RIP.setUnionPayload, RIP.getField @"uS_z" x0)

instance (ty ~ RIP.CInt) => RIP.HasField "uS_z" (RIP.Ptr US) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uS_z")

instance HasCField.HasCField US "uS_z" where

  type CFieldType US "uS_z" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@UU_anon\'y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 42:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
newtype UU_anon'y = UU_anon'y
  { unwrapUU_anon'y :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize UU_anon'y

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw UU_anon'y

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw UU_anon'y

deriving via Marshal.EquivStorable UU_anon'y instance RIP.Storable UU_anon'y

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion UU_anon'y

{-| __C declaration:__ @y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 44:9@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ RIP.CInt) => RIP.HasField "uU_anon'y_y" UU_anon'y ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 44:9@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "uU_anon'y_y" UU_anon'y ty where

  hasField =
    \x0 ->
      (RIP.setUnionPayload, RIP.getField @"uU_anon'y_y" x0)

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "uU_anon'y_y" (RIP.Ptr UU_anon'y) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"uU_anon'y_y")

instance HasCField.HasCField UU_anon'y "uU_anon'y_y" where

  type CFieldType UU_anon'y "uU_anon'y_y" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union UU@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 40:7@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
newtype UU = UU
  { unwrapUU :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize UU

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw UU

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw UU

deriving via Marshal.EquivStorable UU instance RIP.Storable UU

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion UU

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 41:8@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ RIP.CChar) => RIP.HasField "uU_x" UU ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 41:8@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ RIP.CChar) => RIP.CompatHasField.HasField "uU_x" UU ty where

  hasField =
    \x0 -> (RIP.setUnionPayload, RIP.getField @"uU_x" x0)

instance (ty ~ RIP.CChar) => RIP.HasField "uU_x" (RIP.Ptr UU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uU_x")

instance HasCField.HasCField UU "uU_x" where

  type CFieldType UU "uU_x" = RIP.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @anon\'y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 42:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ UU_anon'y) => RIP.HasField "uU_anon'y" UU ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @anon\'y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 42:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ UU_anon'y) => RIP.CompatHasField.HasField "uU_anon'y" UU ty where

  hasField =
    \x0 ->
      (RIP.setUnionPayload, RIP.getField @"uU_anon'y" x0)

instance ( ty ~ UU_anon'y
         ) => RIP.HasField "uU_anon'y" (RIP.Ptr UU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uU_anon'y")

instance HasCField.HasCField UU "uU_anon'y" where

  type CFieldType UU "uU_anon'y" = UU_anon'y

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @z@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 46:7@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ RIP.CInt) => RIP.HasField "uU_z" UU ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @z@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 46:7@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
instance (ty ~ RIP.CInt) => RIP.CompatHasField.HasField "uU_z" UU ty where

  hasField =
    \x0 -> (RIP.setUnionPayload, RIP.getField @"uU_z" x0)

instance (ty ~ RIP.CInt) => RIP.HasField "uU_z" (RIP.Ptr UU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uU_z")

instance HasCField.HasCField UU "uU_z" where

  type CFieldType UU "uU_z" = RIP.CInt

  offset# = \_ -> \_ -> 0
