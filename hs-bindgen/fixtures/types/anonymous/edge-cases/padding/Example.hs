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
    ( Example.SS_y(..)
    , Example.SS(..)
    , Example.SU_y(..)
    , Example.get_sU_y_y
    , Example.set_sU_y_y
    , Example.SU(..)
    , Example.US_y(..)
    , Example.US(..)
    , Example.get_uS_x
    , Example.set_uS_x
    , Example.get_uS_y
    , Example.set_uS_y
    , Example.get_uS_z
    , Example.set_uS_z
    , Example.UU_y(..)
    , Example.get_uU_y_y
    , Example.set_uU_y_y
    , Example.UU(..)
    , Example.get_uU_x
    , Example.set_uU_x
    , Example.get_uU_y
    , Example.set_uU_y
    , Example.get_uU_z
    , Example.set_uU_z
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct \@SS_y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 15:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
data SS_y = SS_y
  { sS_y_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/padding.h 17:9@

         __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize SS_y where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw SS_y where

  readRaw =
    \ptr0 ->
          pure SS_y
      <*> HasCField.readRaw (RIP.Proxy @"sS_y_y") ptr0

instance Marshal.WriteRaw SS_y where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SS_y sS_y_y2 ->
            HasCField.writeRaw (RIP.Proxy @"sS_y_y") ptr0 sS_y_y2

deriving via Marshal.EquivStorable SS_y instance RIP.Storable SS_y

instance HasCField.HasCField SS_y "sS_y_y" where

  type CFieldType SS_y "sS_y_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "sS_y_y" (RIP.Ptr SS_y) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sS_y_y")

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
  , sS_y :: SS_y
    {- ^ __C declaration:__ @y@

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
      <*> HasCField.readRaw (RIP.Proxy @"sS_y") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"sS_z") ptr0

instance Marshal.WriteRaw SS where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SS sS_x2 sS_y3 sS_z4 ->
               HasCField.writeRaw (RIP.Proxy @"sS_x") ptr0 sS_x2
            >> HasCField.writeRaw (RIP.Proxy @"sS_y") ptr0 sS_y3
            >> HasCField.writeRaw (RIP.Proxy @"sS_z") ptr0 sS_z4

deriving via Marshal.EquivStorable SS instance RIP.Storable SS

instance HasCField.HasCField SS "sS_x" where

  type CFieldType SS "sS_x" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "sS_x" (RIP.Ptr SS) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sS_x")

instance HasCField.HasCField SS "sS_y" where

  type CFieldType SS "sS_y" = SS_y

  offset# = \_ -> \_ -> 4

instance (((~) ty) SS_y) => RIP.HasField "sS_y" (RIP.Ptr SS) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sS_y")

instance HasCField.HasCField SS "sS_z" where

  type CFieldType SS "sS_z" = RIP.CInt

  offset# = \_ -> \_ -> 12

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "sS_z" (RIP.Ptr SS) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sS_z")

{-| __C declaration:__ @union \@SU_y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 24:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
newtype SU_y = SU_y
  { unwrapSU_y :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize SU_y

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw SU_y

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw SU_y

deriving via Marshal.EquivStorable SU_y instance RIP.Storable SU_y

{-|

    __See:__ 'set_sU_y_y'

    __C declaration:__ @y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 26:9@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
get_sU_y_y ::
     SU_y
  -> RIP.CInt
get_sU_y_y = RIP.getUnionPayload

{-|

    __See:__ 'get_sU_y_y'

-}
set_sU_y_y ::
     RIP.CInt
  -> SU_y
set_sU_y_y = RIP.setUnionPayload

instance HasCField.HasCField SU_y "sU_y_y" where

  type CFieldType SU_y "sU_y_y" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "sU_y_y" (RIP.Ptr SU_y) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sU_y_y")

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
  , sU_y :: SU_y
    {- ^ __C declaration:__ @y@

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
      <*> HasCField.readRaw (RIP.Proxy @"sU_y") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"sU_z") ptr0

instance Marshal.WriteRaw SU where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          SU sU_x2 sU_y3 sU_z4 ->
               HasCField.writeRaw (RIP.Proxy @"sU_x") ptr0 sU_x2
            >> HasCField.writeRaw (RIP.Proxy @"sU_y") ptr0 sU_y3
            >> HasCField.writeRaw (RIP.Proxy @"sU_z") ptr0 sU_z4

deriving via Marshal.EquivStorable SU instance RIP.Storable SU

instance HasCField.HasCField SU "sU_x" where

  type CFieldType SU "sU_x" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "sU_x" (RIP.Ptr SU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sU_x")

instance HasCField.HasCField SU "sU_y" where

  type CFieldType SU "sU_y" = SU_y

  offset# = \_ -> \_ -> 4

instance (((~) ty) SU_y) => RIP.HasField "sU_y" (RIP.Ptr SU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sU_y")

instance HasCField.HasCField SU "sU_z" where

  type CFieldType SU "sU_z" = RIP.CInt

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "sU_z" (RIP.Ptr SU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"sU_z")

{-| __C declaration:__ @struct \@US_y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 33:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
data US_y = US_y
  { uS_y_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/anonymous\/edge-cases\/padding.h 35:9@

         __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize US_y where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw US_y where

  readRaw =
    \ptr0 ->
          pure US_y
      <*> HasCField.readRaw (RIP.Proxy @"uS_y_y") ptr0

instance Marshal.WriteRaw US_y where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          US_y uS_y_y2 ->
            HasCField.writeRaw (RIP.Proxy @"uS_y_y") ptr0 uS_y_y2

deriving via Marshal.EquivStorable US_y instance RIP.Storable US_y

instance HasCField.HasCField US_y "uS_y_y" where

  type CFieldType US_y "uS_y_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "uS_y_y" (RIP.Ptr US_y) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uS_y_y")

{-| __C declaration:__ @union US@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 31:7@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
newtype US = US
  { unwrapUS :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 8) 4 instance Marshal.StaticSize US

deriving via (RIP.SizedByteArray 8) 4 instance Marshal.ReadRaw US

deriving via (RIP.SizedByteArray 8) 4 instance Marshal.WriteRaw US

deriving via Marshal.EquivStorable US instance RIP.Storable US

{-|

    __See:__ 'set_uS_x'

    __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 32:8@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
get_uS_x ::
     US
  -> RIP.CChar
get_uS_x = RIP.getUnionPayload

{-|

    __See:__ 'get_uS_x'

-}
set_uS_x ::
     RIP.CChar
  -> US
set_uS_x = RIP.setUnionPayload

{-|

    __See:__ 'set_uS_y'

    __C declaration:__ @y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 33:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
get_uS_y ::
     US
  -> US_y
get_uS_y = RIP.getUnionPayload

{-|

    __See:__ 'get_uS_y'

-}
set_uS_y ::
     US_y
  -> US
set_uS_y = RIP.setUnionPayload

{-|

    __See:__ 'set_uS_z'

    __C declaration:__ @z@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 37:7@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
get_uS_z ::
     US
  -> RIP.CInt
get_uS_z = RIP.getUnionPayload

{-|

    __See:__ 'get_uS_z'

-}
set_uS_z ::
     RIP.CInt
  -> US
set_uS_z = RIP.setUnionPayload

instance HasCField.HasCField US "uS_x" where

  type CFieldType US "uS_x" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "uS_x" (RIP.Ptr US) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uS_x")

instance HasCField.HasCField US "uS_y" where

  type CFieldType US "uS_y" = US_y

  offset# = \_ -> \_ -> 0

instance (((~) ty) US_y) => RIP.HasField "uS_y" (RIP.Ptr US) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uS_y")

instance HasCField.HasCField US "uS_z" where

  type CFieldType US "uS_z" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "uS_z" (RIP.Ptr US) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uS_z")

{-| __C declaration:__ @union \@UU_y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 42:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
newtype UU_y = UU_y
  { unwrapUU_y :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize UU_y

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw UU_y

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw UU_y

deriving via Marshal.EquivStorable UU_y instance RIP.Storable UU_y

{-|

    __See:__ 'set_uU_y_y'

    __C declaration:__ @y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 44:9@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
get_uU_y_y ::
     UU_y
  -> RIP.CInt
get_uU_y_y = RIP.getUnionPayload

{-|

    __See:__ 'get_uU_y_y'

-}
set_uU_y_y ::
     RIP.CInt
  -> UU_y
set_uU_y_y = RIP.setUnionPayload

instance HasCField.HasCField UU_y "uU_y_y" where

  type CFieldType UU_y "uU_y_y" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "uU_y_y" (RIP.Ptr UU_y) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uU_y_y")

{-| __C declaration:__ @union UU@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 40:7@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
newtype UU = UU
  { unwrapUU :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize UU

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw UU

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw UU

deriving via Marshal.EquivStorable UU instance RIP.Storable UU

{-|

    __See:__ 'set_uU_x'

    __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 41:8@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
get_uU_x ::
     UU
  -> RIP.CChar
get_uU_x = RIP.getUnionPayload

{-|

    __See:__ 'get_uU_x'

-}
set_uU_x ::
     RIP.CChar
  -> UU
set_uU_x = RIP.setUnionPayload

{-|

    __See:__ 'set_uU_y'

    __C declaration:__ @y@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 42:3@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
get_uU_y ::
     UU
  -> UU_y
get_uU_y = RIP.getUnionPayload

{-|

    __See:__ 'get_uU_y'

-}
set_uU_y ::
     UU_y
  -> UU
set_uU_y = RIP.setUnionPayload

{-|

    __See:__ 'set_uU_z'

    __C declaration:__ @z@

    __defined at:__ @types\/anonymous\/edge-cases\/padding.h 46:7@

    __exported by:__ @types\/anonymous\/edge-cases\/padding.h@
-}
get_uU_z ::
     UU
  -> RIP.CInt
get_uU_z = RIP.getUnionPayload

{-|

    __See:__ 'get_uU_z'

-}
set_uU_z ::
     RIP.CInt
  -> UU
set_uU_z = RIP.setUnionPayload

instance HasCField.HasCField UU "uU_x" where

  type CFieldType UU "uU_x" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "uU_x" (RIP.Ptr UU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uU_x")

instance HasCField.HasCField UU "uU_y" where

  type CFieldType UU "uU_y" = UU_y

  offset# = \_ -> \_ -> 0

instance (((~) ty) UU_y) => RIP.HasField "uU_y" (RIP.Ptr UU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uU_y")

instance HasCField.HasCField UU "uU_z" where

  type CFieldType UU "uU_z" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "uU_z" (RIP.Ptr UU) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"uU_z")
