{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct bools1@

    __defined at:__ @types\/primitives\/bool.h 1:8@

    __exported by:__ @types\/primitives\/bool.h@
-}
data Bools1 = Bools1
  { bools1_x :: RIP.CBool
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/primitives\/bool.h 2:11@

         __exported by:__ @types\/primitives\/bool.h@
    -}
  , bools1_y :: RIP.CBool
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/primitives\/bool.h 3:11@

         __exported by:__ @types\/primitives\/bool.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bools1 where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bools1 where

  readRaw =
    \ptr0 ->
          pure Bools1
      <*> HasCField.readRaw (RIP.Proxy @"bools1_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"bools1_y") ptr0

instance Marshal.WriteRaw Bools1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools1 bools1_x2 bools1_y3 ->
               HasCField.writeRaw (RIP.Proxy @"bools1_x") ptr0 bools1_x2
            >> HasCField.writeRaw (RIP.Proxy @"bools1_y") ptr0 bools1_y3

deriving via Marshal.EquivStorable Bools1 instance RIP.Storable Bools1

instance HasCField.HasCField Bools1 "bools1_x" where

  type CFieldType Bools1 "bools1_x" = RIP.CBool

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CBool
         ) => RIP.HasField "bools1_x" (RIP.Ptr Bools1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bools1_x")

instance HasCField.HasCField Bools1 "bools1_y" where

  type CFieldType Bools1 "bools1_y" = RIP.CBool

  offset# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CBool
         ) => RIP.HasField "bools1_y" (RIP.Ptr Bools1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bools1_y")

{-| __C declaration:__ @struct bools2@

    __defined at:__ @types\/primitives\/bool.h 8:8@

    __exported by:__ @types\/primitives\/bool.h@
-}
data Bools2 = Bools2
  { bools2_x :: RIP.CBool
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/primitives\/bool.h 9:10@

         __exported by:__ @types\/primitives\/bool.h@
    -}
  , bools2_y :: RIP.CBool
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/primitives\/bool.h 10:10@

         __exported by:__ @types\/primitives\/bool.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bools2 where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bools2 where

  readRaw =
    \ptr0 ->
          pure Bools2
      <*> HasCField.readRaw (RIP.Proxy @"bools2_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"bools2_y") ptr0

instance Marshal.WriteRaw Bools2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools2 bools2_x2 bools2_y3 ->
               HasCField.writeRaw (RIP.Proxy @"bools2_x") ptr0 bools2_x2
            >> HasCField.writeRaw (RIP.Proxy @"bools2_y") ptr0 bools2_y3

deriving via Marshal.EquivStorable Bools2 instance RIP.Storable Bools2

instance HasCField.HasCField Bools2 "bools2_x" where

  type CFieldType Bools2 "bools2_x" = RIP.CBool

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CBool
         ) => RIP.HasField "bools2_x" (RIP.Ptr Bools2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bools2_x")

instance HasCField.HasCField Bools2 "bools2_y" where

  type CFieldType Bools2 "bools2_y" = RIP.CBool

  offset# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CBool
         ) => RIP.HasField "bools2_y" (RIP.Ptr Bools2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bools2_y")

{-| __C declaration:__ @BOOL@

    __defined at:__ @types\/primitives\/bool.h 13:9@

    __exported by:__ @types\/primitives\/bool.h@
-}
newtype BOOL = BOOL
  { unwrapBOOL :: RIP.CBool
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CBool
         ) => RIP.HasField "unwrapBOOL" (RIP.Ptr BOOL) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapBOOL")

instance HasCField.HasCField BOOL "unwrapBOOL" where

  type CFieldType BOOL "unwrapBOOL" = RIP.CBool

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct bools3@

    __defined at:__ @types\/primitives\/bool.h 15:8@

    __exported by:__ @types\/primitives\/bool.h@
-}
data Bools3 = Bools3
  { bools3_x :: BOOL
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/primitives\/bool.h 16:10@

         __exported by:__ @types\/primitives\/bool.h@
    -}
  , bools3_y :: BOOL
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/primitives\/bool.h 17:10@

         __exported by:__ @types\/primitives\/bool.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bools3 where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bools3 where

  readRaw =
    \ptr0 ->
          pure Bools3
      <*> HasCField.readRaw (RIP.Proxy @"bools3_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"bools3_y") ptr0

instance Marshal.WriteRaw Bools3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools3 bools3_x2 bools3_y3 ->
               HasCField.writeRaw (RIP.Proxy @"bools3_x") ptr0 bools3_x2
            >> HasCField.writeRaw (RIP.Proxy @"bools3_y") ptr0 bools3_y3

deriving via Marshal.EquivStorable Bools3 instance RIP.Storable Bools3

instance HasCField.HasCField Bools3 "bools3_x" where

  type CFieldType Bools3 "bools3_x" = BOOL

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) BOOL
         ) => RIP.HasField "bools3_x" (RIP.Ptr Bools3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bools3_x")

instance HasCField.HasCField Bools3 "bools3_y" where

  type CFieldType Bools3 "bools3_y" = BOOL

  offset# = \_ -> \_ -> 1

instance ( ((~) ty) BOOL
         ) => RIP.HasField "bools3_y" (RIP.Ptr Bools3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"bools3_y")
