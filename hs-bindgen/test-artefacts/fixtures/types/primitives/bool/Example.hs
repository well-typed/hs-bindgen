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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Bools1(..)
    , Example.Bools2(..)
    , Example.BOOL(..)
    , Example.Bools3(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct bools1@

    __defined at:__ @types\/primitives\/bool.h 1:8@

    __exported by:__ @types\/primitives\/bool.h@
-}
data Bools1 = Bools1
  { bools1_x :: BG.CBool
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/primitives\/bool.h 2:11@

         __exported by:__ @types\/primitives\/bool.h@
    -}
  , bools1_y :: BG.CBool
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/primitives\/bool.h 3:11@

         __exported by:__ @types\/primitives\/bool.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bools1 where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bools1 where

  readRaw =
    \ptr0 ->
          pure Bools1
      <*> HasCField.readRaw (BG.Proxy @"bools1_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"bools1_y") ptr0

instance Marshal.WriteRaw Bools1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools1 bools1_x2 bools1_y3 ->
               HasCField.writeRaw (BG.Proxy @"bools1_x") ptr0 bools1_x2
            >> HasCField.writeRaw (BG.Proxy @"bools1_y") ptr0 bools1_y3

deriving via Marshal.EquivStorable Bools1 instance BG.Storable Bools1

instance ( ty ~ BG.CBool
         ) => BG.CompatHasField.HasField "bools1_x" Bools1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bools1 {bools1_x = y1, bools1_y = BG.getField @"bools1_y" x0}
      , BG.getField @"bools1_x" x0
      )

instance ( ty ~ BG.CBool
         ) => BG.HasField "bools1_x" (BG.Ptr Bools1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bools1_x")

instance HasCField.HasCField Bools1 "bools1_x" where

  type CFieldType Bools1 "bools1_x" = BG.CBool

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.CBool
         ) => BG.CompatHasField.HasField "bools1_y" Bools1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bools1 {bools1_y = y1, bools1_x = BG.getField @"bools1_x" x0}
      , BG.getField @"bools1_y" x0
      )

instance ( ty ~ BG.CBool
         ) => BG.HasField "bools1_y" (BG.Ptr Bools1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bools1_y")

instance HasCField.HasCField Bools1 "bools1_y" where

  type CFieldType Bools1 "bools1_y" = BG.CBool

  offset# = \_ -> \_ -> 1

{-| __C declaration:__ @struct bools2@

    __defined at:__ @types\/primitives\/bool.h 8:8@

    __exported by:__ @types\/primitives\/bool.h@
-}
data Bools2 = Bools2
  { bools2_x :: BG.CBool
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/primitives\/bool.h 9:10@

         __exported by:__ @types\/primitives\/bool.h@
    -}
  , bools2_y :: BG.CBool
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/primitives\/bool.h 10:10@

         __exported by:__ @types\/primitives\/bool.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bools2 where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bools2 where

  readRaw =
    \ptr0 ->
          pure Bools2
      <*> HasCField.readRaw (BG.Proxy @"bools2_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"bools2_y") ptr0

instance Marshal.WriteRaw Bools2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools2 bools2_x2 bools2_y3 ->
               HasCField.writeRaw (BG.Proxy @"bools2_x") ptr0 bools2_x2
            >> HasCField.writeRaw (BG.Proxy @"bools2_y") ptr0 bools2_y3

deriving via Marshal.EquivStorable Bools2 instance BG.Storable Bools2

instance ( ty ~ BG.CBool
         ) => BG.CompatHasField.HasField "bools2_x" Bools2 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bools2 {bools2_x = y1, bools2_y = BG.getField @"bools2_y" x0}
      , BG.getField @"bools2_x" x0
      )

instance ( ty ~ BG.CBool
         ) => BG.HasField "bools2_x" (BG.Ptr Bools2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bools2_x")

instance HasCField.HasCField Bools2 "bools2_x" where

  type CFieldType Bools2 "bools2_x" = BG.CBool

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.CBool
         ) => BG.CompatHasField.HasField "bools2_y" Bools2 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bools2 {bools2_y = y1, bools2_x = BG.getField @"bools2_x" x0}
      , BG.getField @"bools2_y" x0
      )

instance ( ty ~ BG.CBool
         ) => BG.HasField "bools2_y" (BG.Ptr Bools2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bools2_y")

instance HasCField.HasCField Bools2 "bools2_y" where

  type CFieldType Bools2 "bools2_y" = BG.CBool

  offset# = \_ -> \_ -> 1

{-| __C declaration:__ @macro BOOL@

    __defined at:__ @types\/primitives\/bool.h 13:9@

    __exported by:__ @types\/primitives\/bool.h@
-}
newtype BOOL = BOOL
  { unwrapBOOL :: BG.CBool
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CBool
         ) => BG.CompatHasField.HasField "unwrapBOOL" BOOL ty where

  hasField =
    \x0 ->
      (\y1 ->
         BOOL {unwrapBOOL = y1}, BG.getField @"unwrapBOOL" x0)

instance ( ty ~ BG.CBool
         ) => BG.HasField "unwrapBOOL" (BG.Ptr BOOL) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapBOOL")

instance HasCField.HasCField BOOL "unwrapBOOL" where

  type CFieldType BOOL "unwrapBOOL" = BG.CBool

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
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bools3 where

  staticSizeOf = \_ -> (2 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Bools3 where

  readRaw =
    \ptr0 ->
          pure Bools3
      <*> HasCField.readRaw (BG.Proxy @"bools3_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"bools3_y") ptr0

instance Marshal.WriteRaw Bools3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools3 bools3_x2 bools3_y3 ->
               HasCField.writeRaw (BG.Proxy @"bools3_x") ptr0 bools3_x2
            >> HasCField.writeRaw (BG.Proxy @"bools3_y") ptr0 bools3_y3

deriving via Marshal.EquivStorable Bools3 instance BG.Storable Bools3

instance (ty ~ BOOL) => BG.CompatHasField.HasField "bools3_x" Bools3 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bools3 {bools3_x = y1, bools3_y = BG.getField @"bools3_y" x0}
      , BG.getField @"bools3_x" x0
      )

instance (ty ~ BOOL) => BG.HasField "bools3_x" (BG.Ptr Bools3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bools3_x")

instance HasCField.HasCField Bools3 "bools3_x" where

  type CFieldType Bools3 "bools3_x" = BOOL

  offset# = \_ -> \_ -> 0

instance (ty ~ BOOL) => BG.CompatHasField.HasField "bools3_y" Bools3 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bools3 {bools3_y = y1, bools3_x = BG.getField @"bools3_x" x0}
      , BG.getField @"bools3_y" x0
      )

instance (ty ~ BOOL) => BG.HasField "bools3_y" (BG.Ptr Bools3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bools3_y")

instance HasCField.HasCField Bools3 "bools3_y" where

  type CFieldType Bools3 "bools3_y" = BOOL

  offset# = \_ -> \_ -> 1
