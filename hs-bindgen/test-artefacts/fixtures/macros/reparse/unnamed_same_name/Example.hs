{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.M(..)
    , Example.A_b(..)
    , Example.A(..)
    , Example.Named(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, type (~))

{-| __C declaration:__ @macro M@

    __defined at:__ @macros\/reparse\/unnamed_same_name.h 3:9@

    __exported by:__ @macros\/reparse\/unnamed_same_name.h@
-}
newtype M = M
  { unwrap :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrap" M ty where

  hasField =
    \x0 ->
      (\y1 -> M {unwrap = y1}, BG.getField @"unwrap" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrap" (BG.Ptr M) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField M "unwrap" where

  type CFieldType M "unwrap" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@a_b@

    __defined at:__ @macros\/reparse\/unnamed_same_name.h 5:12@

    __exported by:__ @macros\/reparse\/unnamed_same_name.h@
-}
data A_b = A_b
  { x :: M
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/unnamed_same_name.h 5:23@

         __exported by:__ @macros\/reparse\/unnamed_same_name.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize A_b where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw A_b where

  readRaw =
    \ptr0 ->
          pure A_b
      <*> HasCField.readRaw (BG.Proxy @"x") ptr0

instance Marshal.WriteRaw A_b where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A_b x2 -> HasCField.writeRaw (BG.Proxy @"x") ptr0 x2

deriving via Marshal.EquivStorable A_b instance BG.Storable A_b

deriving via Struct.IsStructViaReadRaw A_b instance Struct.IsStruct A_b

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/unnamed_same_name.h 5:23@

    __exported by:__ @macros\/reparse\/unnamed_same_name.h@
-}
instance (ty ~ M) => BG.CompatHasField.HasField "x" A_b ty where

  hasField =
    \x0 -> (\y1 -> A_b {x = y1}, BG.getField @"x" x0)

instance (ty ~ M) => BG.HasField "x" (BG.Ptr A_b) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"x")

instance HasCField.HasCField A_b "x" where

  type CFieldType A_b "x" = M

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct a@

    __defined at:__ @macros\/reparse\/unnamed_same_name.h 5:8@

    __exported by:__ @macros\/reparse\/unnamed_same_name.h@
-}
data A = A
  { b :: A_b
    {- ^ __C declaration:__ @b@

         __defined at:__ @macros\/reparse\/unnamed_same_name.h 5:28@

         __exported by:__ @macros\/reparse\/unnamed_same_name.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize A where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> HasCField.readRaw (BG.Proxy @"b") ptr0

instance Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A b2 -> HasCField.writeRaw (BG.Proxy @"b") ptr0 b2

deriving via Marshal.EquivStorable A instance BG.Storable A

deriving via Struct.IsStructViaReadRaw A instance Struct.IsStruct A

{-| __C declaration:__ @b@

    __defined at:__ @macros\/reparse\/unnamed_same_name.h 5:28@

    __exported by:__ @macros\/reparse\/unnamed_same_name.h@
-}
instance (ty ~ A_b) => BG.CompatHasField.HasField "b" A ty where

  hasField =
    \x0 -> (\y1 -> A {b = y1}, BG.getField @"b" x0)

instance (ty ~ A_b) => BG.HasField "b" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b")

instance HasCField.HasCField A "b" where

  type CFieldType A "b" = A_b

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct a_b@

    __defined at:__ @macros\/reparse\/unnamed_same_name.h 6:8@

    __exported by:__ @macros\/reparse\/unnamed_same_name.h@
-}
data Named = Named
  { x :: BG.Ptr M
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/unnamed_same_name.h 6:17@

         __exported by:__ @macros\/reparse\/unnamed_same_name.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Named where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Named where

  readRaw =
    \ptr0 ->
          pure Named
      <*> HasCField.readRaw (BG.Proxy @"x") ptr0

instance Marshal.WriteRaw Named where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Named x2 ->
            HasCField.writeRaw (BG.Proxy @"x") ptr0 x2

deriving via Marshal.EquivStorable Named instance BG.Storable Named

deriving via Struct.IsStructViaReadRaw Named instance Struct.IsStruct Named

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/unnamed_same_name.h 6:17@

    __exported by:__ @macros\/reparse\/unnamed_same_name.h@
-}
instance (ty ~ BG.Ptr M) => BG.CompatHasField.HasField "x" Named ty where

  hasField =
    \x0 -> (\y1 -> Named {x = y1}, BG.getField @"x" x0)

instance (ty ~ BG.Ptr M) => BG.HasField "x" (BG.Ptr Named) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"x")

instance HasCField.HasCField Named "x" where

  type CFieldType Named "x" = BG.Ptr M

  offset# = \_ -> \_ -> 0
