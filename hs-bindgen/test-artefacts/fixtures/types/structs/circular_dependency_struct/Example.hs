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
    ( Example.B(..)
    , Example.A(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct b@

    __defined at:__ @types\/structs\/circular_dependency_struct.h 3:8@

    __exported by:__ @types\/structs\/circular_dependency_struct.h@
-}
data B = B
  { b_toA :: BG.Ptr A
    {- ^ __C declaration:__ @toA@

         __defined at:__ @types\/structs\/circular_dependency_struct.h 4:13@

         __exported by:__ @types\/structs\/circular_dependency_struct.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize B where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw B where

  readRaw =
    \ptr0 ->
          pure B
      <*> HasCField.readRaw (BG.Proxy @"b_toA") ptr0

instance Marshal.WriteRaw B where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B b_toA2 ->
            HasCField.writeRaw (BG.Proxy @"b_toA") ptr0 b_toA2

deriving via Marshal.EquivStorable B instance BG.Storable B

instance (ty ~ BG.Ptr A) => BG.CompatHasField.HasField "b_toA" B ty where

  hasField =
    \x0 ->
      (\y1 -> B {b_toA = y1}, BG.getField @"b_toA" x0)

instance (ty ~ BG.Ptr A) => BG.HasField "b_toA" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b_toA")

instance HasCField.HasCField B "b_toA" where

  type CFieldType B "b_toA" = BG.Ptr A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct a@

    __defined at:__ @types\/structs\/circular_dependency_struct.h 7:8@

    __exported by:__ @types\/structs\/circular_dependency_struct.h@
-}
data A = A
  { a_toB :: B
    {- ^ __C declaration:__ @toB@

         __defined at:__ @types\/structs\/circular_dependency_struct.h 8:12@

         __exported by:__ @types\/structs\/circular_dependency_struct.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize A where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> HasCField.readRaw (BG.Proxy @"a_toB") ptr0

instance Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_toB2 ->
            HasCField.writeRaw (BG.Proxy @"a_toB") ptr0 a_toB2

deriving via Marshal.EquivStorable A instance BG.Storable A

instance (ty ~ B) => BG.CompatHasField.HasField "a_toB" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {a_toB = y1}, BG.getField @"a_toB" x0)

instance (ty ~ B) => BG.HasField "a_toB" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a_toB")

instance HasCField.HasCField A "a_toB" where

  type CFieldType A "a_toB" = B

  offset# = \_ -> \_ -> 0
