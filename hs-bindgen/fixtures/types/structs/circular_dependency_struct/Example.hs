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

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct b@

    __defined at:__ @types\/structs\/circular_dependency_struct.h 3:8@

    __exported by:__ @types\/structs\/circular_dependency_struct.h@
-}
data B = B
  { b_toA :: RIP.Ptr A
    {- ^ __C declaration:__ @toA@

         __defined at:__ @types\/structs\/circular_dependency_struct.h 4:13@

         __exported by:__ @types\/structs\/circular_dependency_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize B where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw B where

  readRaw =
    \ptr0 ->
          pure B
      <*> HasCField.readRaw (RIP.Proxy @"b_toA") ptr0

instance Marshal.WriteRaw B where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B b_toA2 ->
            HasCField.writeRaw (RIP.Proxy @"b_toA") ptr0 b_toA2

deriving via Marshal.EquivStorable B instance RIP.Storable B

instance HasCField.HasCField B "b_toA" where

  type CFieldType B "b_toA" = RIP.Ptr A

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.Ptr A)
         ) => RIP.HasField "b_toA" (RIP.Ptr B) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"b_toA")

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize A where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> HasCField.readRaw (RIP.Proxy @"a_toB") ptr0

instance Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_toB2 ->
            HasCField.writeRaw (RIP.Proxy @"a_toB") ptr0 a_toB2

deriving via Marshal.EquivStorable A instance RIP.Storable A

instance HasCField.HasCField A "a_toB" where

  type CFieldType A "a_toB" = B

  offset# = \_ -> \_ -> 0

instance (((~) ty) B) => RIP.HasField "a_toB" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a_toB")
