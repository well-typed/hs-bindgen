{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
    ( Example.A(..)
    , Example.B(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct A@

    __defined at:__ @edge-cases\/duplicate_record_field.h 1:8@

    __exported by:__ @edge-cases\/duplicate_record_field.h@
-}
data A = A
  { dup :: RIP.CInt
    {- ^ __C declaration:__ @dup@

         __defined at:__ @edge-cases\/duplicate_record_field.h 2:7@

         __exported by:__ @edge-cases\/duplicate_record_field.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize A where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> HasCField.readRaw (RIP.Proxy @"dup") ptr0

instance Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A dup2 ->
            HasCField.writeRaw (RIP.Proxy @"dup") ptr0 dup2

deriving via Marshal.EquivStorable A instance RIP.Storable A

instance HasCField.HasCField A "dup" where

  type CFieldType A "dup" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "dup" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"dup")

{-| __C declaration:__ @struct B@

    __defined at:__ @edge-cases\/duplicate_record_field.h 5:8@

    __exported by:__ @edge-cases\/duplicate_record_field.h@
-}
data B = B
  { dup :: RIP.CInt
    {- ^ __C declaration:__ @dup@

         __defined at:__ @edge-cases\/duplicate_record_field.h 6:7@

         __exported by:__ @edge-cases\/duplicate_record_field.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize B where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw B where

  readRaw =
    \ptr0 ->
          pure B
      <*> HasCField.readRaw (RIP.Proxy @"dup") ptr0

instance Marshal.WriteRaw B where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B dup2 ->
            HasCField.writeRaw (RIP.Proxy @"dup") ptr0 dup2

deriving via Marshal.EquivStorable B instance RIP.Storable B

instance HasCField.HasCField B "dup" where

  type CFieldType B "dup" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "dup" (RIP.Ptr B) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"dup")
