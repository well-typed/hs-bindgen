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

{-| __C declaration:__ @struct thing@

    __defined at:__ @types\/structs\/struct_arg.h 2:8@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
data Thing = Thing
  { thing_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/struct_arg.h 3:9@

         __exported by:__ @types\/structs\/struct_arg.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Thing where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Thing where

  readRaw =
    \ptr0 ->
          pure Thing
      <*> HasCField.readRaw (RIP.Proxy @"thing_x") ptr0

instance Marshal.WriteRaw Thing where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Thing thing_x2 ->
            HasCField.writeRaw (RIP.Proxy @"thing_x") ptr0 thing_x2

deriving via Marshal.EquivStorable Thing instance RIP.Storable Thing

instance HasCField.HasCField Thing "thing_x" where

  type CFieldType Thing "thing_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "thing_x" (RIP.Ptr Thing) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"thing_x")
