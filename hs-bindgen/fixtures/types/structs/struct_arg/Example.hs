{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct thing@

    __defined at:__ @types\/structs\/struct_arg.h 2:8@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
data Thing = Thing
  { thing_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/struct_arg.h 3:9@

         __exported by:__ @types\/structs\/struct_arg.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Thing where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Thing where

  readRaw =
    \ptr0 ->
          pure Thing
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"thing_x") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Thing where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Thing thing_x2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"thing_x") ptr0 thing_x2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Thing instance F.Storable Thing

instance HsBindgen.Runtime.HasCField.HasCField Thing "thing_x" where

  type CFieldType Thing "thing_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "thing_x" (Ptr.Ptr Thing) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"thing_x")
