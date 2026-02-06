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
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct vector@

    __defined at:__ @types\/complex\/vector_test.h 1:9@

    __exported by:__ @types\/complex\/vector_test.h@
-}
data Vector = Vector
  { vector_x :: FC.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/complex\/vector_test.h 2:12@

         __exported by:__ @types\/complex\/vector_test.h@
    -}
  , vector_y :: FC.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/complex\/vector_test.h 3:12@

         __exported by:__ @types\/complex\/vector_test.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Vector where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Vector where

  readRaw =
    \ptr0 ->
          pure Vector
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"vector_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"vector_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Vector where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_x2 vector_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"vector_x") ptr0 vector_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"vector_y") ptr0 vector_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Vector instance F.Storable Vector

instance HsBindgen.Runtime.HasCField.HasCField Vector "vector_x" where

  type CFieldType Vector "vector_x" = FC.CDouble

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "vector_x" (Ptr.Ptr Vector) (Ptr.Ptr FC.CDouble) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"vector_x")

instance HsBindgen.Runtime.HasCField.HasCField Vector "vector_y" where

  type CFieldType Vector "vector_y" = FC.CDouble

  offset# = \_ -> \_ -> 8

instance GHC.Records.HasField "vector_y" (Ptr.Ptr Vector) (Ptr.Ptr FC.CDouble) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"vector_y")
