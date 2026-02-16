{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.FLAM
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct Vector@

    __defined at:__ @edge-cases\/flam_functions.h 1:8@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
data Vector_Aux = Vector
  { vector_length :: FC.CInt
    {- ^ __C declaration:__ @length@

         __defined at:__ @edge-cases\/flam_functions.h 2:7@

         __exported by:__ @edge-cases\/flam_functions.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Vector_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Vector_Aux where

  readRaw =
    \ptr0 ->
          pure Vector
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"vector_length") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Vector_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_length2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"vector_length") ptr0 vector_length2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Vector_Aux instance F.Storable Vector_Aux

instance HsBindgen.Runtime.HasCField.HasCField Vector_Aux "vector_length" where

  type CFieldType Vector_Aux "vector_length" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "vector_length" (Ptr.Ptr Vector_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"vector_length")

instance HsBindgen.Runtime.FLAM.Offset FC.CLong Vector_Aux where

  offset = \_ty0 -> 8

{-| __C declaration:__ @struct Vector@

    __defined at:__ @edge-cases\/flam_functions.h 1:8@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
type Vector =
  (HsBindgen.Runtime.FLAM.WithFlam FC.CLong) Vector_Aux
