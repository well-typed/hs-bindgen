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

import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct Vector@

    __defined at:__ @edge-cases\/flam_functions.h 1:8@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
data Vector_Aux = Vector
  { vector_length :: RIP.CInt
    {- ^ __C declaration:__ @length@

         __defined at:__ @edge-cases\/flam_functions.h 2:7@

         __exported by:__ @edge-cases\/flam_functions.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Vector_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Vector_Aux where

  readRaw =
    \ptr0 ->
          pure Vector
      <*> HasCField.readRaw (RIP.Proxy @"vector_length") ptr0

instance Marshal.WriteRaw Vector_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_length2 ->
            HasCField.writeRaw (RIP.Proxy @"vector_length") ptr0 vector_length2

deriving via Marshal.EquivStorable Vector_Aux instance RIP.Storable Vector_Aux

instance HasCField.HasCField Vector_Aux "vector_length" where

  type CFieldType Vector_Aux "vector_length" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "vector_length" (RIP.Ptr Vector_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"vector_length")

instance FLAM.Offset RIP.CLong Vector_Aux where

  offset = \_ty0 -> 8

{-| __C declaration:__ @struct Vector@

    __defined at:__ @edge-cases\/flam_functions.h 1:8@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
type Vector = (FLAM.WithFlam RIP.CLong) Vector_Aux
