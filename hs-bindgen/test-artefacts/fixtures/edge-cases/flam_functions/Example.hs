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
    ( Example.Vector_Aux(..)
    , Example.Vector
    )
  where

import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct Vector@

    __defined at:__ @edge-cases\/flam_functions.h 1:8@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
data Vector_Aux = Vector
  { vector_length :: BG.CInt
    {- ^ __C declaration:__ @length@

         __defined at:__ @edge-cases\/flam_functions.h 2:7@

         __exported by:__ @edge-cases\/flam_functions.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Vector_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Vector_Aux where

  readRaw =
    \ptr0 ->
          pure Vector
      <*> HasCField.readRaw (BG.Proxy @"vector_length") ptr0

instance Marshal.WriteRaw Vector_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_length2 ->
            HasCField.writeRaw (BG.Proxy @"vector_length") ptr0 vector_length2

deriving via Marshal.EquivStorable Vector_Aux instance BG.Storable Vector_Aux

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "vector_length" Vector_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         Vector {vector_length = y1}, BG.getField @"vector_length" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "vector_length" (BG.Ptr Vector_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"vector_length")

instance HasCField.HasCField Vector_Aux "vector_length" where

  type CFieldType Vector_Aux "vector_length" = BG.CInt

  offset# = \_ -> \_ -> 0

instance FLAM.Offset BG.CLong Vector_Aux where

  offset = \_proxy0 -> 8

{-| __C declaration:__ @struct Vector@

    __defined at:__ @edge-cases\/flam_functions.h 1:8@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
type Vector = FLAM.WithFlam BG.CLong Vector_Aux
