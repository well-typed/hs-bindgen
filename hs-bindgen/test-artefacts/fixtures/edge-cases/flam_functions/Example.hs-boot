{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Vector_Aux

instance Eq Vector_Aux

instance BG.Generic Vector_Aux

instance Show Vector_Aux

instance Marshal.StaticSize Vector_Aux

instance Marshal.ReadRaw Vector_Aux

instance Marshal.WriteRaw Vector_Aux

instance BG.Storable Vector_Aux

instance Struct.IsStruct Vector_Aux

type Vector = FLAM.WithFlam BG.CLong Vector_Aux
