{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

data On_event_Aux

instance BG.Generic On_event_Aux

instance BG.ToFunPtr On_event_Aux

instance BG.FromFunPtr On_event_Aux

data On_event

instance Eq On_event

instance BG.Generic On_event

instance Ord On_event

instance Show On_event

instance BG.HasFFIType On_event

instance Marshal.ReadRaw On_event

instance Marshal.StaticSize On_event

instance BG.Storable On_event

instance Marshal.WriteRaw On_event

data Buffer_Aux

instance Eq Buffer_Aux

instance BG.Generic Buffer_Aux

instance Show Buffer_Aux

instance Marshal.StaticSize Buffer_Aux

instance Marshal.ReadRaw Buffer_Aux

instance Marshal.WriteRaw Buffer_Aux

instance BG.Storable Buffer_Aux

instance Struct.IsStruct Buffer_Aux

type Buffer = FLAM.WithFlam BG.CChar Buffer_Aux
