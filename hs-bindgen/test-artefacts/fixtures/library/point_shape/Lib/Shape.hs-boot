{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Shape where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

data Shape

instance Eq Shape

instance BG.Generic Shape

instance Show Shape

instance Marshal.StaticSize Shape

instance Marshal.ReadRaw Shape

instance Marshal.WriteRaw Shape

instance BG.Storable Shape

instance Struct.IsStruct Shape

data On_draw_Aux

instance BG.Generic On_draw_Aux

instance BG.ToFunPtr On_draw_Aux

instance BG.FromFunPtr On_draw_Aux

data On_draw

instance Eq On_draw

instance BG.Generic On_draw

instance Ord On_draw

instance Show On_draw

instance BG.HasFFIType On_draw

instance Marshal.ReadRaw On_draw

instance Marshal.StaticSize On_draw

instance BG.Storable On_draw

instance Marshal.WriteRaw On_draw
