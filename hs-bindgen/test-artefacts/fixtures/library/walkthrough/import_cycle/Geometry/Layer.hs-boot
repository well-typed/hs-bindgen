{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Geometry.Layer where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

lAYER_MAX_SHAPES :: BG.CInt

data Layer

instance Eq Layer

instance BG.Generic Layer

instance Show Layer

instance Marshal.StaticSize Layer

instance Marshal.ReadRaw Layer

instance Marshal.WriteRaw Layer

instance BG.Storable Layer

instance Struct.IsStruct Layer

data Layer_paint_Aux

instance BG.Generic Layer_paint_Aux

data Layer_paint

instance Eq Layer_paint

instance BG.Generic Layer_paint

instance Ord Layer_paint

instance Show Layer_paint

instance BG.HasFFIType Layer_paint

instance Marshal.ReadRaw Layer_paint

instance Marshal.StaticSize Layer_paint

instance BG.Storable Layer_paint

instance Marshal.WriteRaw Layer_paint
