{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Geometry.Scene where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

sCENE_MAX_SHAPES :: BG.CInt

data Scene

instance Eq Scene

instance BG.Generic Scene

instance Show Scene

instance Marshal.StaticSize Scene

instance Marshal.ReadRaw Scene

instance Marshal.WriteRaw Scene

instance BG.Storable Scene

instance Struct.IsStruct Scene
