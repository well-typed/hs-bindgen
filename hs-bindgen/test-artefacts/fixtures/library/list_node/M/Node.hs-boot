{-# LANGUAGE NoImplicitPrelude #-}

module M.Node where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Node

instance Eq Node

instance BG.Generic Node

instance Show Node

instance Marshal.StaticSize Node

instance Marshal.ReadRaw Node

instance Marshal.WriteRaw Node

instance BG.Storable Node

instance Struct.IsStruct Node
