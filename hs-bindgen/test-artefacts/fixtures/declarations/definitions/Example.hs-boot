{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Eq, Show)

data X

instance Eq X

instance BG.Generic X

instance Show X

instance Marshal.StaticSize X

instance Marshal.ReadRaw X

instance Marshal.WriteRaw X

instance BG.Storable X

instance Struct.IsStruct X

data Y

instance BG.Generic Y

instance Marshal.StaticSize Y

instance Marshal.ReadRaw Y

instance Marshal.WriteRaw Y

instance BG.Storable Y

instance Union.IsUnion Y
