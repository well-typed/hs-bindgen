{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Thing

instance Eq Thing

instance BG.Generic Thing

instance Show Thing

instance Marshal.StaticSize Thing

instance Marshal.ReadRaw Thing

instance Marshal.WriteRaw Thing

instance BG.Storable Thing

instance Struct.IsStruct Thing
