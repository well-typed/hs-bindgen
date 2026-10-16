{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Piyo

instance Eq Piyo

instance BG.Generic Piyo

instance Show Piyo

instance Marshal.StaticSize Piyo

instance Marshal.ReadRaw Piyo

instance Marshal.WriteRaw Piyo

instance BG.Storable Piyo

instance Struct.IsStruct Piyo
