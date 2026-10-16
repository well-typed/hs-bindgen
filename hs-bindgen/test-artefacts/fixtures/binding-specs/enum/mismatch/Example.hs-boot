{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Pt

instance Eq Pt

instance BG.Generic Pt

instance Show Pt

instance Marshal.StaticSize Pt

instance Marshal.ReadRaw Pt

instance Marshal.WriteRaw Pt

instance BG.Storable Pt

instance Struct.IsStruct Pt
