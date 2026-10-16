{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data UnrelatedDeclaration

instance Eq UnrelatedDeclaration

instance BG.Generic UnrelatedDeclaration

instance Show UnrelatedDeclaration

instance Marshal.StaticSize UnrelatedDeclaration

instance Marshal.ReadRaw UnrelatedDeclaration

instance Marshal.WriteRaw UnrelatedDeclaration

instance BG.Storable UnrelatedDeclaration

instance Struct.IsStruct UnrelatedDeclaration
