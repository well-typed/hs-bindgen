{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Read, Show)

data Test

instance Eq Test

instance BG.Generic Test

instance Ord Test

instance BG.HasFFIType Test

instance Marshal.StaticSize Test

instance Marshal.ReadRaw Test

instance Marshal.WriteRaw Test

instance BG.Storable Test

instance BG.Prim Test

instance CEnum.CEnum Test

instance CEnum.SequentialCEnum Test

instance Show Test

instance Read Test
