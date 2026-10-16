{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Read, Show)

data Uint8_enum

instance Eq Uint8_enum

instance BG.Generic Uint8_enum

instance Ord Uint8_enum

instance BG.HasFFIType Uint8_enum

instance Marshal.StaticSize Uint8_enum

instance Marshal.ReadRaw Uint8_enum

instance Marshal.WriteRaw Uint8_enum

instance BG.Storable Uint8_enum

instance BG.Prim Uint8_enum

instance CEnum.CEnum Uint8_enum

instance Show Uint8_enum

instance Read Uint8_enum
