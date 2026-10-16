{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data FILE

instance Eq FILE

instance BG.Generic FILE

instance Show FILE

instance Marshal.StaticSize FILE

instance Marshal.ReadRaw FILE

instance Marshal.WriteRaw FILE

instance BG.Storable FILE

instance Struct.IsStruct FILE

data Size_t

instance Eq Size_t

instance BG.Generic Size_t

instance Ord Size_t

instance Read Size_t

instance Show Size_t

instance BG.Bitfield Size_t

instance BG.Bits Size_t

instance Bounded Size_t

instance Enum Size_t

instance BG.FiniteBits Size_t

instance BG.HasFFIType Size_t

instance Integral Size_t

instance BG.Ix Size_t

instance Num Size_t

instance BG.Prim Size_t

instance Marshal.ReadRaw Size_t

instance Real Size_t

instance Marshal.StaticSize Size_t

instance BG.Storable Size_t

instance Marshal.WriteRaw Size_t
