{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Uint

instance Eq Uint

instance BG.Generic Uint

instance Ord Uint

instance Read Uint

instance Show Uint

instance BG.Bitfield Uint

instance BG.Bits Uint

instance Bounded Uint

instance Enum Uint

instance BG.FiniteBits Uint

instance BG.HasFFIType Uint

instance Integral Uint

instance BG.Ix Uint

instance Num Uint

instance BG.Prim Uint

instance Marshal.ReadRaw Uint

instance Real Uint

instance Marshal.StaticSize Uint

instance BG.Storable Uint

instance Marshal.WriteRaw Uint

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

data Bar1_t

instance Eq Bar1_t

instance BG.Generic Bar1_t

instance Show Bar1_t

instance Marshal.StaticSize Bar1_t

instance Marshal.ReadRaw Bar1_t

instance Marshal.WriteRaw Bar1_t

instance BG.Storable Bar1_t

instance Struct.IsStruct Bar1_t

data Bar2_t

instance Eq Bar2_t

instance BG.Generic Bar2_t

instance Show Bar2_t

instance Marshal.StaticSize Bar2_t

instance Marshal.ReadRaw Bar2_t

instance Marshal.WriteRaw Bar2_t

instance BG.Storable Bar2_t

instance Struct.IsStruct Bar2_t

data Bar3_t

instance Eq Bar3_t

instance BG.Generic Bar3_t

instance Show Bar3_t

instance Marshal.StaticSize Bar3_t

instance Marshal.ReadRaw Bar3_t

instance Marshal.WriteRaw Bar3_t

instance BG.Storable Bar3_t

instance Struct.IsStruct Bar3_t

data Baz2_t

instance Eq Baz2_t

instance BG.Generic Baz2_t

instance Ord Baz2_t

instance BG.HasFFIType Baz2_t

instance Marshal.StaticSize Baz2_t

instance Marshal.ReadRaw Baz2_t

instance Marshal.WriteRaw Baz2_t

instance BG.Storable Baz2_t

instance BG.Prim Baz2_t

instance CEnum.CEnum Baz2_t

instance CEnum.SequentialCEnum Baz2_t

instance Show Baz2_t

instance Read Baz2_t

data Baz3_t

instance Eq Baz3_t

instance BG.Generic Baz3_t

instance Ord Baz3_t

instance BG.HasFFIType Baz3_t

instance Marshal.StaticSize Baz3_t

instance Marshal.ReadRaw Baz3_t

instance Marshal.WriteRaw Baz3_t

instance BG.Storable Baz3_t

instance BG.Prim Baz3_t

instance CEnum.CEnum Baz3_t

instance CEnum.SequentialCEnum Baz3_t

instance Show Baz3_t

instance Read Baz3_t

data Baz4_t

instance Eq Baz4_t

instance BG.Generic Baz4_t

instance Ord Baz4_t

instance BG.HasFFIType Baz4_t

instance Marshal.StaticSize Baz4_t

instance Marshal.ReadRaw Baz4_t

instance Marshal.WriteRaw Baz4_t

instance BG.Storable Baz4_t

instance BG.Prim Baz4_t

instance CEnum.CEnum Baz4_t

instance CEnum.SequentialCEnum Baz4_t

instance Show Baz4_t

instance Read Baz4_t
