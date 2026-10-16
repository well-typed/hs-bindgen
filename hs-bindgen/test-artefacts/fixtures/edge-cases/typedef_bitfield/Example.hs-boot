{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data MyInt

instance Eq MyInt

instance BG.Generic MyInt

instance Ord MyInt

instance Read MyInt

instance Show MyInt

instance BG.Bitfield MyInt

instance BG.Bits MyInt

instance Bounded MyInt

instance Enum MyInt

instance BG.FiniteBits MyInt

instance BG.HasFFIType MyInt

instance Integral MyInt

instance BG.Ix MyInt

instance Num MyInt

instance BG.Prim MyInt

instance Marshal.ReadRaw MyInt

instance Real MyInt

instance Marshal.StaticSize MyInt

instance BG.Storable MyInt

instance Marshal.WriteRaw MyInt

data MyUInt

instance Eq MyUInt

instance BG.Generic MyUInt

instance Ord MyUInt

instance Read MyUInt

instance Show MyUInt

instance BG.Bitfield MyUInt

instance BG.Bits MyUInt

instance Bounded MyUInt

instance Enum MyUInt

instance BG.FiniteBits MyUInt

instance BG.HasFFIType MyUInt

instance Integral MyUInt

instance BG.Ix MyUInt

instance Num MyUInt

instance BG.Prim MyUInt

instance Marshal.ReadRaw MyUInt

instance Real MyUInt

instance Marshal.StaticSize MyUInt

instance BG.Storable MyUInt

instance Marshal.WriteRaw MyUInt

data MyLong

instance Eq MyLong

instance BG.Generic MyLong

instance Ord MyLong

instance Read MyLong

instance Show MyLong

instance BG.Bitfield MyLong

instance BG.Bits MyLong

instance Bounded MyLong

instance Enum MyLong

instance BG.FiniteBits MyLong

instance BG.HasFFIType MyLong

instance Integral MyLong

instance BG.Ix MyLong

instance Num MyLong

instance BG.Prim MyLong

instance Marshal.ReadRaw MyLong

instance Real MyLong

instance Marshal.StaticSize MyLong

instance BG.Storable MyLong

instance Marshal.WriteRaw MyLong

data MyStruct

instance Eq MyStruct

instance BG.Generic MyStruct

instance Show MyStruct

instance Marshal.StaticSize MyStruct

instance Marshal.ReadRaw MyStruct

instance Marshal.WriteRaw MyStruct

instance BG.Storable MyStruct

instance Struct.IsStruct MyStruct
