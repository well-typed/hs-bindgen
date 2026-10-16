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

data T2

instance Eq T2

instance BG.Generic T2

instance Show T2

instance Marshal.StaticSize T2

instance Marshal.ReadRaw T2

instance Marshal.WriteRaw T2

instance BG.Storable T2

instance Struct.IsStruct T2

data T3

instance Eq T3

instance BG.Generic T3

instance Show T3

instance Marshal.StaticSize T3

instance Marshal.ReadRaw T3

instance Marshal.WriteRaw T3

instance BG.Storable T3

instance Struct.IsStruct T3

data T4

instance Eq T4

instance BG.Generic T4

instance Show T4

instance Marshal.StaticSize T4

instance Marshal.ReadRaw T4

instance Marshal.WriteRaw T4

instance BG.Storable T4

instance Struct.IsStruct T4
