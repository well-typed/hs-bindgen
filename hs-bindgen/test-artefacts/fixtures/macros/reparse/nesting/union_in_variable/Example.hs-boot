{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
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

data G1

instance BG.Generic G1

instance Marshal.StaticSize G1

instance Marshal.ReadRaw G1

instance Marshal.WriteRaw G1

instance BG.Storable G1

instance Union.IsUnion G1

data G2

instance BG.Generic G2

instance Marshal.StaticSize G2

instance Marshal.ReadRaw G2

instance Marshal.WriteRaw G2

instance BG.Storable G2

instance Union.IsUnion G2

data G3

instance BG.Generic G3

instance Marshal.StaticSize G3

instance Marshal.ReadRaw G3

instance Marshal.WriteRaw G3

instance BG.Storable G3

instance Union.IsUnion G3
