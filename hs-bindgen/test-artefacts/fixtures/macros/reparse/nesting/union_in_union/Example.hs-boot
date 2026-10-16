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

data T1_x

instance BG.Generic T1_x

instance Marshal.StaticSize T1_x

instance Marshal.ReadRaw T1_x

instance Marshal.WriteRaw T1_x

instance BG.Storable T1_x

instance Union.IsUnion T1_x

data T1

instance BG.Generic T1

instance Marshal.StaticSize T1

instance Marshal.ReadRaw T1

instance Marshal.WriteRaw T1

instance BG.Storable T1

instance Union.IsUnion T1

data T2

instance BG.Generic T2

instance Marshal.StaticSize T2

instance Marshal.ReadRaw T2

instance Marshal.WriteRaw T2

instance BG.Storable T2

instance Union.IsUnion T2

data T2_x

instance BG.Generic T2_x

instance Marshal.StaticSize T2_x

instance Marshal.ReadRaw T2_x

instance Marshal.WriteRaw T2_x

instance BG.Storable T2_x

instance Union.IsUnion T2_x

data T3

instance BG.Generic T3

instance Marshal.StaticSize T3

instance Marshal.ReadRaw T3

instance Marshal.WriteRaw T3

instance BG.Storable T3

instance Union.IsUnion T3

data T3_x

instance BG.Generic T3_x

instance Marshal.StaticSize T3_x

instance Marshal.ReadRaw T3_x

instance Marshal.WriteRaw T3_x

instance BG.Storable T3_x

instance Union.IsUnion T3_x
