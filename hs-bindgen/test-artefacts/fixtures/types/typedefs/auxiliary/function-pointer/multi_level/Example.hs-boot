{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data F1_Aux

instance BG.Generic F1_Aux

instance BG.ToFunPtr F1_Aux

instance BG.FromFunPtr F1_Aux

data F1

instance Eq F1

instance BG.Generic F1

instance Ord F1

instance Show F1

instance BG.HasFFIType F1

instance Marshal.ReadRaw F1

instance Marshal.StaticSize F1

instance BG.Storable F1

instance Marshal.WriteRaw F1

data F2_Aux

instance BG.Generic F2_Aux

instance BG.ToFunPtr F2_Aux

instance BG.FromFunPtr F2_Aux

data F2

instance Eq F2

instance BG.Generic F2

instance Ord F2

instance Show F2

instance BG.HasFFIType F2

instance Marshal.ReadRaw F2

instance Marshal.StaticSize F2

instance BG.Storable F2

instance Marshal.WriteRaw F2

data F3_Aux

instance BG.Generic F3_Aux

instance BG.ToFunPtr F3_Aux

instance BG.FromFunPtr F3_Aux

data F3

instance Eq F3

instance BG.Generic F3

instance Ord F3

instance Show F3

instance BG.HasFFIType F3

instance Marshal.ReadRaw F3

instance Marshal.StaticSize F3

instance BG.Storable F3

instance Marshal.WriteRaw F3

data F4_Aux

instance BG.Generic F4_Aux

instance BG.ToFunPtr F4_Aux

instance BG.FromFunPtr F4_Aux

data F4

instance Eq F4

instance BG.Generic F4

instance Ord F4

instance Show F4

instance BG.HasFFIType F4

instance Marshal.ReadRaw F4

instance Marshal.StaticSize F4

instance BG.Storable F4

instance Marshal.WriteRaw F4

data F5_Aux

instance BG.Generic F5_Aux

instance BG.ToFunPtr F5_Aux

instance BG.FromFunPtr F5_Aux

data F5

instance Eq F5

instance BG.Generic F5

instance Ord F5

instance Show F5

instance BG.HasFFIType F5

instance Marshal.ReadRaw F5

instance Marshal.StaticSize F5

instance BG.Storable F5

instance Marshal.WriteRaw F5

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

data F6_Aux

instance BG.Generic F6_Aux

instance BG.ToFunPtr F6_Aux

instance BG.FromFunPtr F6_Aux

data F6

instance Eq F6

instance BG.Generic F6

instance Ord F6

instance Show F6

instance BG.HasFFIType F6

instance Marshal.ReadRaw F6

instance Marshal.StaticSize F6

instance BG.Storable F6

instance Marshal.WriteRaw F6
