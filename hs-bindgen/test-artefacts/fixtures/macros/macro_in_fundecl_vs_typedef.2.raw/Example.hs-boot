{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data TC

instance Eq TC

instance BG.Generic TC

instance Ord TC

instance Read TC

instance Show TC

instance BG.Bitfield TC

instance BG.Bits TC

instance Bounded TC

instance Enum TC

instance BG.FiniteBits TC

instance BG.HasFFIType TC

instance Integral TC

instance BG.Ix TC

instance Num TC

instance BG.Prim TC

instance Marshal.ReadRaw TC

instance Real TC

instance Marshal.StaticSize TC

instance BG.Storable TC

instance Marshal.WriteRaw TC

data Struct1

instance Eq Struct1

instance BG.Generic Struct1

instance Show Struct1

instance Marshal.StaticSize Struct1

instance Marshal.ReadRaw Struct1

instance Marshal.WriteRaw Struct1

instance BG.Storable Struct1

instance Struct.IsStruct Struct1

data Struct2

instance Eq Struct2

instance BG.Generic Struct2

instance Show Struct2

instance Marshal.StaticSize Struct2

instance Marshal.ReadRaw Struct2

instance Marshal.WriteRaw Struct2

instance BG.Storable Struct2

instance Struct.IsStruct Struct2

data Struct3

instance Eq Struct3

instance BG.Generic Struct3

instance Show Struct3

instance Marshal.StaticSize Struct3

instance Marshal.ReadRaw Struct3

instance Marshal.WriteRaw Struct3

instance BG.Storable Struct3

instance Struct.IsStruct Struct3

data Struct3_t

instance Eq Struct3_t

instance BG.Generic Struct3_t

instance Show Struct3_t

instance Marshal.ReadRaw Struct3_t

instance Marshal.StaticSize Struct3_t

instance BG.Storable Struct3_t

instance Marshal.WriteRaw Struct3_t

data Struct4

instance Eq Struct4

instance BG.Generic Struct4

instance Show Struct4

instance Marshal.StaticSize Struct4

instance Marshal.ReadRaw Struct4

instance Marshal.WriteRaw Struct4

instance BG.Storable Struct4

instance Struct.IsStruct Struct4
