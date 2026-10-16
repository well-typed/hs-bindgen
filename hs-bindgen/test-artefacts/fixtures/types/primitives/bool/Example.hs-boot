{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Bools1

instance Eq Bools1

instance BG.Generic Bools1

instance Show Bools1

instance Marshal.StaticSize Bools1

instance Marshal.ReadRaw Bools1

instance Marshal.WriteRaw Bools1

instance BG.Storable Bools1

instance Struct.IsStruct Bools1

data Bools2

instance Eq Bools2

instance BG.Generic Bools2

instance Show Bools2

instance Marshal.StaticSize Bools2

instance Marshal.ReadRaw Bools2

instance Marshal.WriteRaw Bools2

instance BG.Storable Bools2

instance Struct.IsStruct Bools2

data BOOL

instance Eq BOOL

instance BG.Generic BOOL

instance Ord BOOL

instance Read BOOL

instance Show BOOL

instance BG.Bitfield BOOL

instance BG.Bits BOOL

instance Bounded BOOL

instance Enum BOOL

instance BG.FiniteBits BOOL

instance BG.HasFFIType BOOL

instance Integral BOOL

instance BG.Ix BOOL

instance Num BOOL

instance BG.Prim BOOL

instance Marshal.ReadRaw BOOL

instance Real BOOL

instance Marshal.StaticSize BOOL

instance BG.Storable BOOL

instance Marshal.WriteRaw BOOL

data Bools3

instance Eq Bools3

instance BG.Generic Bools3

instance Show Bools3

instance Marshal.StaticSize Bools3

instance Marshal.ReadRaw Bools3

instance Marshal.WriteRaw Bools3

instance BG.Storable Bools3

instance Struct.IsStruct Bools3
