{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data T1

instance Eq T1

instance BG.Generic T1

instance Ord T1

instance Read T1

instance Show T1

instance BG.Bitfield T1

instance BG.Bits T1

instance Bounded T1

instance Enum T1

instance BG.FiniteBits T1

instance BG.HasFFIType T1

instance Integral T1

instance BG.Ix T1

instance Num T1

instance BG.Prim T1

instance Marshal.ReadRaw T1

instance Real T1

instance Marshal.StaticSize T1

instance BG.Storable T1

instance Marshal.WriteRaw T1

data T2

instance Eq T2

instance BG.Generic T2

instance Ord T2

instance Read T2

instance Show T2

instance BG.Bitfield T2

instance BG.Bits T2

instance Bounded T2

instance Enum T2

instance BG.FiniteBits T2

instance BG.HasFFIType T2

instance Integral T2

instance BG.Ix T2

instance Num T2

instance BG.Prim T2

instance Marshal.ReadRaw T2

instance Real T2

instance Marshal.StaticSize T2

instance BG.Storable T2

instance Marshal.WriteRaw T2

data M1

instance Eq M1

instance BG.Generic M1

instance Ord M1

instance Read M1

instance Show M1

instance BG.Bitfield M1

instance BG.Bits M1

instance Bounded M1

instance Enum M1

instance BG.FiniteBits M1

instance BG.HasFFIType M1

instance Integral M1

instance BG.Ix M1

instance Num M1

instance BG.Prim M1

instance Marshal.ReadRaw M1

instance Real M1

instance Marshal.StaticSize M1

instance BG.Storable M1

instance Marshal.WriteRaw M1

data M2

instance Eq M2

instance BG.Generic M2

instance Ord M2

instance Read M2

instance Show M2

instance BG.Bitfield M2

instance BG.Bits M2

instance Bounded M2

instance Enum M2

instance BG.FiniteBits M2

instance BG.HasFFIType M2

instance Integral M2

instance BG.Ix M2

instance Num M2

instance BG.Prim M2

instance Marshal.ReadRaw M2

instance Real M2

instance Marshal.StaticSize M2

instance BG.Storable M2

instance Marshal.WriteRaw M2

data M3

instance Eq M3

instance BG.Generic M3

instance Ord M3

instance Show M3

instance BG.HasFFIType M3

instance Marshal.ReadRaw M3

instance Marshal.StaticSize M3

instance BG.Storable M3

instance Marshal.WriteRaw M3

data ExampleStruct

instance Eq ExampleStruct

instance BG.Generic ExampleStruct

instance Show ExampleStruct

instance Marshal.StaticSize ExampleStruct

instance Marshal.ReadRaw ExampleStruct

instance Marshal.WriteRaw ExampleStruct

instance BG.Storable ExampleStruct

instance Struct.IsStruct ExampleStruct

data Uint64_t

instance Eq Uint64_t

instance BG.Generic Uint64_t

instance Ord Uint64_t

instance Read Uint64_t

instance Show Uint64_t

instance BG.Bitfield Uint64_t

instance BG.Bits Uint64_t

instance Bounded Uint64_t

instance Enum Uint64_t

instance BG.FiniteBits Uint64_t

instance BG.HasFFIType Uint64_t

instance Integral Uint64_t

instance BG.Ix Uint64_t

instance Num Uint64_t

instance BG.Prim Uint64_t

instance Marshal.ReadRaw Uint64_t

instance Real Uint64_t

instance Marshal.StaticSize Uint64_t

instance BG.Storable Uint64_t

instance Marshal.WriteRaw Uint64_t

data Foo

instance Eq Foo

instance BG.Generic Foo

instance Show Foo

instance Marshal.StaticSize Foo

instance Marshal.ReadRaw Foo

instance Marshal.WriteRaw Foo

instance BG.Storable Foo

instance Struct.IsStruct Foo
