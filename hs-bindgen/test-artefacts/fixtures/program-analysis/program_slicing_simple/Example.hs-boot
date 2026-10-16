{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Uint32_t

instance Eq Uint32_t

instance BG.Generic Uint32_t

instance Ord Uint32_t

instance Read Uint32_t

instance Show Uint32_t

instance BG.Bitfield Uint32_t

instance BG.Bits Uint32_t

instance Bounded Uint32_t

instance Enum Uint32_t

instance BG.FiniteBits Uint32_t

instance BG.HasFFIType Uint32_t

instance Integral Uint32_t

instance BG.Ix Uint32_t

instance Num Uint32_t

instance BG.Prim Uint32_t

instance Marshal.ReadRaw Uint32_t

instance Real Uint32_t

instance Marshal.StaticSize Uint32_t

instance BG.Storable Uint32_t

instance Marshal.WriteRaw Uint32_t

data Foo

instance Eq Foo

instance BG.Generic Foo

instance Show Foo

instance BG.Storable Foo
