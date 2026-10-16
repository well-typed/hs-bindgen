{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Stdlib_CBool

instance Eq Stdlib_CBool

instance BG.Generic Stdlib_CBool

instance Ord Stdlib_CBool

instance Read Stdlib_CBool

instance Show Stdlib_CBool

instance BG.Bitfield Stdlib_CBool

instance BG.Bits Stdlib_CBool

instance Bounded Stdlib_CBool

instance Enum Stdlib_CBool

instance BG.FiniteBits Stdlib_CBool

instance BG.HasFFIType Stdlib_CBool

instance Integral Stdlib_CBool

instance BG.Ix Stdlib_CBool

instance Num Stdlib_CBool

instance BG.Prim Stdlib_CBool

instance Marshal.ReadRaw Stdlib_CBool

instance Real Stdlib_CBool

instance Marshal.StaticSize Stdlib_CBool

instance BG.Storable Stdlib_CBool

instance Marshal.WriteRaw Stdlib_CBool
