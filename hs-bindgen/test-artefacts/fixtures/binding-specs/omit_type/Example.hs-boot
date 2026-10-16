{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Sym

instance Eq Sym

instance BG.Generic Sym

instance Ord Sym

instance Read Sym

instance Show Sym

instance BG.Bitfield Sym

instance BG.Bits Sym

instance Bounded Sym

instance Enum Sym

instance BG.FiniteBits Sym

instance BG.HasFFIType Sym

instance Integral Sym

instance BG.Ix Sym

instance Num Sym

instance BG.Prim Sym

instance Marshal.ReadRaw Sym

instance Real Sym

instance Marshal.StaticSize Sym

instance BG.Storable Sym

instance Marshal.WriteRaw Sym
