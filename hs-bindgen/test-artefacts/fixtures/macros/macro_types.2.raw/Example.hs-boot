{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Floating, Fractional, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

data Tty

instance Eq Tty

instance BG.Generic Tty

instance Ord Tty

instance Read Tty

instance Show Tty

instance Enum Tty

instance Floating Tty

instance Fractional Tty

instance BG.HasFFIType Tty

instance Num Tty

instance BG.Prim Tty

instance Marshal.ReadRaw Tty

instance Real Tty

instance RealFloat Tty

instance RealFrac Tty

instance Marshal.StaticSize Tty

instance BG.Storable Tty

instance Marshal.WriteRaw Tty

data Boolean_T

instance Eq Boolean_T

instance BG.Generic Boolean_T

instance Ord Boolean_T

instance Read Boolean_T

instance Show Boolean_T

instance BG.Bitfield Boolean_T

instance BG.Bits Boolean_T

instance Bounded Boolean_T

instance Enum Boolean_T

instance BG.FiniteBits Boolean_T

instance BG.HasFFIType Boolean_T

instance Integral Boolean_T

instance BG.Ix Boolean_T

instance Num Boolean_T

instance BG.Prim Boolean_T

instance Marshal.ReadRaw Boolean_T

instance Real Boolean_T

instance Marshal.StaticSize Boolean_T

instance BG.Storable Boolean_T

instance Marshal.WriteRaw Boolean_T
