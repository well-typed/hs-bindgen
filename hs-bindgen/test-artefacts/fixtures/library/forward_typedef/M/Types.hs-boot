{-# LANGUAGE NoImplicitPrelude #-}

module M.Types where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Lib_status

instance Eq Lib_status

instance BG.Generic Lib_status

instance Ord Lib_status

instance Read Lib_status

instance Show Lib_status

instance BG.Bitfield Lib_status

instance BG.Bits Lib_status

instance Bounded Lib_status

instance Enum Lib_status

instance BG.FiniteBits Lib_status

instance BG.HasFFIType Lib_status

instance Integral Lib_status

instance BG.Ix Lib_status

instance Num Lib_status

instance BG.Prim Lib_status

instance Marshal.ReadRaw Lib_status

instance Real Lib_status

instance Marshal.StaticSize Lib_status

instance BG.Storable Lib_status

instance Marshal.WriteRaw Lib_status
