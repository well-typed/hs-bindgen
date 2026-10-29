{-# LANGUAGE NoImplicitPrelude #-}

module M.Shape where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Shape_status

instance Eq Shape_status

instance BG.Generic Shape_status

instance Ord Shape_status

instance Read Shape_status

instance Show Shape_status

instance BG.Bitfield Shape_status

instance BG.Bits Shape_status

instance Bounded Shape_status

instance Enum Shape_status

instance BG.FiniteBits Shape_status

instance BG.HasFFIType Shape_status

instance Integral Shape_status

instance BG.Ix Shape_status

instance Num Shape_status

instance BG.Prim Shape_status

instance Marshal.ReadRaw Shape_status

instance Real Shape_status

instance Marshal.StaticSize Shape_status

instance BG.Storable Shape_status

instance Marshal.WriteRaw Shape_status
