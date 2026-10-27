{-# LANGUAGE NoImplicitPrelude #-}

module M.Draw where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Draw_status

instance Eq Draw_status

instance BG.Generic Draw_status

instance Ord Draw_status

instance Read Draw_status

instance Show Draw_status

instance BG.Bitfield Draw_status

instance BG.Bits Draw_status

instance Bounded Draw_status

instance Enum Draw_status

instance BG.FiniteBits Draw_status

instance BG.HasFFIType Draw_status

instance Integral Draw_status

instance BG.Ix Draw_status

instance Num Draw_status

instance BG.Prim Draw_status

instance Marshal.ReadRaw Draw_status

instance Real Draw_status

instance Marshal.StaticSize Draw_status

instance BG.Storable Draw_status

instance Marshal.WriteRaw Draw_status
