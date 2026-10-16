{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data FILE

instance Eq FILE

instance BG.Generic FILE

instance Ord FILE

instance Read FILE

instance Show FILE

instance BG.Bitfield FILE

instance BG.Bits FILE

instance Bounded FILE

instance Enum FILE

instance BG.FiniteBits FILE

instance BG.HasFFIType FILE

instance Integral FILE

instance BG.Ix FILE

instance Num FILE

instance BG.Prim FILE

instance Marshal.ReadRaw FILE

instance Real FILE

instance Marshal.StaticSize FILE

instance BG.Storable FILE

instance Marshal.WriteRaw FILE
