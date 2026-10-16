{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data A

instance Eq A

instance BG.Generic A

instance Ord A

instance Read A

instance Show A

instance BG.Bitfield A

instance BG.Bits A

instance Bounded A

instance Enum A

instance BG.FiniteBits A

instance BG.HasFFIType A

instance Integral A

instance BG.Ix A

instance Num A

instance BG.Prim A

instance Marshal.ReadRaw A

instance Real A

instance Marshal.StaticSize A

instance BG.Storable A

instance Marshal.WriteRaw A
