{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data MySym

instance Eq MySym

instance BG.Generic MySym

instance Ord MySym

instance Read MySym

instance Show MySym

instance BG.Bitfield MySym

instance BG.Bits MySym

instance Bounded MySym

instance Enum MySym

instance BG.FiniteBits MySym

instance BG.HasFFIType MySym

instance Integral MySym

instance BG.Ix MySym

instance Num MySym

instance BG.Prim MySym

instance Marshal.ReadRaw MySym

instance Real MySym

instance Marshal.StaticSize MySym

instance BG.Storable MySym

instance Marshal.WriteRaw MySym
