{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data B

instance Eq B

instance BG.Generic B

instance Ord B

instance Read B

instance Show B

instance BG.Bitfield B

instance BG.Bits B

instance Bounded B

instance Enum B

instance BG.FiniteBits B

instance BG.HasFFIType B

instance Integral B

instance BG.Ix B

instance Num B

instance BG.Prim B

instance Marshal.ReadRaw B

instance Real B

instance Marshal.StaticSize B

instance BG.Storable B

instance Marshal.WriteRaw B

data S

instance Eq S

instance BG.Generic S

instance Show S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S
