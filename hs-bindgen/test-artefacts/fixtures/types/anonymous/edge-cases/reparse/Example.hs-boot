{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
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

data S_anon'anon'x_anon'x

instance Eq S_anon'anon'x_anon'x

instance BG.Generic S_anon'anon'x_anon'x

instance Show S_anon'anon'x_anon'x

instance Marshal.StaticSize S_anon'anon'x_anon'x

instance Marshal.ReadRaw S_anon'anon'x_anon'x

instance Marshal.WriteRaw S_anon'anon'x_anon'x

instance BG.Storable S_anon'anon'x_anon'x

instance Struct.IsStruct S_anon'anon'x_anon'x

data S_anon'anon'x

instance Eq S_anon'anon'x

instance BG.Generic S_anon'anon'x

instance Show S_anon'anon'x

instance Marshal.StaticSize S_anon'anon'x

instance Marshal.ReadRaw S_anon'anon'x

instance Marshal.WriteRaw S_anon'anon'x

instance BG.Storable S_anon'anon'x

instance Struct.IsStruct S_anon'anon'x

data S

instance Eq S

instance BG.Generic S

instance Show S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S
