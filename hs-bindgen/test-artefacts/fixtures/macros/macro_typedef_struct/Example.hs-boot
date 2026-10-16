{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data MY_TYPE

instance Eq MY_TYPE

instance BG.Generic MY_TYPE

instance Ord MY_TYPE

instance Read MY_TYPE

instance Show MY_TYPE

instance BG.Bitfield MY_TYPE

instance BG.Bits MY_TYPE

instance Bounded MY_TYPE

instance Enum MY_TYPE

instance BG.FiniteBits MY_TYPE

instance BG.HasFFIType MY_TYPE

instance Integral MY_TYPE

instance BG.Ix MY_TYPE

instance Num MY_TYPE

instance BG.Prim MY_TYPE

instance Marshal.ReadRaw MY_TYPE

instance Real MY_TYPE

instance Marshal.StaticSize MY_TYPE

instance BG.Storable MY_TYPE

instance Marshal.WriteRaw MY_TYPE

data Bar

instance Eq Bar

instance BG.Generic Bar

instance Show Bar

instance Marshal.StaticSize Bar

instance Marshal.ReadRaw Bar

instance Marshal.WriteRaw Bar

instance BG.Storable Bar

instance Struct.IsStruct Bar
