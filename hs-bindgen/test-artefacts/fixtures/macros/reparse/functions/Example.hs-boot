{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data MY_INT

instance Eq MY_INT

instance BG.Generic MY_INT

instance Ord MY_INT

instance Read MY_INT

instance Show MY_INT

instance BG.Bitfield MY_INT

instance BG.Bits MY_INT

instance Bounded MY_INT

instance Enum MY_INT

instance BG.FiniteBits MY_INT

instance BG.HasFFIType MY_INT

instance Integral MY_INT

instance BG.Ix MY_INT

instance Num MY_INT

instance BG.Prim MY_INT

instance Marshal.ReadRaw MY_INT

instance Real MY_INT

instance Marshal.StaticSize MY_INT

instance BG.Storable MY_INT

instance Marshal.WriteRaw MY_INT

data My_int_t

instance Eq My_int_t

instance BG.Generic My_int_t

instance Ord My_int_t

instance Read My_int_t

instance Show My_int_t

instance BG.Bitfield My_int_t

instance BG.Bits My_int_t

instance Bounded My_int_t

instance Enum My_int_t

instance BG.FiniteBits My_int_t

instance BG.HasFFIType My_int_t

instance Integral My_int_t

instance BG.Ix My_int_t

instance Num My_int_t

instance BG.Prim My_int_t

instance Marshal.ReadRaw My_int_t

instance Real My_int_t

instance Marshal.StaticSize My_int_t

instance BG.Storable My_int_t

instance Marshal.WriteRaw My_int_t
