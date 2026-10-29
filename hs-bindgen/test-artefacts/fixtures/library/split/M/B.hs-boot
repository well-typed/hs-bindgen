{-# LANGUAGE NoImplicitPrelude #-}

module M.B where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Foo

instance Eq Foo

instance BG.Generic Foo

instance Show Foo

instance Marshal.StaticSize Foo

instance Marshal.ReadRaw Foo

instance Marshal.WriteRaw Foo

instance BG.Storable Foo

instance Struct.IsStruct Foo

data Bar

instance Eq Bar

instance BG.Generic Bar

instance Ord Bar

instance Read Bar

instance Show Bar

instance BG.Bitfield Bar

instance BG.Bits Bar

instance Bounded Bar

instance Enum Bar

instance BG.FiniteBits Bar

instance BG.HasFFIType Bar

instance Integral Bar

instance BG.Ix Bar

instance Num Bar

instance BG.Prim Bar

instance Marshal.ReadRaw Bar

instance Real Bar

instance Marshal.StaticSize Bar

instance BG.Storable Bar

instance Marshal.WriteRaw Bar
