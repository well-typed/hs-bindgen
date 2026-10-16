{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Int_t

instance Eq Int_t

instance BG.Generic Int_t

instance Ord Int_t

instance Read Int_t

instance Show Int_t

instance BG.Bitfield Int_t

instance BG.Bits Int_t

instance Bounded Int_t

instance Enum Int_t

instance BG.FiniteBits Int_t

instance BG.HasFFIType Int_t

instance Integral Int_t

instance BG.Ix Int_t

instance Num Int_t

instance BG.Prim Int_t

instance Marshal.ReadRaw Int_t

instance Real Int_t

instance Marshal.StaticSize Int_t

instance BG.Storable Int_t

instance Marshal.WriteRaw Int_t

data X

instance Eq X

instance BG.Generic X

instance Show X

instance Marshal.StaticSize X

instance Marshal.ReadRaw X

instance Marshal.WriteRaw X

instance BG.Storable X

instance Struct.IsStruct X

data Y

instance BG.Generic Y

instance Marshal.StaticSize Y

instance Marshal.ReadRaw Y

instance Marshal.WriteRaw Y

instance BG.Storable Y

instance Union.IsUnion Y
