{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Int16_T

instance Eq Int16_T

instance BG.Generic Int16_T

instance Ord Int16_T

instance Read Int16_T

instance Show Int16_T

instance BG.Bitfield Int16_T

instance BG.Bits Int16_T

instance Bounded Int16_T

instance Enum Int16_T

instance BG.FiniteBits Int16_T

instance BG.HasFFIType Int16_T

instance Integral Int16_T

instance BG.Ix Int16_T

instance Num Int16_T

instance BG.Prim Int16_T

instance Marshal.ReadRaw Int16_T

instance Real Int16_T

instance Marshal.StaticSize Int16_T

instance BG.Storable Int16_T

instance Marshal.WriteRaw Int16_T

data Int32_T

instance Eq Int32_T

instance BG.Generic Int32_T

instance Ord Int32_T

instance Read Int32_T

instance Show Int32_T

instance BG.Bitfield Int32_T

instance BG.Bits Int32_T

instance Bounded Int32_T

instance Enum Int32_T

instance BG.FiniteBits Int32_T

instance BG.HasFFIType Int32_T

instance Integral Int32_T

instance BG.Ix Int32_T

instance Num Int32_T

instance BG.Prim Int32_T

instance Marshal.ReadRaw Int32_T

instance Real Int32_T

instance Marshal.StaticSize Int32_T

instance BG.Storable Int32_T

instance Marshal.WriteRaw Int32_T

data Int64_T

instance Eq Int64_T

instance BG.Generic Int64_T

instance Ord Int64_T

instance Read Int64_T

instance Show Int64_T

instance BG.Bitfield Int64_T

instance BG.Bits Int64_T

instance Bounded Int64_T

instance Enum Int64_T

instance BG.FiniteBits Int64_T

instance BG.HasFFIType Int64_T

instance Integral Int64_T

instance BG.Ix Int64_T

instance Num Int64_T

instance BG.Prim Int64_T

instance Marshal.ReadRaw Int64_T

instance Real Int64_T

instance Marshal.StaticSize Int64_T

instance BG.Storable Int64_T

instance Marshal.WriteRaw Int64_T

data Cint16_T

instance Eq Cint16_T

instance BG.Generic Cint16_T

instance Show Cint16_T

instance Marshal.StaticSize Cint16_T

instance Marshal.ReadRaw Cint16_T

instance Marshal.WriteRaw Cint16_T

instance BG.Storable Cint16_T

instance Struct.IsStruct Cint16_T

data B

instance Eq B

instance BG.Generic B

instance Show B

instance Marshal.StaticSize B

instance Marshal.ReadRaw B

instance Marshal.WriteRaw B

instance BG.Storable B

instance Struct.IsStruct B

data A

instance Eq A

instance BG.Generic A

instance Show A

instance Marshal.StaticSize A

instance Marshal.ReadRaw A

instance Marshal.WriteRaw A

instance BG.Storable A

instance Struct.IsStruct A

data C
