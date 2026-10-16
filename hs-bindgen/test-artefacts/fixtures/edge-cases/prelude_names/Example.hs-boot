{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data String'

instance Eq String'

instance BG.Generic String'

instance Show String'

instance Marshal.StaticSize String'

instance Marshal.ReadRaw String'

instance Marshal.WriteRaw String'

instance BG.Storable String'

instance Struct.IsStruct String'

data Show'

instance Eq Show'

instance BG.Generic Show'

instance Ord Show'

instance Read Show'

instance Show Show'

instance BG.Bitfield Show'

instance BG.Bits Show'

instance Bounded Show'

instance Enum Show'

instance BG.FiniteBits Show'

instance BG.HasFFIType Show'

instance Integral Show'

instance BG.Ix Show'

instance Num Show'

instance BG.Prim Show'

instance Marshal.ReadRaw Show'

instance Real Show'

instance Marshal.StaticSize Show'

instance BG.Storable Show'

instance Marshal.WriteRaw Show'

data CChar'

instance Eq CChar'

instance BG.Generic CChar'

instance Show CChar'

instance Marshal.StaticSize CChar'

instance Marshal.ReadRaw CChar'

instance Marshal.WriteRaw CChar'

instance BG.Storable CChar'

instance Struct.IsStruct CChar'

data C_types

instance Eq C_types

instance BG.Generic C_types

instance Ord C_types

instance BG.HasFFIType C_types

instance Marshal.StaticSize C_types

instance Marshal.ReadRaw C_types

instance Marshal.WriteRaw C_types

instance BG.Storable C_types

instance BG.Prim C_types

instance CEnum.CEnum C_types

instance CEnum.SequentialCEnum C_types

instance Show C_types

instance Read C_types

data Constants

instance Eq Constants

instance BG.Generic Constants

instance Ord Constants

instance BG.HasFFIType Constants

instance Marshal.StaticSize Constants

instance Marshal.ReadRaw Constants

instance Marshal.WriteRaw Constants

instance BG.Storable Constants

instance BG.Prim Constants

instance CEnum.CEnum Constants

instance CEnum.SequentialCEnum Constants

instance Show Constants

instance Read Constants

data Ordering

instance Eq Ordering

instance BG.Generic Ordering

instance Ord Ordering

instance BG.HasFFIType Ordering

instance Marshal.StaticSize Ordering

instance Marshal.ReadRaw Ordering

instance Marshal.WriteRaw Ordering

instance BG.Storable Ordering

instance BG.Prim Ordering

instance CEnum.CEnum Ordering

instance CEnum.SequentialCEnum Ordering

instance Show Ordering

instance Read Ordering

data Maybe

instance Eq Maybe

instance BG.Generic Maybe

instance Ord Maybe

instance Read Maybe

instance Show Maybe

instance BG.Bitfield Maybe

instance BG.Bits Maybe

instance Bounded Maybe

instance Enum Maybe

instance BG.FiniteBits Maybe

instance BG.HasFFIType Maybe

instance Integral Maybe

instance BG.Ix Maybe

instance Num Maybe

instance BG.Prim Maybe

instance Marshal.ReadRaw Maybe

instance Real Maybe

instance Marshal.StaticSize Maybe

instance BG.Storable Maybe

instance Marshal.WriteRaw Maybe

data Word

instance Eq Word

instance BG.Generic Word

instance Show Word

instance Marshal.StaticSize Word

instance Marshal.ReadRaw Word

instance Marshal.WriteRaw Word

instance BG.Storable Word

instance Struct.IsStruct Word

data Void

instance Eq Void

instance BG.Generic Void

instance Show Void

instance Marshal.StaticSize Void

instance Marshal.ReadRaw Void

instance Marshal.WriteRaw Void

instance BG.Storable Void

instance Struct.IsStruct Void
