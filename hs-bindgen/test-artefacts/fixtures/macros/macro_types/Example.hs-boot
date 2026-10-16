{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Floating, Fractional, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

data PtrInt

instance Eq PtrInt

instance BG.Generic PtrInt

instance Ord PtrInt

instance Show PtrInt

instance BG.HasFFIType PtrInt

instance Marshal.ReadRaw PtrInt

instance Marshal.StaticSize PtrInt

instance BG.Storable PtrInt

instance Marshal.WriteRaw PtrInt

data ShortInt

instance Eq ShortInt

instance BG.Generic ShortInt

instance Ord ShortInt

instance Read ShortInt

instance Show ShortInt

instance BG.Bitfield ShortInt

instance BG.Bits ShortInt

instance Bounded ShortInt

instance Enum ShortInt

instance BG.FiniteBits ShortInt

instance BG.HasFFIType ShortInt

instance Integral ShortInt

instance BG.Ix ShortInt

instance Num ShortInt

instance BG.Prim ShortInt

instance Marshal.ReadRaw ShortInt

instance Real ShortInt

instance Marshal.StaticSize ShortInt

instance BG.Storable ShortInt

instance Marshal.WriteRaw ShortInt

data SignedShortInt

instance Eq SignedShortInt

instance BG.Generic SignedShortInt

instance Ord SignedShortInt

instance Read SignedShortInt

instance Show SignedShortInt

instance BG.Bitfield SignedShortInt

instance BG.Bits SignedShortInt

instance Bounded SignedShortInt

instance Enum SignedShortInt

instance BG.FiniteBits SignedShortInt

instance BG.HasFFIType SignedShortInt

instance Integral SignedShortInt

instance BG.Ix SignedShortInt

instance Num SignedShortInt

instance BG.Prim SignedShortInt

instance Marshal.ReadRaw SignedShortInt

instance Real SignedShortInt

instance Marshal.StaticSize SignedShortInt

instance BG.Storable SignedShortInt

instance Marshal.WriteRaw SignedShortInt

data UnsignedShortInt

instance Eq UnsignedShortInt

instance BG.Generic UnsignedShortInt

instance Ord UnsignedShortInt

instance Read UnsignedShortInt

instance Show UnsignedShortInt

instance BG.Bitfield UnsignedShortInt

instance BG.Bits UnsignedShortInt

instance Bounded UnsignedShortInt

instance Enum UnsignedShortInt

instance BG.FiniteBits UnsignedShortInt

instance BG.HasFFIType UnsignedShortInt

instance Integral UnsignedShortInt

instance BG.Ix UnsignedShortInt

instance Num UnsignedShortInt

instance BG.Prim UnsignedShortInt

instance Marshal.ReadRaw UnsignedShortInt

instance Real UnsignedShortInt

instance Marshal.StaticSize UnsignedShortInt

instance BG.Storable UnsignedShortInt

instance Marshal.WriteRaw UnsignedShortInt

data PtrPtrChar

instance Eq PtrPtrChar

instance BG.Generic PtrPtrChar

instance Ord PtrPtrChar

instance Show PtrPtrChar

instance BG.HasFFIType PtrPtrChar

instance Marshal.ReadRaw PtrPtrChar

instance Marshal.StaticSize PtrPtrChar

instance BG.Storable PtrPtrChar

instance Marshal.WriteRaw PtrPtrChar

data MTy

instance Eq MTy

instance BG.Generic MTy

instance Ord MTy

instance Read MTy

instance Show MTy

instance Enum MTy

instance Floating MTy

instance Fractional MTy

instance BG.HasFFIType MTy

instance Num MTy

instance BG.Prim MTy

instance Marshal.ReadRaw MTy

instance Real MTy

instance RealFloat MTy

instance RealFrac MTy

instance Marshal.StaticSize MTy

instance BG.Storable MTy

instance Marshal.WriteRaw MTy

data Tty

instance Eq Tty

instance BG.Generic Tty

instance Ord Tty

instance Read Tty

instance Show Tty

instance Enum Tty

instance Floating Tty

instance Fractional Tty

instance BG.HasFFIType Tty

instance Num Tty

instance BG.Prim Tty

instance Marshal.ReadRaw Tty

instance Real Tty

instance RealFloat Tty

instance RealFrac Tty

instance Marshal.StaticSize Tty

instance BG.Storable Tty

instance Marshal.WriteRaw Tty

data UINT8_T

instance Eq UINT8_T

instance BG.Generic UINT8_T

instance Ord UINT8_T

instance Read UINT8_T

instance Show UINT8_T

instance BG.Bitfield UINT8_T

instance BG.Bits UINT8_T

instance Bounded UINT8_T

instance Enum UINT8_T

instance BG.FiniteBits UINT8_T

instance BG.HasFFIType UINT8_T

instance Integral UINT8_T

instance BG.Ix UINT8_T

instance Num UINT8_T

instance BG.Prim UINT8_T

instance Marshal.ReadRaw UINT8_T

instance Real UINT8_T

instance Marshal.StaticSize UINT8_T

instance BG.Storable UINT8_T

instance Marshal.WriteRaw UINT8_T

data BOOLEAN_T

instance Eq BOOLEAN_T

instance BG.Generic BOOLEAN_T

instance Ord BOOLEAN_T

instance Read BOOLEAN_T

instance Show BOOLEAN_T

instance BG.Bitfield BOOLEAN_T

instance BG.Bits BOOLEAN_T

instance Bounded BOOLEAN_T

instance Enum BOOLEAN_T

instance BG.FiniteBits BOOLEAN_T

instance BG.HasFFIType BOOLEAN_T

instance Integral BOOLEAN_T

instance BG.Ix BOOLEAN_T

instance Num BOOLEAN_T

instance BG.Prim BOOLEAN_T

instance Marshal.ReadRaw BOOLEAN_T

instance Real BOOLEAN_T

instance Marshal.StaticSize BOOLEAN_T

instance BG.Storable BOOLEAN_T

instance Marshal.WriteRaw BOOLEAN_T

data Boolean_T

instance Eq Boolean_T

instance BG.Generic Boolean_T

instance Ord Boolean_T

instance Read Boolean_T

instance Show Boolean_T

instance BG.Bitfield Boolean_T

instance BG.Bits Boolean_T

instance Bounded Boolean_T

instance Enum Boolean_T

instance BG.FiniteBits Boolean_T

instance BG.HasFFIType Boolean_T

instance Integral Boolean_T

instance BG.Ix Boolean_T

instance Num Boolean_T

instance BG.Prim Boolean_T

instance Marshal.ReadRaw Boolean_T

instance Real Boolean_T

instance Marshal.StaticSize Boolean_T

instance BG.Storable Boolean_T

instance Marshal.WriteRaw Boolean_T
