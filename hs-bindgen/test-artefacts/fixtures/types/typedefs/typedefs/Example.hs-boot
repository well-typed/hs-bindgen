{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Myint

instance Eq Myint

instance BG.Generic Myint

instance Ord Myint

instance Read Myint

instance Show Myint

instance BG.Bitfield Myint

instance BG.Bits Myint

instance Bounded Myint

instance Enum Myint

instance BG.FiniteBits Myint

instance BG.HasFFIType Myint

instance Integral Myint

instance BG.Ix Myint

instance Num Myint

instance BG.Prim Myint

instance Marshal.ReadRaw Myint

instance Real Myint

instance Marshal.StaticSize Myint

instance BG.Storable Myint

instance Marshal.WriteRaw Myint

data Intptr

instance Eq Intptr

instance BG.Generic Intptr

instance Ord Intptr

instance Show Intptr

instance BG.HasFFIType Intptr

instance Marshal.ReadRaw Intptr

instance Marshal.StaticSize Intptr

instance BG.Storable Intptr

instance Marshal.WriteRaw Intptr

data Int2int

instance BG.Generic Int2int

instance BG.ToFunPtr Int2int

instance BG.FromFunPtr Int2int

data FunctionPointer_Function_Aux

instance BG.Generic FunctionPointer_Function_Aux

instance BG.ToFunPtr FunctionPointer_Function_Aux

instance BG.FromFunPtr FunctionPointer_Function_Aux

data FunctionPointer_Function

instance Eq FunctionPointer_Function

instance BG.Generic FunctionPointer_Function

instance Ord FunctionPointer_Function

instance Show FunctionPointer_Function

instance BG.HasFFIType FunctionPointer_Function

instance Marshal.ReadRaw FunctionPointer_Function

instance Marshal.StaticSize FunctionPointer_Function

instance BG.Storable FunctionPointer_Function

instance Marshal.WriteRaw FunctionPointer_Function

data NonFunctionPointer_Function

instance BG.Generic NonFunctionPointer_Function

instance BG.ToFunPtr NonFunctionPointer_Function

instance BG.FromFunPtr NonFunctionPointer_Function

data F1_Aux

instance BG.Generic F1_Aux

instance BG.ToFunPtr F1_Aux

instance BG.FromFunPtr F1_Aux

data F1

instance Eq F1

instance BG.Generic F1

instance Ord F1

instance Show F1

instance BG.HasFFIType F1

instance Marshal.ReadRaw F1

instance Marshal.StaticSize F1

instance BG.Storable F1

instance Marshal.WriteRaw F1

data G1

instance BG.Generic G1

instance BG.ToFunPtr G1

instance BG.FromFunPtr G1

data G2

instance Eq G2

instance BG.Generic G2

instance Ord G2

instance Show G2

instance BG.HasFFIType G2

instance Marshal.ReadRaw G2

instance Marshal.StaticSize G2

instance BG.Storable G2

instance Marshal.WriteRaw G2

data H1

instance BG.Generic H1

instance BG.ToFunPtr H1

instance BG.FromFunPtr H1

data H2

instance BG.Generic H2

data H3

instance Eq H3

instance BG.Generic H3

instance Ord H3

instance Show H3

instance BG.HasFFIType H3

instance Marshal.ReadRaw H3

instance Marshal.StaticSize H3

instance BG.Storable H3

instance Marshal.WriteRaw H3
