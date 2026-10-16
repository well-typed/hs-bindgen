{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

data PtrToVoid

instance Eq PtrToVoid

instance BG.Generic PtrToVoid

instance Ord PtrToVoid

instance Show PtrToVoid

instance BG.HasFFIType PtrToVoid

instance Marshal.ReadRaw PtrToVoid

instance Marshal.StaticSize PtrToVoid

instance BG.Storable PtrToVoid

instance Marshal.WriteRaw PtrToVoid

data PtrToConstVoidL

instance Eq PtrToConstVoidL

instance BG.Generic PtrToConstVoidL

instance Ord PtrToConstVoidL

instance Show PtrToConstVoidL

instance BG.HasFFIType PtrToConstVoidL

instance Marshal.ReadRaw PtrToConstVoidL

instance Marshal.StaticSize PtrToConstVoidL

instance BG.Storable PtrToConstVoidL

instance Marshal.WriteRaw PtrToConstVoidL

data PtrToConstVoidR

instance Eq PtrToConstVoidR

instance BG.Generic PtrToConstVoidR

instance Ord PtrToConstVoidR

instance Show PtrToConstVoidR

instance BG.HasFFIType PtrToConstVoidR

instance Marshal.ReadRaw PtrToConstVoidR

instance Marshal.StaticSize PtrToConstVoidR

instance BG.Storable PtrToConstVoidR

instance Marshal.WriteRaw PtrToConstVoidR

data PtrToConstIntL

instance Eq PtrToConstIntL

instance BG.Generic PtrToConstIntL

instance Ord PtrToConstIntL

instance Show PtrToConstIntL

instance BG.HasFFIType PtrToConstIntL

instance Marshal.ReadRaw PtrToConstIntL

instance Marshal.StaticSize PtrToConstIntL

instance BG.Storable PtrToConstIntL

instance Marshal.WriteRaw PtrToConstIntL

data PtrToConstIntR

instance Eq PtrToConstIntR

instance BG.Generic PtrToConstIntR

instance Ord PtrToConstIntR

instance Show PtrToConstIntR

instance BG.HasFFIType PtrToConstIntR

instance Marshal.ReadRaw PtrToConstIntR

instance Marshal.StaticSize PtrToConstIntR

instance BG.Storable PtrToConstIntR

instance Marshal.WriteRaw PtrToConstIntR

data ConstPtrToInt

instance Eq ConstPtrToInt

instance BG.Generic ConstPtrToInt

instance Ord ConstPtrToInt

instance Show ConstPtrToInt

instance BG.HasFFIType ConstPtrToInt

instance Marshal.ReadRaw ConstPtrToInt

instance Marshal.StaticSize ConstPtrToInt

instance BG.Storable ConstPtrToInt

instance Marshal.WriteRaw ConstPtrToInt
