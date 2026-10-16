{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

data Fun_ptr_Aux

instance BG.Generic Fun_ptr_Aux

instance BG.ToFunPtr Fun_ptr_Aux

instance BG.FromFunPtr Fun_ptr_Aux

data Fun_ptr

instance Eq Fun_ptr

instance BG.Generic Fun_ptr

instance Ord Fun_ptr

instance Show Fun_ptr

instance BG.HasFFIType Fun_ptr

instance Marshal.ReadRaw Fun_ptr

instance Marshal.StaticSize Fun_ptr

instance BG.Storable Fun_ptr

instance Marshal.WriteRaw Fun_ptr

data Forward_declaration

instance Eq Forward_declaration

instance BG.Generic Forward_declaration

instance Show Forward_declaration

instance Marshal.StaticSize Forward_declaration

instance Marshal.ReadRaw Forward_declaration

instance Marshal.WriteRaw Forward_declaration

instance BG.Storable Forward_declaration

instance Struct.IsStruct Forward_declaration
