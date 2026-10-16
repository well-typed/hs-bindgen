{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Config

instance Eq Config

instance BG.Generic Config

instance Show Config

instance Marshal.StaticSize Config

instance Marshal.ReadRaw Config

instance Marshal.WriteRaw Config

instance BG.Storable Config

instance Struct.IsStruct Config

data Inline_struct

instance Eq Inline_struct

instance BG.Generic Inline_struct

instance Show Inline_struct

instance Marshal.StaticSize Inline_struct

instance Marshal.ReadRaw Inline_struct

instance Marshal.WriteRaw Inline_struct

instance BG.Storable Inline_struct

instance Struct.IsStruct Inline_struct

data Version_t

instance Eq Version_t

instance BG.Generic Version_t

instance Show Version_t

instance Marshal.StaticSize Version_t

instance Marshal.ReadRaw Version_t

instance Marshal.WriteRaw Version_t

instance BG.Storable Version_t

instance Struct.IsStruct Version_t

data Struct1_t

instance Eq Struct1_t

instance BG.Generic Struct1_t

instance Show Struct1_t

instance Marshal.StaticSize Struct1_t

instance Marshal.ReadRaw Struct1_t

instance Marshal.WriteRaw Struct1_t

instance BG.Storable Struct1_t

instance Struct.IsStruct Struct1_t

data Struct2_t

instance Eq Struct2_t

instance BG.Generic Struct2_t

instance Show Struct2_t

instance Marshal.StaticSize Struct2_t

instance Marshal.ReadRaw Struct2_t

instance Marshal.WriteRaw Struct2_t

instance BG.Storable Struct2_t

instance Struct.IsStruct Struct2_t
