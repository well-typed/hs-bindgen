{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Eq, Show)

data Has_implicit_fields_anon'x2_1

instance Eq Has_implicit_fields_anon'x2_1

instance BG.Generic Has_implicit_fields_anon'x2_1

instance Show Has_implicit_fields_anon'x2_1

instance Marshal.StaticSize Has_implicit_fields_anon'x2_1

instance Marshal.ReadRaw Has_implicit_fields_anon'x2_1

instance Marshal.WriteRaw Has_implicit_fields_anon'x2_1

instance BG.Storable Has_implicit_fields_anon'x2_1

instance Struct.IsStruct Has_implicit_fields_anon'x2_1

data Has_implicit_fields_anon'x4_1

instance Eq Has_implicit_fields_anon'x4_1

instance BG.Generic Has_implicit_fields_anon'x4_1

instance Show Has_implicit_fields_anon'x4_1

instance Marshal.StaticSize Has_implicit_fields_anon'x4_1

instance Marshal.ReadRaw Has_implicit_fields_anon'x4_1

instance Marshal.WriteRaw Has_implicit_fields_anon'x4_1

instance BG.Storable Has_implicit_fields_anon'x4_1

instance Struct.IsStruct Has_implicit_fields_anon'x4_1

data Has_implicit_fields_anon'x5_1

instance BG.Generic Has_implicit_fields_anon'x5_1

instance Marshal.StaticSize Has_implicit_fields_anon'x5_1

instance Marshal.ReadRaw Has_implicit_fields_anon'x5_1

instance Marshal.WriteRaw Has_implicit_fields_anon'x5_1

instance BG.Storable Has_implicit_fields_anon'x5_1

instance Union.IsUnion Has_implicit_fields_anon'x5_1

data Has_implicit_fields

instance BG.Generic Has_implicit_fields

instance Marshal.StaticSize Has_implicit_fields

instance Marshal.ReadRaw Has_implicit_fields

instance Marshal.WriteRaw Has_implicit_fields

instance BG.Storable Has_implicit_fields

instance Struct.IsStruct Has_implicit_fields
