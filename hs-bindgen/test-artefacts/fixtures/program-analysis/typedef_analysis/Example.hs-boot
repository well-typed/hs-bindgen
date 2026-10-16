{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

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

data Struct3_t

data Struct4_t

data Struct5

instance Eq Struct5

instance BG.Generic Struct5

instance Show Struct5

instance Marshal.StaticSize Struct5

instance Marshal.ReadRaw Struct5

instance Marshal.WriteRaw Struct5

instance BG.Storable Struct5

instance Struct.IsStruct Struct5

data Struct5_t

instance Eq Struct5_t

instance BG.Generic Struct5_t

instance Ord Struct5_t

instance Show Struct5_t

instance BG.HasFFIType Struct5_t

instance Marshal.ReadRaw Struct5_t

instance Marshal.StaticSize Struct5_t

instance BG.Storable Struct5_t

instance Marshal.WriteRaw Struct5_t

data Struct6a_struct

instance Eq Struct6a_struct

instance BG.Generic Struct6a_struct

instance Show Struct6a_struct

instance Marshal.StaticSize Struct6a_struct

instance Marshal.ReadRaw Struct6a_struct

instance Marshal.WriteRaw Struct6a_struct

instance BG.Storable Struct6a_struct

instance Struct.IsStruct Struct6a_struct

data Struct6a

instance Eq Struct6a

instance BG.Generic Struct6a

instance Ord Struct6a

instance Show Struct6a

instance BG.HasFFIType Struct6a

instance Marshal.ReadRaw Struct6a

instance Marshal.StaticSize Struct6a

instance BG.Storable Struct6a

instance Marshal.WriteRaw Struct6a

data Struct6b_struct

instance Eq Struct6b_struct

instance BG.Generic Struct6b_struct

instance Show Struct6b_struct

instance Marshal.StaticSize Struct6b_struct

instance Marshal.ReadRaw Struct6b_struct

instance Marshal.WriteRaw Struct6b_struct

instance BG.Storable Struct6b_struct

instance Struct.IsStruct Struct6b_struct

data Struct6b

instance Eq Struct6b

instance BG.Generic Struct6b

instance Show Struct6b

instance IsA.IsArray Struct6b

instance Marshal.ReadRaw Struct6b

instance Marshal.StaticSize Struct6b

instance BG.Storable Struct6b

instance Marshal.WriteRaw Struct6b

data Struct7

instance Eq Struct7

instance BG.Generic Struct7

instance Show Struct7

instance Marshal.StaticSize Struct7

instance Marshal.ReadRaw Struct7

instance Marshal.WriteRaw Struct7

instance BG.Storable Struct7

instance Struct.IsStruct Struct7

data Struct7a

instance Eq Struct7a

instance BG.Generic Struct7a

instance Show Struct7a

instance Marshal.ReadRaw Struct7a

instance Marshal.StaticSize Struct7a

instance BG.Storable Struct7a

instance Marshal.WriteRaw Struct7a

data Struct7b

instance Eq Struct7b

instance BG.Generic Struct7b

instance Show Struct7b

instance Marshal.ReadRaw Struct7b

instance Marshal.StaticSize Struct7b

instance BG.Storable Struct7b

instance Marshal.WriteRaw Struct7b

data Struct8

instance Eq Struct8

instance BG.Generic Struct8

instance Show Struct8

instance Marshal.StaticSize Struct8

instance Marshal.ReadRaw Struct8

instance Marshal.WriteRaw Struct8

instance BG.Storable Struct8

instance Struct.IsStruct Struct8

data Struct8b

instance Eq Struct8b

instance BG.Generic Struct8b

instance Show Struct8b

instance Marshal.ReadRaw Struct8b

instance Marshal.StaticSize Struct8b

instance BG.Storable Struct8b

instance Marshal.WriteRaw Struct8b

data Struct9

instance Eq Struct9

instance BG.Generic Struct9

instance Show Struct9

instance Marshal.StaticSize Struct9

instance Marshal.ReadRaw Struct9

instance Marshal.WriteRaw Struct9

instance BG.Storable Struct9

instance Struct.IsStruct Struct9

data Struct9_t

instance Eq Struct9_t

instance BG.Generic Struct9_t

instance Show Struct9_t

instance Marshal.ReadRaw Struct9_t

instance Marshal.StaticSize Struct9_t

instance BG.Storable Struct9_t

instance Marshal.WriteRaw Struct9_t

data Struct10_t

instance Eq Struct10_t

instance BG.Generic Struct10_t

instance Show Struct10_t

instance Marshal.StaticSize Struct10_t

instance Marshal.ReadRaw Struct10_t

instance Marshal.WriteRaw Struct10_t

instance BG.Storable Struct10_t

instance Struct.IsStruct Struct10_t

data Struct10_t_t

instance Eq Struct10_t_t

instance BG.Generic Struct10_t_t

instance Show Struct10_t_t

instance Marshal.ReadRaw Struct10_t_t

instance Marshal.StaticSize Struct10_t_t

instance BG.Storable Struct10_t_t

instance Marshal.WriteRaw Struct10_t_t

data Struct11_t

instance Eq Struct11_t

instance BG.Generic Struct11_t

instance Show Struct11_t

instance Marshal.StaticSize Struct11_t

instance Marshal.ReadRaw Struct11_t

instance Marshal.WriteRaw Struct11_t

instance BG.Storable Struct11_t

instance Struct.IsStruct Struct11_t

data Struct12_t

instance Eq Struct12_t

instance BG.Generic Struct12_t

instance Show Struct12_t

instance Marshal.StaticSize Struct12_t

instance Marshal.ReadRaw Struct12_t

instance Marshal.WriteRaw Struct12_t

instance BG.Storable Struct12_t

instance Struct.IsStruct Struct12_t

data Use_sites

instance Eq Use_sites

instance BG.Generic Use_sites

instance Show Use_sites

instance Marshal.StaticSize Use_sites

instance Marshal.ReadRaw Use_sites

instance Marshal.WriteRaw Use_sites

instance BG.Storable Use_sites

instance Struct.IsStruct Use_sites

data Foo_struct

instance Eq Foo_struct

instance BG.Generic Foo_struct

instance Show Foo_struct

instance Marshal.StaticSize Foo_struct

instance Marshal.ReadRaw Foo_struct

instance Marshal.WriteRaw Foo_struct

instance BG.Storable Foo_struct

instance Struct.IsStruct Foo_struct

data Foo_Aux

instance BG.Generic Foo_Aux

data Foo

instance Eq Foo

instance BG.Generic Foo

instance Ord Foo

instance Show Foo

instance BG.HasFFIType Foo

instance Marshal.ReadRaw Foo

instance Marshal.StaticSize Foo

instance BG.Storable Foo

instance Marshal.WriteRaw Foo

data Bar_struct

instance Eq Bar_struct

instance BG.Generic Bar_struct

instance Show Bar_struct

instance Marshal.StaticSize Bar_struct

instance Marshal.ReadRaw Bar_struct

instance Marshal.WriteRaw Bar_struct

instance BG.Storable Bar_struct

instance Struct.IsStruct Bar_struct

data Bar_Aux

instance BG.Generic Bar_Aux

data Bar

instance Eq Bar

instance BG.Generic Bar

instance Ord Bar

instance Show Bar

instance BG.HasFFIType Bar

instance Marshal.ReadRaw Bar

instance Marshal.StaticSize Bar

instance BG.Storable Bar

instance Marshal.WriteRaw Bar

data Struct15

instance Eq Struct15

instance BG.Generic Struct15

instance Show Struct15

instance Marshal.StaticSize Struct15

instance Marshal.ReadRaw Struct15

instance Marshal.WriteRaw Struct15

instance BG.Storable Struct15

instance Struct.IsStruct Struct15

data Struct16_struct

instance Eq Struct16_struct

instance BG.Generic Struct16_struct

instance Show Struct16_struct

instance Marshal.StaticSize Struct16_struct

instance Marshal.ReadRaw Struct16_struct

instance Marshal.WriteRaw Struct16_struct

instance BG.Storable Struct16_struct

instance Struct.IsStruct Struct16_struct

data Struct16

instance Eq Struct16

instance BG.Generic Struct16

instance Ord Struct16

instance Show Struct16

instance BG.HasFFIType Struct16

instance Marshal.ReadRaw Struct16

instance Marshal.StaticSize Struct16

instance BG.Storable Struct16

instance Marshal.WriteRaw Struct16

data Use_sites_qual

instance Eq Use_sites_qual

instance BG.Generic Use_sites_qual

instance Show Use_sites_qual

instance Marshal.StaticSize Use_sites_qual

instance Marshal.ReadRaw Use_sites_qual

instance Marshal.WriteRaw Use_sites_qual

instance BG.Storable Use_sites_qual

instance Struct.IsStruct Use_sites_qual
