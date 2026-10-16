{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Size_type

instance Eq Size_type

instance BG.Generic Size_type

instance Ord Size_type

instance Read Size_type

instance Show Size_type

instance BG.Bitfield Size_type

instance BG.Bits Size_type

instance Bounded Size_type

instance Enum Size_type

instance BG.FiniteBits Size_type

instance BG.HasFFIType Size_type

instance Integral Size_type

instance BG.Ix Size_type

instance Num Size_type

instance BG.Prim Size_type

instance Marshal.ReadRaw Size_type

instance Real Size_type

instance Marshal.StaticSize Size_type

instance BG.Storable Size_type

instance Marshal.WriteRaw Size_type

data Opaque_struct

data Opaque_union

data Color_enum

instance Eq Color_enum

instance BG.Generic Color_enum

instance Ord Color_enum

instance BG.HasFFIType Color_enum

instance Marshal.StaticSize Color_enum

instance Marshal.ReadRaw Color_enum

instance Marshal.WriteRaw Color_enum

instance BG.Storable Color_enum

instance BG.Prim Color_enum

instance CEnum.CEnum Color_enum

instance CEnum.SequentialCEnum Color_enum

instance Show Color_enum

instance Read Color_enum

data Event_callback_t_Aux

instance BG.Generic Event_callback_t_Aux

instance BG.ToFunPtr Event_callback_t_Aux

instance BG.FromFunPtr Event_callback_t_Aux

data Event_callback_t

instance Eq Event_callback_t

instance BG.Generic Event_callback_t

instance Ord Event_callback_t

instance Show Event_callback_t

instance BG.HasFFIType Event_callback_t

instance Marshal.ReadRaw Event_callback_t

instance Marshal.StaticSize Event_callback_t

instance BG.Storable Event_callback_t

instance Marshal.WriteRaw Event_callback_t

data Config_t

instance Eq Config_t

instance BG.Generic Config_t

instance Show Config_t

instance Marshal.StaticSize Config_t

instance Marshal.ReadRaw Config_t

instance Marshal.WriteRaw Config_t

instance BG.Storable Config_t

instance Struct.IsStruct Config_t

data Status_code_t

instance Eq Status_code_t

instance BG.Generic Status_code_t

instance Ord Status_code_t

instance BG.HasFFIType Status_code_t

instance Marshal.StaticSize Status_code_t

instance Marshal.ReadRaw Status_code_t

instance Marshal.WriteRaw Status_code_t

instance BG.Storable Status_code_t

instance BG.Prim Status_code_t

instance CEnum.CEnum Status_code_t

instance Show Status_code_t

instance Read Status_code_t

data Data_union_t_as_parts

instance Eq Data_union_t_as_parts

instance BG.Generic Data_union_t_as_parts

instance Show Data_union_t_as_parts

instance Marshal.StaticSize Data_union_t_as_parts

instance Marshal.ReadRaw Data_union_t_as_parts

instance Marshal.WriteRaw Data_union_t_as_parts

instance BG.Storable Data_union_t_as_parts

instance Struct.IsStruct Data_union_t_as_parts

data Data_union_t

instance BG.Generic Data_union_t

instance Marshal.StaticSize Data_union_t

instance Marshal.ReadRaw Data_union_t

instance Marshal.WriteRaw Data_union_t

instance BG.Storable Data_union_t

instance Union.IsUnion Data_union_t

data Bitfield_t

instance Eq Bitfield_t

instance BG.Generic Bitfield_t

instance Show Bitfield_t

instance Marshal.StaticSize Bitfield_t

instance Marshal.ReadRaw Bitfield_t

instance Marshal.WriteRaw Bitfield_t

instance BG.Storable Bitfield_t

instance Struct.IsStruct Bitfield_t

data Processor_fn_t_Aux

instance BG.Generic Processor_fn_t_Aux

instance BG.ToFunPtr Processor_fn_t_Aux

instance BG.FromFunPtr Processor_fn_t_Aux

data Processor_fn_t

instance Eq Processor_fn_t

instance BG.Generic Processor_fn_t

instance Ord Processor_fn_t

instance Show Processor_fn_t

instance BG.HasFFIType Processor_fn_t

instance Marshal.ReadRaw Processor_fn_t

instance Marshal.StaticSize Processor_fn_t

instance BG.Storable Processor_fn_t

instance Marshal.WriteRaw Processor_fn_t

data Filename_t

instance Eq Filename_t

instance BG.Generic Filename_t

instance Show Filename_t

instance IsA.IsArray Filename_t

instance Marshal.ReadRaw Filename_t

instance Marshal.StaticSize Filename_t

instance BG.Storable Filename_t

instance Marshal.WriteRaw Filename_t

data Flexible_array_Aux

instance Eq Flexible_array_Aux

instance BG.Generic Flexible_array_Aux

instance Show Flexible_array_Aux

instance Marshal.StaticSize Flexible_array_Aux

instance Marshal.ReadRaw Flexible_array_Aux

instance Marshal.WriteRaw Flexible_array_Aux

instance BG.Storable Flexible_array_Aux

instance Struct.IsStruct Flexible_array_Aux

type Flexible_array =
  FLAM.WithFlam BG.CInt Flexible_array_Aux

data Dyn_array_t

instance Eq Dyn_array_t

instance BG.Generic Dyn_array_t

instance Show Dyn_array_t

instance Marshal.StaticSize Dyn_array_t

instance Marshal.ReadRaw Dyn_array_t

instance Marshal.WriteRaw Dyn_array_t

instance BG.Storable Dyn_array_t

instance Struct.IsStruct Dyn_array_t

data Multi_untagged_t_pos

instance Eq Multi_untagged_t_pos

instance BG.Generic Multi_untagged_t_pos

instance Show Multi_untagged_t_pos

instance Marshal.StaticSize Multi_untagged_t_pos

instance Marshal.ReadRaw Multi_untagged_t_pos

instance Marshal.WriteRaw Multi_untagged_t_pos

instance BG.Storable Multi_untagged_t_pos

instance Struct.IsStruct Multi_untagged_t_pos

data Multi_untagged_t_dim

instance Eq Multi_untagged_t_dim

instance BG.Generic Multi_untagged_t_dim

instance Show Multi_untagged_t_dim

instance Marshal.StaticSize Multi_untagged_t_dim

instance Marshal.ReadRaw Multi_untagged_t_dim

instance Marshal.WriteRaw Multi_untagged_t_dim

instance BG.Storable Multi_untagged_t_dim

instance Struct.IsStruct Multi_untagged_t_dim

data Multi_untagged_t

instance Eq Multi_untagged_t

instance BG.Generic Multi_untagged_t

instance Show Multi_untagged_t

instance Marshal.StaticSize Multi_untagged_t

instance Marshal.ReadRaw Multi_untagged_t

instance Marshal.WriteRaw Multi_untagged_t

instance BG.Storable Multi_untagged_t

instance Struct.IsStruct Multi_untagged_t

data Named_inner

instance Eq Named_inner

instance BG.Generic Named_inner

instance Show Named_inner

instance Marshal.StaticSize Named_inner

instance Marshal.ReadRaw Named_inner

instance Marshal.WriteRaw Named_inner

instance BG.Storable Named_inner

instance Struct.IsStruct Named_inner

data Named_outer

instance Eq Named_outer

instance BG.Generic Named_outer

instance Show Named_outer

instance Marshal.StaticSize Named_outer

instance Marshal.ReadRaw Named_outer

instance Marshal.WriteRaw Named_outer

instance BG.Storable Named_outer

instance Struct.IsStruct Named_outer

data Deep_mid_untagged_field

instance Eq Deep_mid_untagged_field

instance BG.Generic Deep_mid_untagged_field

instance Show Deep_mid_untagged_field

instance Marshal.StaticSize Deep_mid_untagged_field

instance Marshal.ReadRaw Deep_mid_untagged_field

instance Marshal.WriteRaw Deep_mid_untagged_field

instance BG.Storable Deep_mid_untagged_field

instance Struct.IsStruct Deep_mid_untagged_field

data Deep_mid

instance Eq Deep_mid

instance BG.Generic Deep_mid

instance Show Deep_mid

instance Marshal.StaticSize Deep_mid

instance Marshal.ReadRaw Deep_mid

instance Marshal.WriteRaw Deep_mid

instance BG.Storable Deep_mid

instance Struct.IsStruct Deep_mid

data Deep_outer

instance Eq Deep_outer

instance BG.Generic Deep_outer

instance Show Deep_outer

instance Marshal.StaticSize Deep_outer

instance Marshal.ReadRaw Deep_outer

instance Marshal.WriteRaw Deep_outer

instance BG.Storable Deep_outer

instance Struct.IsStruct Deep_outer

data Unnamed_field_t_anon'ua

instance Eq Unnamed_field_t_anon'ua

instance BG.Generic Unnamed_field_t_anon'ua

instance Show Unnamed_field_t_anon'ua

instance Marshal.StaticSize Unnamed_field_t_anon'ua

instance Marshal.ReadRaw Unnamed_field_t_anon'ua

instance Marshal.WriteRaw Unnamed_field_t_anon'ua

instance BG.Storable Unnamed_field_t_anon'ua

instance Struct.IsStruct Unnamed_field_t_anon'ua

data Unnamed_field_t

instance Eq Unnamed_field_t

instance BG.Generic Unnamed_field_t

instance Show Unnamed_field_t

instance Marshal.StaticSize Unnamed_field_t

instance Marshal.ReadRaw Unnamed_field_t

instance Marshal.WriteRaw Unnamed_field_t

instance BG.Storable Unnamed_field_t

instance Struct.IsStruct Unnamed_field_t

data Api_version_t

instance Eq Api_version_t

instance BG.Generic Api_version_t

instance Ord Api_version_t

instance Read Api_version_t

instance Show Api_version_t

instance BG.Bitfield Api_version_t

instance BG.Bits Api_version_t

instance Bounded Api_version_t

instance Enum Api_version_t

instance BG.FiniteBits Api_version_t

instance BG.HasFFIType Api_version_t

instance Integral Api_version_t

instance BG.Ix Api_version_t

instance Num Api_version_t

instance BG.Prim Api_version_t

instance Marshal.ReadRaw Api_version_t

instance Real Api_version_t

instance Marshal.StaticSize Api_version_t

instance BG.Storable Api_version_t

instance Marshal.WriteRaw Api_version_t
