{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Another_typedef_struct_t

instance Eq Another_typedef_struct_t

instance BG.Generic Another_typedef_struct_t

instance Show Another_typedef_struct_t

instance Marshal.StaticSize Another_typedef_struct_t

instance Marshal.ReadRaw Another_typedef_struct_t

instance Marshal.WriteRaw Another_typedef_struct_t

instance BG.Storable Another_typedef_struct_t

instance Struct.IsStruct Another_typedef_struct_t

data Another_typedef_enum_e

instance Eq Another_typedef_enum_e

instance BG.Generic Another_typedef_enum_e

instance Ord Another_typedef_enum_e

instance BG.HasFFIType Another_typedef_enum_e

instance Marshal.StaticSize Another_typedef_enum_e

instance Marshal.ReadRaw Another_typedef_enum_e

instance Marshal.WriteRaw Another_typedef_enum_e

instance BG.Storable Another_typedef_enum_e

instance BG.Prim Another_typedef_enum_e

instance CEnum.CEnum Another_typedef_enum_e

instance CEnum.SequentialCEnum Another_typedef_enum_e

instance Show Another_typedef_enum_e

instance Read Another_typedef_enum_e

data A_type_t

instance Eq A_type_t

instance BG.Generic A_type_t

instance Ord A_type_t

instance Read A_type_t

instance Show A_type_t

instance BG.Bitfield A_type_t

instance BG.Bits A_type_t

instance Bounded A_type_t

instance Enum A_type_t

instance BG.FiniteBits A_type_t

instance BG.HasFFIType A_type_t

instance Integral A_type_t

instance BG.Ix A_type_t

instance Num A_type_t

instance BG.Prim A_type_t

instance Marshal.ReadRaw A_type_t

instance Real A_type_t

instance Marshal.StaticSize A_type_t

instance BG.Storable A_type_t

instance Marshal.WriteRaw A_type_t

data Var_t

instance Eq Var_t

instance BG.Generic Var_t

instance Ord Var_t

instance Read Var_t

instance Show Var_t

instance BG.Bitfield Var_t

instance BG.Bits Var_t

instance Bounded Var_t

instance Enum Var_t

instance BG.FiniteBits Var_t

instance BG.HasFFIType Var_t

instance Integral Var_t

instance BG.Ix Var_t

instance Num Var_t

instance BG.Prim Var_t

instance Marshal.ReadRaw Var_t

instance Real Var_t

instance Marshal.StaticSize Var_t

instance BG.Storable Var_t

instance Marshal.WriteRaw Var_t

data A_typedef_struct_t

instance Eq A_typedef_struct_t

instance BG.Generic A_typedef_struct_t

instance Show A_typedef_struct_t

instance Marshal.StaticSize A_typedef_struct_t

instance Marshal.ReadRaw A_typedef_struct_t

instance Marshal.WriteRaw A_typedef_struct_t

instance BG.Storable A_typedef_struct_t

instance Struct.IsStruct A_typedef_struct_t

data A_typedef_enum_e

instance Eq A_typedef_enum_e

instance BG.Generic A_typedef_enum_e

instance Ord A_typedef_enum_e

instance BG.HasFFIType A_typedef_enum_e

instance Marshal.StaticSize A_typedef_enum_e

instance Marshal.ReadRaw A_typedef_enum_e

instance Marshal.WriteRaw A_typedef_enum_e

instance BG.Storable A_typedef_enum_e

instance BG.Prim A_typedef_enum_e

instance CEnum.CEnum A_typedef_enum_e

instance CEnum.SequentialCEnum A_typedef_enum_e

instance Show A_typedef_enum_e

instance Read A_typedef_enum_e

data Callback_t_Aux

instance BG.Generic Callback_t_Aux

instance BG.ToFunPtr Callback_t_Aux

instance BG.FromFunPtr Callback_t_Aux

data Callback_t

instance Eq Callback_t

instance BG.Generic Callback_t

instance Ord Callback_t

instance Show Callback_t

instance BG.HasFFIType Callback_t

instance Marshal.ReadRaw Callback_t

instance Marshal.StaticSize Callback_t

instance BG.Storable Callback_t

instance Marshal.WriteRaw Callback_t
