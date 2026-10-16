{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data A

instance Eq A

instance BG.Generic A

instance Ord A

instance Read A

instance Show A

instance BG.Bitfield A

instance BG.Bits A

instance Bounded A

instance Enum A

instance BG.FiniteBits A

instance BG.HasFFIType A

instance Integral A

instance BG.Ix A

instance Num A

instance BG.Prim A

instance Marshal.ReadRaw A

instance Real A

instance Marshal.StaticSize A

instance BG.Storable A

instance Marshal.WriteRaw A

data Some_struct

instance Eq Some_struct

instance BG.Generic Some_struct

instance Show Some_struct

instance Marshal.StaticSize Some_struct

instance Marshal.ReadRaw Some_struct

instance Marshal.WriteRaw Some_struct

instance BG.Storable Some_struct

instance Struct.IsStruct Some_struct

data Some_union

instance BG.Generic Some_union

instance Marshal.StaticSize Some_union

instance Marshal.ReadRaw Some_union

instance Marshal.WriteRaw Some_union

instance BG.Storable Some_union

instance Union.IsUnion Some_union

data Some_enum

instance Eq Some_enum

instance BG.Generic Some_enum

instance Ord Some_enum

instance BG.HasFFIType Some_enum

instance Marshal.StaticSize Some_enum

instance Marshal.ReadRaw Some_enum

instance Marshal.WriteRaw Some_enum

instance BG.Storable Some_enum

instance BG.Prim Some_enum

instance CEnum.CEnum Some_enum

instance CEnum.SequentialCEnum Some_enum

instance Show Some_enum

instance Read Some_enum

data Arr_typedef1

instance Eq Arr_typedef1

instance BG.Generic Arr_typedef1

instance Show Arr_typedef1

instance IsA.IsArray Arr_typedef1

data Arr_typedef2

instance Eq Arr_typedef2

instance BG.Generic Arr_typedef2

instance Show Arr_typedef2

instance IsA.IsArray Arr_typedef2

data Arr_typedef3

instance Eq Arr_typedef3

instance BG.Generic Arr_typedef3

instance Show Arr_typedef3

instance IsA.IsArray Arr_typedef3

instance Marshal.ReadRaw Arr_typedef3

instance Marshal.StaticSize Arr_typedef3

instance BG.Storable Arr_typedef3

instance Marshal.WriteRaw Arr_typedef3

data Arr_typedef4

instance Eq Arr_typedef4

instance BG.Generic Arr_typedef4

instance Show Arr_typedef4

instance IsA.IsArray Arr_typedef4

instance Marshal.ReadRaw Arr_typedef4

instance Marshal.StaticSize Arr_typedef4

instance BG.Storable Arr_typedef4

instance Marshal.WriteRaw Arr_typedef4

data Typedef1

instance Eq Typedef1

instance BG.Generic Typedef1

instance Ord Typedef1

instance Read Typedef1

instance Show Typedef1

instance BG.Bitfield Typedef1

instance BG.Bits Typedef1

instance Bounded Typedef1

instance Enum Typedef1

instance BG.FiniteBits Typedef1

instance BG.HasFFIType Typedef1

instance Integral Typedef1

instance BG.Ix Typedef1

instance Num Typedef1

instance BG.Prim Typedef1

instance Marshal.ReadRaw Typedef1

instance Real Typedef1

instance Marshal.StaticSize Typedef1

instance BG.Storable Typedef1

instance Marshal.WriteRaw Typedef1

data Typedef2

instance Eq Typedef2

instance BG.Generic Typedef2

instance Ord Typedef2

instance Show Typedef2

instance BG.HasFFIType Typedef2

instance Marshal.ReadRaw Typedef2

instance Marshal.StaticSize Typedef2

instance BG.Storable Typedef2

instance Marshal.WriteRaw Typedef2

data Typedef3

instance Eq Typedef3

instance BG.Generic Typedef3

instance Ord Typedef3

instance Show Typedef3

instance BG.HasFFIType Typedef3

instance Marshal.ReadRaw Typedef3

instance Marshal.StaticSize Typedef3

instance BG.Storable Typedef3

instance Marshal.WriteRaw Typedef3

data Funptr_typedef1_Aux

instance BG.Generic Funptr_typedef1_Aux

instance BG.ToFunPtr Funptr_typedef1_Aux

instance BG.FromFunPtr Funptr_typedef1_Aux

data Funptr_typedef1

instance Eq Funptr_typedef1

instance BG.Generic Funptr_typedef1

instance Ord Funptr_typedef1

instance Show Funptr_typedef1

instance BG.HasFFIType Funptr_typedef1

instance Marshal.ReadRaw Funptr_typedef1

instance Marshal.StaticSize Funptr_typedef1

instance BG.Storable Funptr_typedef1

instance Marshal.WriteRaw Funptr_typedef1

data Funptr_typedef2_Aux

instance BG.Generic Funptr_typedef2_Aux

instance BG.ToFunPtr Funptr_typedef2_Aux

instance BG.FromFunPtr Funptr_typedef2_Aux

data Funptr_typedef2

instance Eq Funptr_typedef2

instance BG.Generic Funptr_typedef2

instance Ord Funptr_typedef2

instance Show Funptr_typedef2

instance BG.HasFFIType Funptr_typedef2

instance Marshal.ReadRaw Funptr_typedef2

instance Marshal.StaticSize Funptr_typedef2

instance BG.Storable Funptr_typedef2

instance Marshal.WriteRaw Funptr_typedef2

data Funptr_typedef3_Aux

instance BG.Generic Funptr_typedef3_Aux

instance BG.ToFunPtr Funptr_typedef3_Aux

instance BG.FromFunPtr Funptr_typedef3_Aux

data Funptr_typedef3

instance Eq Funptr_typedef3

instance BG.Generic Funptr_typedef3

instance Ord Funptr_typedef3

instance Show Funptr_typedef3

instance BG.HasFFIType Funptr_typedef3

instance Marshal.ReadRaw Funptr_typedef3

instance Marshal.StaticSize Funptr_typedef3

instance BG.Storable Funptr_typedef3

instance Marshal.WriteRaw Funptr_typedef3

data Funptr_typedef4_Aux

instance BG.Generic Funptr_typedef4_Aux

instance BG.ToFunPtr Funptr_typedef4_Aux

instance BG.FromFunPtr Funptr_typedef4_Aux

data Funptr_typedef4

instance Eq Funptr_typedef4

instance BG.Generic Funptr_typedef4

instance Ord Funptr_typedef4

instance Show Funptr_typedef4

instance BG.HasFFIType Funptr_typedef4

instance Marshal.ReadRaw Funptr_typedef4

instance Marshal.StaticSize Funptr_typedef4

instance BG.Storable Funptr_typedef4

instance Marshal.WriteRaw Funptr_typedef4

data Funptr_typedef5_Aux

instance BG.Generic Funptr_typedef5_Aux

instance BG.ToFunPtr Funptr_typedef5_Aux

instance BG.FromFunPtr Funptr_typedef5_Aux

data Funptr_typedef5

instance Eq Funptr_typedef5

instance BG.Generic Funptr_typedef5

instance Ord Funptr_typedef5

instance Show Funptr_typedef5

instance BG.HasFFIType Funptr_typedef5

instance Marshal.ReadRaw Funptr_typedef5

instance Marshal.StaticSize Funptr_typedef5

instance BG.Storable Funptr_typedef5

instance Marshal.WriteRaw Funptr_typedef5

data Comments2

instance Eq Comments2

instance BG.Generic Comments2

instance Ord Comments2

instance Read Comments2

instance Show Comments2

instance BG.Bitfield Comments2

instance BG.Bits Comments2

instance Bounded Comments2

instance Enum Comments2

instance BG.FiniteBits Comments2

instance BG.HasFFIType Comments2

instance Integral Comments2

instance BG.Ix Comments2

instance Num Comments2

instance BG.Prim Comments2

instance Marshal.ReadRaw Comments2

instance Real Comments2

instance Marshal.StaticSize Comments2

instance BG.Storable Comments2

instance Marshal.WriteRaw Comments2

data Example_struct

instance Eq Example_struct

instance BG.Generic Example_struct

instance Show Example_struct

instance Marshal.StaticSize Example_struct

instance Marshal.ReadRaw Example_struct

instance Marshal.WriteRaw Example_struct

instance BG.Storable Example_struct

instance Struct.IsStruct Example_struct

data Const_typedef1

instance Eq Const_typedef1

instance BG.Generic Const_typedef1

instance Ord Const_typedef1

instance Read Const_typedef1

instance Show Const_typedef1

instance BG.Bitfield Const_typedef1

instance BG.Bits Const_typedef1

instance Bounded Const_typedef1

instance Enum Const_typedef1

instance BG.FiniteBits Const_typedef1

instance BG.HasFFIType Const_typedef1

instance Integral Const_typedef1

instance BG.Ix Const_typedef1

instance Num Const_typedef1

instance BG.Prim Const_typedef1

instance Marshal.ReadRaw Const_typedef1

instance Real Const_typedef1

instance Marshal.StaticSize Const_typedef1

instance BG.Storable Const_typedef1

instance Marshal.WriteRaw Const_typedef1

data Const_typedef2

instance Eq Const_typedef2

instance BG.Generic Const_typedef2

instance Ord Const_typedef2

instance Read Const_typedef2

instance Show Const_typedef2

instance BG.Bitfield Const_typedef2

instance BG.Bits Const_typedef2

instance Bounded Const_typedef2

instance Enum Const_typedef2

instance BG.FiniteBits Const_typedef2

instance BG.HasFFIType Const_typedef2

instance Integral Const_typedef2

instance BG.Ix Const_typedef2

instance Num Const_typedef2

instance BG.Prim Const_typedef2

instance Marshal.ReadRaw Const_typedef2

instance Real Const_typedef2

instance Marshal.StaticSize Const_typedef2

instance BG.Storable Const_typedef2

instance Marshal.WriteRaw Const_typedef2

data Const_typedef3

instance Eq Const_typedef3

instance BG.Generic Const_typedef3

instance Ord Const_typedef3

instance Show Const_typedef3

instance BG.HasFFIType Const_typedef3

instance Marshal.ReadRaw Const_typedef3

instance Marshal.StaticSize Const_typedef3

instance BG.Storable Const_typedef3

instance Marshal.WriteRaw Const_typedef3

data Const_typedef4

instance Eq Const_typedef4

instance BG.Generic Const_typedef4

instance Ord Const_typedef4

instance Show Const_typedef4

instance BG.HasFFIType Const_typedef4

instance Marshal.ReadRaw Const_typedef4

instance Marshal.StaticSize Const_typedef4

instance BG.Storable Const_typedef4

instance Marshal.WriteRaw Const_typedef4

data Const_typedef5

instance Eq Const_typedef5

instance BG.Generic Const_typedef5

instance Ord Const_typedef5

instance Show Const_typedef5

instance BG.HasFFIType Const_typedef5

instance Marshal.ReadRaw Const_typedef5

instance Marshal.StaticSize Const_typedef5

instance BG.Storable Const_typedef5

instance Marshal.WriteRaw Const_typedef5

data Const_typedef6

instance Eq Const_typedef6

instance BG.Generic Const_typedef6

instance Ord Const_typedef6

instance Show Const_typedef6

instance BG.HasFFIType Const_typedef6

instance Marshal.ReadRaw Const_typedef6

instance Marshal.StaticSize Const_typedef6

instance BG.Storable Const_typedef6

instance Marshal.WriteRaw Const_typedef6

data Const_typedef7

instance Eq Const_typedef7

instance BG.Generic Const_typedef7

instance Ord Const_typedef7

instance Show Const_typedef7

instance BG.HasFFIType Const_typedef7

instance Marshal.ReadRaw Const_typedef7

instance Marshal.StaticSize Const_typedef7

instance BG.Storable Const_typedef7

instance Marshal.WriteRaw Const_typedef7

data Example_struct_with_const

instance Eq Example_struct_with_const

instance BG.Generic Example_struct_with_const

instance Show Example_struct_with_const

instance Marshal.StaticSize Example_struct_with_const

instance Marshal.ReadRaw Example_struct_with_const

instance Marshal.WriteRaw Example_struct_with_const

instance BG.Storable Example_struct_with_const

instance Struct.IsStruct Example_struct_with_const

data Const_funptr1_Aux

instance BG.Generic Const_funptr1_Aux

instance BG.ToFunPtr Const_funptr1_Aux

instance BG.FromFunPtr Const_funptr1_Aux

data Const_funptr1

instance Eq Const_funptr1

instance BG.Generic Const_funptr1

instance Ord Const_funptr1

instance Show Const_funptr1

instance BG.HasFFIType Const_funptr1

instance Marshal.ReadRaw Const_funptr1

instance Marshal.StaticSize Const_funptr1

instance BG.Storable Const_funptr1

instance Marshal.WriteRaw Const_funptr1

data Const_funptr2_Aux

instance BG.Generic Const_funptr2_Aux

instance BG.ToFunPtr Const_funptr2_Aux

instance BG.FromFunPtr Const_funptr2_Aux

data Const_funptr2

instance Eq Const_funptr2

instance BG.Generic Const_funptr2

instance Ord Const_funptr2

instance Show Const_funptr2

instance BG.HasFFIType Const_funptr2

instance Marshal.ReadRaw Const_funptr2

instance Marshal.StaticSize Const_funptr2

instance BG.Storable Const_funptr2

instance Marshal.WriteRaw Const_funptr2

data Const_funptr3_Aux

instance BG.Generic Const_funptr3_Aux

instance BG.ToFunPtr Const_funptr3_Aux

instance BG.FromFunPtr Const_funptr3_Aux

data Const_funptr3

instance Eq Const_funptr3

instance BG.Generic Const_funptr3

instance Ord Const_funptr3

instance Show Const_funptr3

instance BG.HasFFIType Const_funptr3

instance Marshal.ReadRaw Const_funptr3

instance Marshal.StaticSize Const_funptr3

instance BG.Storable Const_funptr3

instance Marshal.WriteRaw Const_funptr3

data Const_funptr4_Aux

instance BG.Generic Const_funptr4_Aux

instance BG.ToFunPtr Const_funptr4_Aux

instance BG.FromFunPtr Const_funptr4_Aux

data Const_funptr4

instance Eq Const_funptr4

instance BG.Generic Const_funptr4

instance Ord Const_funptr4

instance Show Const_funptr4

instance BG.HasFFIType Const_funptr4

instance Marshal.ReadRaw Const_funptr4

instance Marshal.StaticSize Const_funptr4

instance BG.Storable Const_funptr4

instance Marshal.WriteRaw Const_funptr4

data Const_funptr5_Aux

instance BG.Generic Const_funptr5_Aux

instance BG.ToFunPtr Const_funptr5_Aux

instance BG.FromFunPtr Const_funptr5_Aux

data Const_funptr5

instance Eq Const_funptr5

instance BG.Generic Const_funptr5

instance Ord Const_funptr5

instance Show Const_funptr5

instance BG.HasFFIType Const_funptr5

instance Marshal.ReadRaw Const_funptr5

instance Marshal.StaticSize Const_funptr5

instance BG.Storable Const_funptr5

instance Marshal.WriteRaw Const_funptr5

data Const_funptr6_Aux

instance BG.Generic Const_funptr6_Aux

instance BG.ToFunPtr Const_funptr6_Aux

instance BG.FromFunPtr Const_funptr6_Aux

data Const_funptr6

instance Eq Const_funptr6

instance BG.Generic Const_funptr6

instance Ord Const_funptr6

instance Show Const_funptr6

instance BG.HasFFIType Const_funptr6

instance Marshal.ReadRaw Const_funptr6

instance Marshal.StaticSize Const_funptr6

instance BG.Storable Const_funptr6

instance Marshal.WriteRaw Const_funptr6

data Const_funptr7_Aux

instance BG.Generic Const_funptr7_Aux

instance BG.ToFunPtr Const_funptr7_Aux

instance BG.FromFunPtr Const_funptr7_Aux

data Const_funptr7

instance Eq Const_funptr7

instance BG.Generic Const_funptr7

instance Ord Const_funptr7

instance Show Const_funptr7

instance BG.HasFFIType Const_funptr7

instance Marshal.ReadRaw Const_funptr7

instance Marshal.StaticSize Const_funptr7

instance BG.Storable Const_funptr7

instance Marshal.WriteRaw Const_funptr7

data BOOL

instance Eq BOOL

instance BG.Generic BOOL

instance Ord BOOL

instance Read BOOL

instance Show BOOL

instance BG.Bitfield BOOL

instance BG.Bits BOOL

instance Bounded BOOL

instance Enum BOOL

instance BG.FiniteBits BOOL

instance BG.HasFFIType BOOL

instance Integral BOOL

instance BG.Ix BOOL

instance Num BOOL

instance BG.Prim BOOL

instance Marshal.ReadRaw BOOL

instance Real BOOL

instance Marshal.StaticSize BOOL

instance BG.Storable BOOL

instance Marshal.WriteRaw BOOL

data INT

instance Eq INT

instance BG.Generic INT

instance Ord INT

instance Read INT

instance Show INT

instance BG.Bitfield INT

instance BG.Bits INT

instance Bounded INT

instance Enum INT

instance BG.FiniteBits INT

instance BG.HasFFIType INT

instance Integral INT

instance BG.Ix INT

instance Num INT

instance BG.Prim INT

instance Marshal.ReadRaw INT

instance Real INT

instance Marshal.StaticSize INT

instance BG.Storable INT

instance Marshal.WriteRaw INT

data INTP

instance Eq INTP

instance BG.Generic INTP

instance Ord INTP

instance Show INTP

instance BG.HasFFIType INTP

instance Marshal.ReadRaw INTP

instance Marshal.StaticSize INTP

instance BG.Storable INTP

instance Marshal.WriteRaw INTP

data INTCP

instance Eq INTCP

instance BG.Generic INTCP

instance Ord INTCP

instance Show INTCP

instance BG.HasFFIType INTCP

instance Marshal.ReadRaw INTCP

instance Marshal.StaticSize INTCP

instance BG.Storable INTCP

instance Marshal.WriteRaw INTCP
