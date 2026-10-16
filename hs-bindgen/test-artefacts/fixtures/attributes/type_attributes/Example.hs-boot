{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data S

instance Eq S

instance BG.Generic S

instance Show S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S

data More_aligned_int

instance Eq More_aligned_int

instance BG.Generic More_aligned_int

instance Ord More_aligned_int

instance Read More_aligned_int

instance Show More_aligned_int

instance BG.Bitfield More_aligned_int

instance BG.Bits More_aligned_int

instance Bounded More_aligned_int

instance Enum More_aligned_int

instance BG.FiniteBits More_aligned_int

instance BG.HasFFIType More_aligned_int

instance Integral More_aligned_int

instance BG.Ix More_aligned_int

instance Num More_aligned_int

instance BG.Prim More_aligned_int

instance Marshal.ReadRaw More_aligned_int

instance Real More_aligned_int

instance Marshal.StaticSize More_aligned_int

instance BG.Storable More_aligned_int

instance Marshal.WriteRaw More_aligned_int

data S2

instance Eq S2

instance BG.Generic S2

instance Show S2

instance Marshal.StaticSize S2

instance Marshal.ReadRaw S2

instance Marshal.WriteRaw S2

instance BG.Storable S2

instance Struct.IsStruct S2

data My_unpacked_struct

instance Eq My_unpacked_struct

instance BG.Generic My_unpacked_struct

instance Show My_unpacked_struct

instance Marshal.StaticSize My_unpacked_struct

instance Marshal.ReadRaw My_unpacked_struct

instance Marshal.WriteRaw My_unpacked_struct

instance BG.Storable My_unpacked_struct

instance Struct.IsStruct My_unpacked_struct

data My_packed_struct

instance Eq My_packed_struct

instance BG.Generic My_packed_struct

instance Show My_packed_struct

instance Marshal.StaticSize My_packed_struct

instance Marshal.ReadRaw My_packed_struct

instance Marshal.WriteRaw My_packed_struct

instance BG.Storable My_packed_struct

instance Struct.IsStruct My_packed_struct

data Wait_status_ptr_t

instance BG.Generic Wait_status_ptr_t

instance Marshal.StaticSize Wait_status_ptr_t

instance Marshal.ReadRaw Wait_status_ptr_t

instance Marshal.WriteRaw Wait_status_ptr_t

instance BG.Storable Wait_status_ptr_t

instance Union.IsUnion Wait_status_ptr_t

data Wait

data T1

instance Eq T1

instance BG.Generic T1

instance Ord T1

instance Read T1

instance Show T1

instance BG.Bitfield T1

instance BG.Bits T1

instance Bounded T1

instance Enum T1

instance BG.FiniteBits T1

instance BG.HasFFIType T1

instance Integral T1

instance BG.Ix T1

instance Num T1

instance BG.Prim T1

instance Marshal.ReadRaw T1

instance Real T1

instance Marshal.StaticSize T1

instance BG.Storable T1

instance Marshal.WriteRaw T1

data Short_a

instance Eq Short_a

instance BG.Generic Short_a

instance Ord Short_a

instance Read Short_a

instance Show Short_a

instance BG.Bitfield Short_a

instance BG.Bits Short_a

instance Bounded Short_a

instance Enum Short_a

instance BG.FiniteBits Short_a

instance BG.HasFFIType Short_a

instance Integral Short_a

instance BG.Ix Short_a

instance Num Short_a

instance BG.Prim Short_a

instance Marshal.ReadRaw Short_a

instance Real Short_a

instance Marshal.StaticSize Short_a

instance BG.Storable Short_a

instance Marshal.WriteRaw Short_a
