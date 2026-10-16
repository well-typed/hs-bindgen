{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import HsBindgen.Runtime.Marshal qualified as Marshal
import HsBindgen.Runtime.Struct qualified as Struct
import HsBindgen.Runtime.Support qualified as BG
import Prelude (Eq, Ord, Show)

data S1

instance Eq S1

instance BG.Generic S1

instance Show S1

instance Marshal.StaticSize S1

instance Marshal.ReadRaw S1

instance Marshal.WriteRaw S1

instance BG.Storable S1

instance Struct.IsStruct S1

data S2_t

instance Eq S2_t

instance BG.Generic S2_t

instance Show S2_t

instance Marshal.StaticSize S2_t

instance Marshal.ReadRaw S2_t

instance Marshal.WriteRaw S2_t

instance BG.Storable S2_t

instance Struct.IsStruct S2_t

data S3_t

instance Eq S3_t

instance BG.Generic S3_t

instance Show S3_t

instance Marshal.StaticSize S3_t

instance Marshal.ReadRaw S3_t

instance Marshal.WriteRaw S3_t

instance BG.Storable S3_t

instance Struct.IsStruct S3_t

data S4

instance Eq S4

instance BG.Generic S4

instance Show S4

instance Marshal.StaticSize S4

instance Marshal.ReadRaw S4

instance Marshal.WriteRaw S4

instance BG.Storable S4

instance Struct.IsStruct S4

data S5

instance Eq S5

instance BG.Generic S5

instance Show S5

instance Marshal.StaticSize S5

instance Marshal.ReadRaw S5

instance Marshal.WriteRaw S5

instance BG.Storable S5

instance Struct.IsStruct S5

data S6

instance Eq S6

instance BG.Generic S6

instance Show S6

instance Marshal.StaticSize S6

instance Marshal.ReadRaw S6

instance Marshal.WriteRaw S6

instance BG.Storable S6

instance Struct.IsStruct S6

data S7a_Aux

instance Eq S7a_Aux

instance BG.Generic S7a_Aux

instance Show S7a_Aux

instance Marshal.StaticSize S7a_Aux

instance Marshal.ReadRaw S7a_Aux

instance Marshal.WriteRaw S7a_Aux

instance BG.Storable S7a_Aux

instance Struct.IsStruct S7a_Aux

data S7a

instance Eq S7a

instance BG.Generic S7a

instance Ord S7a

instance Show S7a

instance BG.HasFFIType S7a

instance Marshal.ReadRaw S7a

instance Marshal.StaticSize S7a

instance BG.Storable S7a

instance Marshal.WriteRaw S7a

data S7b_Aux

instance Eq S7b_Aux

instance BG.Generic S7b_Aux

instance Show S7b_Aux

instance Marshal.StaticSize S7b_Aux

instance Marshal.ReadRaw S7b_Aux

instance Marshal.WriteRaw S7b_Aux

instance BG.Storable S7b_Aux

instance Struct.IsStruct S7b_Aux

data S7b

instance Eq S7b

instance BG.Generic S7b

instance Ord S7b

instance Show S7b

instance BG.HasFFIType S7b

instance Marshal.ReadRaw S7b

instance Marshal.StaticSize S7b

instance BG.Storable S7b

instance Marshal.WriteRaw S7b
