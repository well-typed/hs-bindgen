{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Eq, Show)

data SS_anon'y

instance Eq SS_anon'y

instance BG.Generic SS_anon'y

instance Show SS_anon'y

instance Marshal.StaticSize SS_anon'y

instance Marshal.ReadRaw SS_anon'y

instance Marshal.WriteRaw SS_anon'y

instance BG.Storable SS_anon'y

instance Struct.IsStruct SS_anon'y

data SS

instance Eq SS

instance BG.Generic SS

instance Show SS

instance Marshal.StaticSize SS

instance Marshal.ReadRaw SS

instance Marshal.WriteRaw SS

instance BG.Storable SS

instance Struct.IsStruct SS

data SU_anon'y

instance BG.Generic SU_anon'y

instance Marshal.StaticSize SU_anon'y

instance Marshal.ReadRaw SU_anon'y

instance Marshal.WriteRaw SU_anon'y

instance BG.Storable SU_anon'y

instance Union.IsUnion SU_anon'y

data SU

instance BG.Generic SU

instance Marshal.StaticSize SU

instance Marshal.ReadRaw SU

instance Marshal.WriteRaw SU

instance BG.Storable SU

instance Struct.IsStruct SU

data US_anon'y

instance Eq US_anon'y

instance BG.Generic US_anon'y

instance Show US_anon'y

instance Marshal.StaticSize US_anon'y

instance Marshal.ReadRaw US_anon'y

instance Marshal.WriteRaw US_anon'y

instance BG.Storable US_anon'y

instance Struct.IsStruct US_anon'y

data US

instance BG.Generic US

instance Marshal.StaticSize US

instance Marshal.ReadRaw US

instance Marshal.WriteRaw US

instance BG.Storable US

instance Union.IsUnion US

data UU_anon'y

instance BG.Generic UU_anon'y

instance Marshal.StaticSize UU_anon'y

instance Marshal.ReadRaw UU_anon'y

instance Marshal.WriteRaw UU_anon'y

instance BG.Storable UU_anon'y

instance Union.IsUnion UU_anon'y

data UU

instance BG.Generic UU

instance Marshal.StaticSize UU

instance Marshal.ReadRaw UU

instance Marshal.WriteRaw UU

instance BG.Storable UU

instance Union.IsUnion UU
