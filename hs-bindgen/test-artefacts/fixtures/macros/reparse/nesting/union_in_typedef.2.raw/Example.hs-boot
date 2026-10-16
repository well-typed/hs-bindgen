{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Eq, Ord, Show)

data T1

instance BG.Generic T1

instance Marshal.StaticSize T1

instance Marshal.ReadRaw T1

instance Marshal.WriteRaw T1

instance BG.Storable T1

instance Union.IsUnion T1

data T2_Aux

instance BG.Generic T2_Aux

instance Marshal.StaticSize T2_Aux

instance Marshal.ReadRaw T2_Aux

instance Marshal.WriteRaw T2_Aux

instance BG.Storable T2_Aux

instance Union.IsUnion T2_Aux

data T2

instance Eq T2

instance BG.Generic T2

instance Ord T2

instance Show T2

instance BG.HasFFIType T2

instance Marshal.ReadRaw T2

instance Marshal.StaticSize T2

instance BG.Storable T2

instance Marshal.WriteRaw T2

data T3_Aux

instance BG.Generic T3_Aux

instance Marshal.StaticSize T3_Aux

instance Marshal.ReadRaw T3_Aux

instance Marshal.WriteRaw T3_Aux

instance BG.Storable T3_Aux

instance Union.IsUnion T3_Aux

data T3

instance Eq T3

instance BG.Generic T3

instance Ord T3

instance Show T3

instance BG.HasFFIType T3

instance Marshal.ReadRaw T3

instance Marshal.StaticSize T3

instance BG.Storable T3

instance Marshal.WriteRaw T3
