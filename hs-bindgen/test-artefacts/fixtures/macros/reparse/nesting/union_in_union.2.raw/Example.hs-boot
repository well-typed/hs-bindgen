{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union

data T1_x

instance BG.Generic T1_x

instance Marshal.StaticSize T1_x

instance Marshal.ReadRaw T1_x

instance Marshal.WriteRaw T1_x

instance BG.Storable T1_x

instance Union.IsUnion T1_x

data T1

instance BG.Generic T1

instance Marshal.StaticSize T1

instance Marshal.ReadRaw T1

instance Marshal.WriteRaw T1

instance BG.Storable T1

instance Union.IsUnion T1

data T2

instance BG.Generic T2

instance Marshal.StaticSize T2

instance Marshal.ReadRaw T2

instance Marshal.WriteRaw T2

instance BG.Storable T2

instance Union.IsUnion T2

data T2_x

instance BG.Generic T2_x

instance Marshal.StaticSize T2_x

instance Marshal.ReadRaw T2_x

instance Marshal.WriteRaw T2_x

instance BG.Storable T2_x

instance Union.IsUnion T2_x

data T3

instance BG.Generic T3

instance Marshal.StaticSize T3

instance Marshal.ReadRaw T3

instance Marshal.WriteRaw T3

instance BG.Storable T3

instance Union.IsUnion T3

data T3_x

instance BG.Generic T3_x

instance Marshal.StaticSize T3_x

instance Marshal.ReadRaw T3_x

instance Marshal.WriteRaw T3_x

instance BG.Storable T3_x

instance Union.IsUnion T3_x
