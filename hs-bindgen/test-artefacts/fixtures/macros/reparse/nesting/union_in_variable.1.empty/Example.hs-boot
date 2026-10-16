{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union

data G1

instance BG.Generic G1

instance Marshal.StaticSize G1

instance Marshal.ReadRaw G1

instance Marshal.WriteRaw G1

instance BG.Storable G1

instance Union.IsUnion G1

data G2

instance BG.Generic G2

instance Marshal.StaticSize G2

instance Marshal.ReadRaw G2

instance Marshal.WriteRaw G2

instance BG.Storable G2

instance Union.IsUnion G2

data G3

instance BG.Generic G3

instance Marshal.StaticSize G3

instance Marshal.ReadRaw G3

instance Marshal.WriteRaw G3

instance BG.Storable G3

instance Union.IsUnion G3
