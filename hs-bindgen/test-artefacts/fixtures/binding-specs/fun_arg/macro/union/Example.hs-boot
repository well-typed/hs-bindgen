{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union

data MyUnion

instance BG.Generic MyUnion

instance Marshal.StaticSize MyUnion

instance Marshal.ReadRaw MyUnion

instance Marshal.WriteRaw MyUnion

instance BG.Storable MyUnion

instance Union.IsUnion MyUnion

data A

instance BG.Generic A

instance Marshal.ReadRaw A

instance Marshal.StaticSize A

instance BG.Storable A

instance Marshal.WriteRaw A

data B

instance BG.Generic B

instance Marshal.ReadRaw B

instance Marshal.StaticSize B

instance BG.Storable B

instance Marshal.WriteRaw B

data E

instance BG.Generic E
