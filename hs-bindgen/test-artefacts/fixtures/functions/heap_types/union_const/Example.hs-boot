{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union

data T

instance BG.Generic T

instance Marshal.StaticSize T

instance Marshal.ReadRaw T

instance Marshal.WriteRaw T

instance BG.Storable T

instance Union.IsUnion T
