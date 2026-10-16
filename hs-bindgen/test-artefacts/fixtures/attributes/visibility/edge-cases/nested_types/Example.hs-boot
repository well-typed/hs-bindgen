{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union

data U

instance BG.Generic U

instance Marshal.StaticSize U

instance Marshal.ReadRaw U

instance Marshal.WriteRaw U

instance BG.Storable U

instance Union.IsUnion U

data S

instance BG.Generic S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S
