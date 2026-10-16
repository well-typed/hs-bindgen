{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data S

instance Eq S

instance BG.Generic S

instance Show S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S

data T

instance Eq T

instance BG.Generic T

instance Show T

instance Marshal.StaticSize T

instance Marshal.ReadRaw T

instance Marshal.WriteRaw T

instance BG.Storable T

instance Struct.IsStruct T
