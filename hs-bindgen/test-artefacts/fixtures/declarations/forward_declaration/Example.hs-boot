{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data S1_t

instance Eq S1_t

instance BG.Generic S1_t

instance Show S1_t

instance Marshal.StaticSize S1_t

instance Marshal.ReadRaw S1_t

instance Marshal.WriteRaw S1_t

instance BG.Storable S1_t

instance Struct.IsStruct S1_t

data S2

instance Eq S2

instance BG.Generic S2

instance Show S2

instance Marshal.StaticSize S2

instance Marshal.ReadRaw S2

instance Marshal.WriteRaw S2

instance BG.Storable S2

instance Struct.IsStruct S2
