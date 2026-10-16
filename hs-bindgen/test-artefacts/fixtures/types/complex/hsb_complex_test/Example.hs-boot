{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Complex_object_t

instance Eq Complex_object_t

instance BG.Generic Complex_object_t

instance Show Complex_object_t

instance Marshal.StaticSize Complex_object_t

instance Marshal.ReadRaw Complex_object_t

instance Marshal.WriteRaw Complex_object_t

instance BG.Storable Complex_object_t

instance Struct.IsStruct Complex_object_t
