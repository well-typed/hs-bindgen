{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data OkBefore

instance Eq OkBefore

instance BG.Generic OkBefore

instance Show OkBefore

instance Marshal.StaticSize OkBefore

instance Marshal.ReadRaw OkBefore

instance Marshal.WriteRaw OkBefore

instance BG.Storable OkBefore

instance Struct.IsStruct OkBefore

data OkAfter

instance Eq OkAfter

instance BG.Generic OkAfter

instance Show OkAfter

instance Marshal.StaticSize OkAfter

instance Marshal.ReadRaw OkAfter

instance Marshal.WriteRaw OkAfter

instance BG.Storable OkAfter

instance Struct.IsStruct OkAfter
