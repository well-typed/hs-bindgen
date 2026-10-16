{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Hoge

instance Eq Hoge

instance BG.Generic Hoge

instance Show Hoge

instance Marshal.StaticSize Hoge

instance Marshal.ReadRaw Hoge

instance Marshal.WriteRaw Hoge

instance BG.Storable Hoge

instance Struct.IsStruct Hoge
