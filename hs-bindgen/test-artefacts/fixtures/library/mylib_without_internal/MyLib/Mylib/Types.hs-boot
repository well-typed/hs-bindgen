{-# LANGUAGE NoImplicitPrelude #-}

module MyLib.Mylib.Types where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Point_t

instance Eq Point_t

instance BG.Generic Point_t

instance Show Point_t

instance Marshal.StaticSize Point_t

instance Marshal.ReadRaw Point_t

instance Marshal.WriteRaw Point_t

instance BG.Storable Point_t

instance Struct.IsStruct Point_t

data Rect_t

instance Eq Rect_t

instance BG.Generic Rect_t

instance Show Rect_t

instance Marshal.StaticSize Rect_t

instance Marshal.ReadRaw Rect_t

instance Marshal.WriteRaw Rect_t

instance BG.Storable Rect_t

instance Struct.IsStruct Rect_t
