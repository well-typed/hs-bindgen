{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Outer1_anon'fieldX

instance Eq Outer1_anon'fieldX

instance BG.Generic Outer1_anon'fieldX

instance Show Outer1_anon'fieldX

instance Marshal.StaticSize Outer1_anon'fieldX

instance Marshal.ReadRaw Outer1_anon'fieldX

instance Marshal.WriteRaw Outer1_anon'fieldX

instance BG.Storable Outer1_anon'fieldX

instance Struct.IsStruct Outer1_anon'fieldX

data Outer1

instance Eq Outer1

instance BG.Generic Outer1

instance Show Outer1

instance Marshal.StaticSize Outer1

instance Marshal.ReadRaw Outer1

instance Marshal.WriteRaw Outer1

instance BG.Storable Outer1

instance Struct.IsStruct Outer1

data Outer2_fieldB

instance Eq Outer2_fieldB

instance BG.Generic Outer2_fieldB

instance Show Outer2_fieldB

instance Marshal.StaticSize Outer2_fieldB

instance Marshal.ReadRaw Outer2_fieldB

instance Marshal.WriteRaw Outer2_fieldB

instance BG.Storable Outer2_fieldB

instance Struct.IsStruct Outer2_fieldB

data Outer2

instance Eq Outer2

instance BG.Generic Outer2

instance Show Outer2

instance Marshal.StaticSize Outer2

instance Marshal.ReadRaw Outer2

instance Marshal.WriteRaw Outer2

instance BG.Storable Outer2

instance Struct.IsStruct Outer2

data Inner3

instance Eq Inner3

instance BG.Generic Inner3

instance Show Inner3

instance Marshal.StaticSize Inner3

instance Marshal.ReadRaw Inner3

instance Marshal.WriteRaw Inner3

instance BG.Storable Inner3

instance Struct.IsStruct Inner3

data Outer3

instance Eq Outer3

instance BG.Generic Outer3

instance Show Outer3

instance Marshal.StaticSize Outer3

instance Marshal.ReadRaw Outer3

instance Marshal.WriteRaw Outer3

instance BG.Storable Outer3

instance Struct.IsStruct Outer3
