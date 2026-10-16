{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Foo

instance Eq Foo

instance BG.Generic Foo

instance Show Foo

instance Marshal.StaticSize Foo

instance Marshal.ReadRaw Foo

instance Marshal.WriteRaw Foo

instance BG.Storable Foo

instance Struct.IsStruct Foo

data Bar

instance Eq Bar

instance BG.Generic Bar

instance Show Bar

instance Marshal.StaticSize Bar

instance Marshal.ReadRaw Bar

instance Marshal.WriteRaw Bar

instance BG.Storable Bar

instance Struct.IsStruct Bar

data Ex3_ex3_struct

instance Eq Ex3_ex3_struct

instance BG.Generic Ex3_ex3_struct

instance Show Ex3_ex3_struct

instance Marshal.StaticSize Ex3_ex3_struct

instance Marshal.ReadRaw Ex3_ex3_struct

instance Marshal.WriteRaw Ex3_ex3_struct

instance BG.Storable Ex3_ex3_struct

instance Struct.IsStruct Ex3_ex3_struct

data Ex3

instance Eq Ex3

instance BG.Generic Ex3

instance Show Ex3

instance Marshal.StaticSize Ex3

instance Marshal.ReadRaw Ex3

instance Marshal.WriteRaw Ex3

instance BG.Storable Ex3

instance Struct.IsStruct Ex3

data Ex4_odd

instance Eq Ex4_odd

instance BG.Generic Ex4_odd

instance Show Ex4_odd

instance Marshal.StaticSize Ex4_odd

instance Marshal.ReadRaw Ex4_odd

instance Marshal.WriteRaw Ex4_odd

instance BG.Storable Ex4_odd

instance Struct.IsStruct Ex4_odd

data Ex4_even

instance Eq Ex4_even

instance BG.Generic Ex4_even

instance Show Ex4_even

instance Marshal.StaticSize Ex4_even

instance Marshal.ReadRaw Ex4_even

instance Marshal.WriteRaw Ex4_even

instance BG.Storable Ex4_even

instance Struct.IsStruct Ex4_even
