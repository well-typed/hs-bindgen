{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Triplet

instance Eq Triplet

instance BG.Generic Triplet

instance Show Triplet

instance IsA.IsArray Triplet

instance Marshal.ReadRaw Triplet

instance Marshal.StaticSize Triplet

instance BG.Storable Triplet

instance Marshal.WriteRaw Triplet

data List

instance Eq List

instance BG.Generic List

instance Show List

instance IsA.IsArray List

data Matrix

instance Eq Matrix

instance BG.Generic Matrix

instance Show Matrix

instance IsA.IsArray Matrix

instance Marshal.ReadRaw Matrix

instance Marshal.StaticSize Matrix

instance BG.Storable Matrix

instance Marshal.WriteRaw Matrix

data Tripletlist

instance Eq Tripletlist

instance BG.Generic Tripletlist

instance Show Tripletlist

instance IsA.IsArray Tripletlist

data Example

instance Eq Example

instance BG.Generic Example

instance Show Example

instance Marshal.StaticSize Example

instance Marshal.ReadRaw Example

instance Marshal.WriteRaw Example

instance BG.Storable Example

instance Struct.IsStruct Example

data Sudoku

instance Eq Sudoku

instance BG.Generic Sudoku

instance Show Sudoku

instance IsA.IsArray Sudoku

instance Marshal.ReadRaw Sudoku

instance Marshal.StaticSize Sudoku

instance BG.Storable Sudoku

instance Marshal.WriteRaw Sudoku
