module HsBindgen.Frontend.Pass.Slice.IsPass (
    Slice
  ) where

import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Sort.IsPass (Sort)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Slice :: Pass
data Slice a deriving anyclass ValidPass

type family AnnSlice ix where
  AnnSlice ix = Ann ix Sort

instance IsPass Slice where
  type Id         Slice = Id         Sort
  type FieldName  Slice = FieldName  Sort
  type TypedefRef Slice = TypedefRef Sort
  type MacroBody  Slice = MacroBody  Sort
  type ExtBinding Slice = ExtBinding Sort
  type Ann ix     Slice = AnnSlice ix
