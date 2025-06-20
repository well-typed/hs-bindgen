module HsBindgen.Frontend.Pass.Slice.IsPass (
    Slice
  ) where

import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Sort.IsPass (DeclMeta, Sort)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Slice :: Pass
data Slice a deriving anyclass ValidPass

type family AnnSlice ix where
  AnnSlice "TranslationUnit" = DeclMeta
  AnnSlice ix                = Ann ix Parse

instance IsPass Slice where
  type Id         Slice = Id         Sort
  type FieldName  Slice = FieldName  Sort
  type TypedefRef Slice = TypedefRef Sort
  type MacroBody  Slice = MacroBody  Sort
  type Ann ix     Slice = AnnSlice ix
