module HsBindgen.Frontend.Pass.NameAnon.IsPass (NameAnon) where

import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C (CName)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type NameAnon :: Pass
data NameAnon a deriving anyclass ValidPass

type family AnnNameAnon ix where
  AnnNameAnon "TranslationUnit" = DeclMeta
  AnnNameAnon _                 = NoAnn

instance IsPass NameAnon where
  type Id         NameAnon = CName
  type FieldName  NameAnon = CName
  type TypedefRef NameAnon = CName
  type MacroBody  NameAnon = C.CheckedMacro NameAnon
  type ExtBinding NameAnon = Void
  type Ann ix     NameAnon = AnnNameAnon ix
