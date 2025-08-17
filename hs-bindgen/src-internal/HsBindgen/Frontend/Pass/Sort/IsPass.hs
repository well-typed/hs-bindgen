module HsBindgen.Frontend.Pass.Sort.IsPass (
    Sort
  , DeclMeta(..)
  , SortMsg(..)
  ) where

import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.NonParsedDecls (NonParsedDecls)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse, ReparseInfo)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition

  The only thing that changes in this pass is the annotation on the top-level
  'TranslationUnit'; for everything else we simply refer to the 'Parse' pass.
-------------------------------------------------------------------------------}

type Sort :: Pass
data Sort a deriving anyclass ValidPass

type family AnnSort (ix :: Symbol) :: Star where
  AnnSort "TranslationUnit" = DeclMeta
  AnnSort "StructField"     = ReparseInfo
  AnnSort "UnionField"      = ReparseInfo
  AnnSort "Typedef"         = ReparseInfo
  AnnSort "Function"        = ReparseInfo
  AnnSort _                 = NoAnn

instance IsPass Sort where
  type Id         Sort = Id         Parse
  type FieldName  Sort = FieldName  Parse
  type TypedefRef Sort = TypedefRef Parse
  type MacroBody  Sort = MacroBody  Parse
  type ExtBinding Sort = ExtBinding Parse
  type Ann ix     Sort = AnnSort ix
  type Msg        Sort = SortMsg

{-------------------------------------------------------------------------------
  Information about the declarations
-------------------------------------------------------------------------------}

data DeclMeta = DeclMeta {
      declIndex     :: DeclIndex
    , declUseDecl   :: UseDeclGraph
    , declDeclUse   :: DeclUseGraph
    , declNonParsed :: NonParsedDecls
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data SortMsg =
    SortErrorDeclIndex DeclIndexError
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
