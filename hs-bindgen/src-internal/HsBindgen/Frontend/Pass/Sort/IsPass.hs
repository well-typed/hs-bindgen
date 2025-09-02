module HsBindgen.Frontend.Pass.Sort.IsPass (
    Sort
  , SortDeclMeta(..)
  , SortMsg(..)
  ) where

import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph
import HsBindgen.Frontend.AST.Coerce (CoercePass (..))
import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
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
  AnnSort "TranslationUnit" = SortDeclMeta
  AnnSort "StructField"     = ReparseInfo
  AnnSort "UnionField"      = ReparseInfo
  AnnSort "Typedef"         = ReparseInfo
  AnnSort "Function"        = ReparseInfo
  AnnSort _                 = NoAnn

instance IsPass Sort where
  type Id           Sort = C.PrelimDeclId
  type FieldName    Sort = C.Name
  type ArgumentName Sort = Maybe C.Name
  type TypedefRef   Sort = OrigTypedefRef Sort
  type MacroBody    Sort = UnparsedMacro
  type ExtBinding   Sort = Void
  type Ann ix       Sort = AnnSort ix
  type Msg          Sort = SortMsg

{-------------------------------------------------------------------------------
  Information about the declarations
-------------------------------------------------------------------------------}

-- TODO_PR: Rename back to `DeclMeta`.
data SortDeclMeta = SortDeclMeta {
      declIndex       :: DeclIndex
    , declUseDecl     :: UseDeclGraph
    , declDeclUse     :: DeclUseGraph
    }
  deriving stock (Show, Generic)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data SortMsg =
    SortErrorDeclIndex DeclIndexError
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePass TypedefRefWrapper Parse Sort where
  coercePass (TypedefRefWrapper p) = TypedefRefWrapper (coercePass p)
