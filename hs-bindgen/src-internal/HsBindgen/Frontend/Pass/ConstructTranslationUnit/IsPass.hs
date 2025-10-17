module HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (
    ConstructTranslationUnit
  , DeclMeta(..)
  , ConstructTranslationUnitMsg(..)
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

  Inspect the parse results and construct the translation unit.
-------------------------------------------------------------------------------}

type ConstructTranslationUnit :: Pass
data ConstructTranslationUnit a deriving anyclass ValidPass

type family AnnConstructTranslationUnit (ix :: Symbol) :: Star where
  AnnConstructTranslationUnit "TranslationUnit" = DeclMeta
  AnnConstructTranslationUnit "StructField"     = ReparseInfo
  AnnConstructTranslationUnit "UnionField"      = ReparseInfo
  AnnConstructTranslationUnit "Typedef"         = ReparseInfo
  AnnConstructTranslationUnit "Function"        = ReparseInfo
  AnnConstructTranslationUnit _                 = NoAnn

instance IsPass ConstructTranslationUnit where
  type Id           ConstructTranslationUnit = C.PrelimDeclId
  type FieldName    ConstructTranslationUnit = C.Name
  type ArgumentName ConstructTranslationUnit = Maybe C.Name
  type TypedefRef   ConstructTranslationUnit = OrigTypedefRef ConstructTranslationUnit
  type MacroBody    ConstructTranslationUnit = UnparsedMacro
  type ExtBinding   ConstructTranslationUnit = Void
  type Ann ix       ConstructTranslationUnit = AnnConstructTranslationUnit ix
  type Msg          ConstructTranslationUnit = ConstructTranslationUnitMsg

{-------------------------------------------------------------------------------
  Information about the declarations
-------------------------------------------------------------------------------}

data DeclMeta = DeclMeta {
      declIndex       :: DeclIndex
    , declUseDecl     :: UseDeclGraph
    , declDeclUse     :: DeclUseGraph
    }
  deriving stock (Show, Generic)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ConstructTranslationUnitMsg =
    ConstructTranslationUnitErrorDeclIndex DeclIndexError
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePass TypedefRefWrapper Parse ConstructTranslationUnit where
  coercePass (TypedefRefWrapper p) = TypedefRefWrapper (coercePass p)
