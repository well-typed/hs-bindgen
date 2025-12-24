module HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (
    ConstructTranslationUnit
  , DeclMeta(..)
  , ConstructTranslationUnitMsg
  ) where

import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo, UnparsedMacro)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition

  Inspect the parse results and construct the translation unit.
-------------------------------------------------------------------------------}

type ConstructTranslationUnit :: Pass
data ConstructTranslationUnit a

type family AnnConstructTranslationUnit (ix :: Symbol) :: Star where
  AnnConstructTranslationUnit "TranslationUnit" = DeclMeta
  AnnConstructTranslationUnit "StructField"     = ReparseInfo
  AnnConstructTranslationUnit "UnionField"      = ReparseInfo
  AnnConstructTranslationUnit "Typedef"         = ReparseInfo
  AnnConstructTranslationUnit "Function"        = ReparseInfo
  AnnConstructTranslationUnit _                 = NoAnn

instance IsPass ConstructTranslationUnit where
  type MacroBody  ConstructTranslationUnit = UnparsedMacro
  type ExtBinding ConstructTranslationUnit = Void
  type Ann ix     ConstructTranslationUnit = AnnConstructTranslationUnit ix
  type Msg        ConstructTranslationUnit = ConstructTranslationUnitMsg

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

data ConstructTranslationUnitMsg
  deriving stock (Show, Generic)

instance PrettyForTrace ConstructTranslationUnitMsg where
  prettyForTrace = const "no message available"

instance IsTrace Level ConstructTranslationUnitMsg where
  getDefaultLogLevel = const Debug
  getSource          = const HsBindgen
  getTraceId         = const "construct-translation-unit"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassMacroBody AssignAnonIds ConstructTranslationUnit
instance CoercePassId        AssignAnonIds ConstructTranslationUnit
