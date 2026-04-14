module HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (
    ConstructTranslationUnit
  , DeclMeta(..)
  ) where

import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph.Definition (DeclUseGraph)
import HsBindgen.Frontend.Analysis.UseDeclGraph (UseDeclGraph)
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
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
  AnnConstructTranslationUnit "Global"          = ReparseInfo
  AnnConstructTranslationUnit _                 = NoAnn

instance IsPass ConstructTranslationUnit where
  type MacroBody  ConstructTranslationUnit = ParsedMacro
  type ExtBinding ConstructTranslationUnit = Void
  type Ann ix     ConstructTranslationUnit = AnnConstructTranslationUnit ix
  type Msg        ConstructTranslationUnit = NoMsg Level

{-------------------------------------------------------------------------------
  Information about the declarations
-------------------------------------------------------------------------------}

data DeclMeta = DeclMeta {
      declIndex    :: DeclIndex
    , useDeclGraph :: UseDeclGraph
    , declUseGraph :: DeclUseGraph
    }
  deriving stock (Show, Generic)

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassMacroBody         AssignAnonIds ConstructTranslationUnit
instance CoercePassId                AssignAnonIds ConstructTranslationUnit
instance CoercePassMacroId           AssignAnonIds ConstructTranslationUnit

instance CoercePassAnn "TypeFunArg"  AssignAnonIds ConstructTranslationUnit
instance CoercePassAnn "StructField" AssignAnonIds ConstructTranslationUnit
instance CoercePassAnn "UnionField"  AssignAnonIds ConstructTranslationUnit
instance CoercePassAnn "Typedef"     AssignAnonIds ConstructTranslationUnit
instance CoercePassAnn "Function"    AssignAnonIds ConstructTranslationUnit
instance CoercePassAnn "Global"      AssignAnonIds ConstructTranslationUnit
