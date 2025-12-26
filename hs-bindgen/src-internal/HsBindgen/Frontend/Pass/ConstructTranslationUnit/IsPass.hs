{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoRecordWildCards #-}

module HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (
    ConstructTranslationUnit
  , DeclMeta(..)
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

instance CoercePassMacroBody AssignAnonIds ConstructTranslationUnit
instance CoercePassId        AssignAnonIds ConstructTranslationUnit
