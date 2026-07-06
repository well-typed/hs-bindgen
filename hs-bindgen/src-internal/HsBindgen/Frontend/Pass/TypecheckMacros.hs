module HsBindgen.Frontend.Pass.TypecheckMacros (
    typecheckMacros
  ) where

import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map

import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.KnownTypes
import HsBindgen.Frontend.Pass.TypecheckMacros.Typecheck
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

type In  = ConstructTranslationUnit
type Out = TypecheckMacros

-- | We perform two traversals:
--
-- Traversal 1: collect known types
--
-- Traversal 2: typecheck macros
--
-- Return the updated translation unit together with the 'LanC.ReparseEnv' that
-- 'reparseMacroExpansions' uses to reparse declarations.
--
-- Register macro typecheck failures in @DeclMeta@.
typecheckMacros ::
     Macro.HasTypes l
  => Macro.Lang l
  -> C.TranslationUnit l In
  -> ( C.TranslationUnit l Out
     , Map LanC.CName (C.Type TypecheckMacros)
     , Set LanC.CName
     )
typecheckMacros macroLang unit =
    let knownTypes =
          collectKnownTypes unit.decls
        (tcRes, resolvedMacros) =
          typecheckDecls macroLang unit.decls
        (failedMacros, typecheckedDecls) =
          partitionEithers tcRes
    in  ( reconstructAfterTypecheck unit failedMacros typecheckedDecls
        , Map.fromList [
              (n, v)
            | (declId, v) <- Map.toList knownTypes
            , Just n <- [C.renderNonAnonDeclId declId]
            ]
        , resolvedMacros
        )

reconstructAfterTypecheck ::
     C.TranslationUnit l In
  -> [FailedMacro]
  -> [C.Decl l Out]
  -> C.TranslationUnit l Out
reconstructAfterTypecheck unit failedMacros decls =
    C.TranslationUnit{
        decls        = decls
      , includeGraph = unit.includeGraph
      , meta         = unit.meta{
            declIndex =
              Foldable.foldl'
                (flip DeclIndex.registerMacroTypecheckFailure)
                unit.meta.declIndex
                failedMacros
          }
      }
