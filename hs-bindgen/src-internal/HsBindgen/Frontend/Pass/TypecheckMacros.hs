module HsBindgen.Frontend.Pass.TypecheckMacros (
    typecheckMacros
  ) where

import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map

import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.KnownTypes
import HsBindgen.Frontend.Pass.TypecheckMacros.Typecheck
import HsBindgen.Imports

type Pre  = ConstructTranslationUnit
type Post = TypecheckMacros

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
     C.TranslationUnit Pre
  -> ( C.TranslationUnit Post
     , Map LanC.CName (C.Type ReparseMacroExpansions)
     , Map LanC.CName (C.Type ReparseMacroExpansions)
     )
typecheckMacros unit =
    let (typedefTypes, taggedTypes) =
          collectKnownTypes unit.decls
        (tcRes, resolvedMacroTypes) =
          typecheckDecls typedefTypes taggedTypes unit.decls
        (failedMacros, typecheckedDecls) =
          partitionEithers tcRes
    in  ( reconstructAfterTypecheck unit failedMacros typecheckedDecls
        ,    Map.map coercePass $ typedefTypes
          <> Map.mapKeys renderCDeclNameC taggedTypes
        , Map.map coercePass resolvedMacroTypes
        )

reconstructAfterTypecheck ::
     C.TranslationUnit Pre
  -> [FailedMacro]
  -> [C.Decl Post]
  -> C.TranslationUnit Post
reconstructAfterTypecheck unit failedMacros decls =
    C.TranslationUnit{
        decls        = decls
      , includeGraph = unit.includeGraph
      , ann          = unit.ann{
            declIndex =
              Foldable.foldl'
                (flip DeclIndex.registerMacroTypecheckFailure)
                unit.ann.declIndex
                failedMacros
          }
      }
