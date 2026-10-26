module HsBindgen.Frontend.Pass.TypecheckMacros (
    typecheckMacros
  ) where

import Data.Map qualified as Map

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
-- Macros that fail to typecheck stay in the translation unit; the
-- @ApplyMacroFallback@ pass decides what becomes of them.
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
          collectKnownTypes unit.db
        (db', knownMacroTypes) =
          typecheckDecls macroLang unit.db
    in  ( unit{C.db = db'}
        , Map.fromList [
              (n, v)
            | (declId, v) <- Map.toList knownTypes
            , Just n <- [C.renderNamedDeclId declId]
            ]
        , knownMacroTypes
        )
