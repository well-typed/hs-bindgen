module HsBindgen.Frontend.Pass.ReparseMacroExpansions (
    reparseMacroExpansions
  ) where

import Prelude hiding (zip)

import Clang.CStandard (ClangCStandard)

import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (PrepareReparse)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC (lanC)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports (Map, Set)
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

-- | Reparse declarations that have macro expansions in their type positions,
-- using the known-types environment from 'typecheckMacros'.
reparseMacroExpansions ::
     Macro.HasTypes l
  => ClangCStandard
  -> Map LanC.CName (C.Type PrepareReparse)
     -- ^ Known non-macro names and their types (see 'ReparseEnv')
  -> Set LanC.CName
     -- ^ Known macro names (see 'ReparseEnv')
  -> Macro.Lang l
  -> C.TranslationUnit l PrepareReparse
  -> C.TranslationUnit l ReparseMacroExpansions
reparseMacroExpansions cStd knownNonMacroTypes knownMacros macroLang unit =
    lanC cStd knownNonMacroTypes knownMacros macroLang unit
