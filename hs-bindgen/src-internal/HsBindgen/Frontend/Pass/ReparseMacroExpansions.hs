module HsBindgen.Frontend.Pass.ReparseMacroExpansions (
    reparseMacroExpansions
  ) where

import Data.Map.Lazy qualified as Map

import Clang.CStandard (ClangCStandard)

import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass (CoercePass (coercePass))

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Reparse declarations that have macro expansions in their type positions,
-- using the known-types environment from 'typecheckMacros'.
reparseMacroExpansions ::
     ClangCStandard
  -> Map LanC.CName (C.Type PrepareReparse)
     -- ^ Known non-macro names and their types (see 'ReparseEnv')
  -> Set LanC.CName
     -- ^ Known macro names (see 'ReparseEnv')
  -> C.TranslationUnit l PrepareReparse
  -> C.TranslationUnit l ReparseMacroExpansions
reparseMacroExpansions cStd knownNonMacroTypes knownMacros unit = unitFinal
  where
    unit1 = lanC cStd (Map.map coercePass knownNonMacroTypes) knownMacros unit

    unitFinal = coercePass unit1
