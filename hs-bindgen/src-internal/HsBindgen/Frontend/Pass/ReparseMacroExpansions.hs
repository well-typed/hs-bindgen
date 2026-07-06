module HsBindgen.Frontend.Pass.ReparseMacroExpansions (
    reparseMacroExpansions
  ) where

import Prelude hiding (zip)

import Data.Map qualified as Map

import Clang.CStandard (ClangCStandard)

import HsBindgen.Frontend.LanguageC qualified as LanC
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (PrepareReparse)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC (lanC)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.LanC.IsPass (LanC)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.Zip (zip)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.Intermediate.Zip.IsPass (Zip)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass (ReparseMacroExpansions)
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports (Map, Set)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro

-- | Reparse declarations that have macro expansions in their type positions,
-- using the known-types environment from 'typecheckMacros'.
reparseMacroExpansions ::
     forall l. Macro.HasTypes l
  => ClangCStandard
  -> Map LanC.CName (C.Type PrepareReparse)
     -- ^ Known non-macro names and their types (see 'ReparseEnv')
  -> Set LanC.CName
     -- ^ Known macro names (see 'ReparseEnv')
  -> Macro.Lang l
  -> C.TranslationUnit l PrepareReparse
  -> C.TranslationUnit l ReparseMacroExpansions
reparseMacroExpansions cStd knownNonMacroTypes knownMacros macroLang unit =
    unit3
  where
    -- | Intermediate pass 1: 'LanC'
    --
    -- Parse raw @libclang@ tokens using @language-c@ if applicable, inserting
    -- references to macro types where necessary. After this, references to
    -- macro types will not yet have underlying types.
    --
    unit1 :: C.TranslationUnit l LanC
    unit1 = lanC cStd (Map.map coercePass knownNonMacroTypes) knownMacros unit

    -- | Intermediate pass 2: 'Zip'
    --
    -- Reconcile each reparsed declaration with its pre-reparse representation,
    -- producing a single zipped C AST. This adds underlying types to macro type
    -- references. It also updates the 'DeclUseGraph' to replace dependencies
    -- that pointed to underlying types (as seen before reparsing) with
    -- references to the macro-defined types discovered during reparsing. For
    -- example, if a declaration depends on @A@ before reparsing, but after
    -- reparsing it is known to depend on the macro-defined type @B@ (which
    -- expands to @A@), the dependency on @A@ is replaced by a dependency on
    -- @B@.
    --
    unit2 :: C.TranslationUnit l Zip
    unit2 = zip macroLang unit1

    -- | Intermediate pass 3: final
    --
    -- 'Zip' is just an internal pass parameter, so we coerce the result of the
    -- previous pass to the external pass parameter 'ReparseMacroExpansions'.
    --
    unit3 :: C.TranslationUnit l ReparseMacroExpansions
    unit3 = coercePass unit2
