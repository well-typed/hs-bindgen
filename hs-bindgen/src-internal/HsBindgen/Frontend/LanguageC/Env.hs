{-# LANGUAGE CPP #-}

module HsBindgen.Frontend.LanguageC.Env (
    ReparseEnv
  , bespokeReparseEnv
  ) where

import Data.Map qualified as Map

import HsBindgen.Clang.CStandard
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.LanguageC.PartialAST
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Imports

#if !MIN_VERSION_language_c(0,10,2)
import HsBindgen.Language.C qualified as C
#endif

-- | Types in scope when reparsing a particular declaration
type ReparseEnv = Map CName (C.Type ReparseMacroExpansions)

-- | C-standard-specific "built in" types.
--
-- Careful, user-provided values must supersede bespoke types.
bespokeReparseEnv :: ClangCStandard -> ReparseEnv
bespokeReparseEnv = Map.fromList . bespokeTypes

{-------------------------------------------------------------------------------
  Bespoke types
-------------------------------------------------------------------------------}

-- | \"Primitive\" we expect the reparser to recognize
--
-- The language-c parser does not support these explicitly.
bespokeTypes :: ClangCStandard -> [(CName, C.Type p)]
bespokeTypes = \case
#if !MIN_VERSION_language_c(0,10,2)
    -- Make sure that we really only replace keywords lacking definitions.
    --
    -- If we add entries for types to `bespokeTypes` which are not keywords
    -- (i.e., are not part of the standard), we will pretend to know what these
    -- types are, but the actual type must come from a header, and we actually
    -- do not know what that definition is.
    ClangCStandard C23 _gnu -> [("bool", C.TypePrim C.PrimBool)]
#endif
    _otherwise -> []
