-- | Main entry point into the clang AST folding code
--
-- Intended for unqualified import.
module HsBindgen.Frontend (processTranslationUnit) where

import Clang.LowLevel.Core

import HsBindgen.Frontend.AST (TranslationUnit)
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.NameMangler
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.RenameAnon
import HsBindgen.Frontend.Pass.ResolveBindingSpecs

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Alias for the final pass
--
-- This avoids having to change many type signatures if we ever insert another
-- pass in the C processing pipeline.
type Final = RenameAnon

processTranslationUnit :: CXTranslationUnit -> IO (TranslationUnit Final)
processTranslationUnit unit = do
    (afterParse, unsupportedErrors) <- parseTranslationUnit unit

    let (afterHandleMacros, macroErrors) = handleMacros afterParse
        afterRenameAnon                  = renameAnon afterHandleMacros
        afterResolveBindingSpecs         = resolveBindingSpecs afterRenameAnon
        afterNameMangler                 = mangleNames afterResolveBindingSpecs

    -- TODO: Use tracer for these
    mapM_ print unsupportedErrors
    mapM_ print macroErrors

    return afterRenameAnon
