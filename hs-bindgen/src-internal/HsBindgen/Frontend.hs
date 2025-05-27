-- | Main entry point into the clang AST folding code
--
-- Intended for unqualified import.
module HsBindgen.Frontend (processTranslationUnit) where

import HsBindgen.Frontend.AST (TranslationUnit (..))
import HsBindgen.Frontend.Graph.UseDef qualified as UseDef
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.NameMangler
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.Parse.Monad (ParseEnv)
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

processTranslationUnit :: ParseEnv -> IO (TranslationUnit Final)
processTranslationUnit parseEnvironment = do
    afterParse <- parseTranslationUnit parseEnvironment

    let (afterHandleMacros, macroErrors) = handleMacros afterParse
        afterRenameAnon                  = renameAnon afterHandleMacros
        afterResolveBindingSpecs         = resolveBindingSpecs afterRenameAnon
        afterNameMangler                 = mangleNames afterResolveBindingSpecs

    writeFile "usedef.mermaid" $ UseDef.dumpMermaid show (unitAnn afterParse)

    -- TODO: Use tracer for these
    mapM_ print macroErrors

    return afterRenameAnon
