-- | Main entry point into the clang AST folding code
--
-- Intended for unqualified import.
module HsBindgen.Frontend (processTranslationUnit) where

import Control.Tracer (Tracer)

import Clang.LowLevel.Core

import HsBindgen.Frontend.AST (TranslationUnit(..))
import HsBindgen.Frontend.Graph.UseDef qualified as UseDef
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.NameMangler
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.Parse.Monad (ParseEnv (ParseEnv), ParseLog)
import HsBindgen.Frontend.Pass.RenameAnon
import HsBindgen.Frontend.Pass.ResolveBindingSpecs
import HsBindgen.Util.Tracer (TraceWithCallStack)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Alias for the final pass
--
-- This avoids having to change many type signatures if we ever insert another
-- pass in the C processing pipeline.
type Final = RenameAnon

processTranslationUnit :: CXTranslationUnit -> Tracer IO (TraceWithCallStack ParseLog) -> IO (TranslationUnit Final)
processTranslationUnit unit tracer = do
    afterParse <- parseTranslationUnit $ ParseEnv unit tracer

    let (afterHandleMacros, macroErrors) = handleMacros afterParse
        afterRenameAnon                  = renameAnon afterHandleMacros
        afterResolveBindingSpecs         = resolveBindingSpecs afterRenameAnon
        afterNameMangler                 = mangleNames afterResolveBindingSpecs

    writeFile "usedef.mermaid" $ UseDef.dumpMermaid show (unitAnn afterParse)

    -- TODO: Use tracer for these
    mapM_ print macroErrors

    return afterRenameAnon
