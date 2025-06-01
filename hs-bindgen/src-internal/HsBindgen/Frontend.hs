-- | Main entry point into the clang AST folding code
--
-- Intended for unqualified import.
module HsBindgen.Frontend (processTranslationUnit) where

import HsBindgen.BindingSpecs (ResolvedBindingSpecs)
import HsBindgen.Frontend.AST.External qualified as Ext
import HsBindgen.Frontend.AST.Finalize
import HsBindgen.Frontend.AST.Internal qualified as Int
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

-- TODO: Pass in command line options to write out include/use-def graphs
processTranslationUnit :: ParseEnv -> IO Ext.TranslationUnit
processTranslationUnit parseEnvironment = do
    afterParse <- parseTranslationUnit parseEnvironment

    -- TODO receive binding specifications via arguments
    let confSpecs, extSpecs :: ResolvedBindingSpecs
        confSpecs = undefined
        extSpecs  = undefined

    let (afterHandleMacros, macroErrors) =
          handleMacros afterParse
        afterRenameAnon =
          renameAnon afterHandleMacros
        (afterResolveBindingSpecs, bindingSpecsErrors) =
          resolveBindingSpecs confSpecs extSpecs afterRenameAnon
        (afterNameMangler, mangleErrors) =
          mangleNames afterResolveBindingSpecs

    writeFile "usedef.mermaid" $
      UseDef.dumpMermaid show (Int.unitAnn afterParse)

    -- TODO: Use tracer for these
    mapM_ print macroErrors
    mapM_ print bindingSpecsErrors
    mapM_ print mangleErrors

    return $ finalize afterNameMangler

