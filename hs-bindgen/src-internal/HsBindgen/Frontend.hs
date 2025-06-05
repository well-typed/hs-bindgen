-- | Main entry point into the clang AST folding code
--
-- Intended for unqualified import.
module HsBindgen.Frontend (processTranslationUnit) where

import Control.Tracer (Tracer)

import Clang.LowLevel.Core
import HsBindgen.BindingSpec (ResolvedBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.Frontend.AST.External qualified as Ext
import HsBindgen.Frontend.AST.Finalize
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.NameMangler
import HsBindgen.Frontend.Pass.Parse (parseDecls)
import HsBindgen.Frontend.Pass.Parse.Monad
import HsBindgen.Frontend.Pass.RenameAnon
import HsBindgen.Frontend.Pass.ResolveBindingSpec
import HsBindgen.Frontend.Pass.Sort
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

processTranslationUnit ::
     Tracer IO (TraceWithCallStack ParseLog)
  -> ResolvedBindingSpec
  -> RootHeader
  -> Predicate
  -> CXTranslationUnit -> IO Ext.TranslationUnit
processTranslationUnit tracer extSpec rootHeader predicate unit = do
    (includeGraph, isMainFile) <-
      processIncludes rootHeader unit
    afterParse <-
      parseDecls tracer rootHeader predicate includeGraph isMainFile unit

    -- TODO receive configuration binding specifications via argument
    let confSpec :: ResolvedBindingSpec
        confSpec = BindingSpec.empty

    let afterSort =
          sortDecls afterParse
        (afterHandleMacros, macroErrors) =
          handleMacros afterSort
        afterRenameAnon =
          renameAnon afterHandleMacros
        (afterResolveBindingSpec, bindingSpecErrors) =
          resolveBindingSpec confSpec extSpec afterRenameAnon
        (afterNameMangler, mangleErrors) =
          mangleNames afterResolveBindingSpec

    -- writeFile "usedef.mermaid" $
    --   UseDef.dumpMermaid (Int.unitAnn afterSort)

    -- TODO: Use tracer for these
    mapM_ print macroErrors
    mapM_ print bindingSpecErrors
    mapM_ print mangleErrors

    return $ finalize afterNameMangler
