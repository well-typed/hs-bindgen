-- | Main entry point into the clang AST folding code
--
-- Intended for unqualified import.
module HsBindgen.Frontend (
    processTranslationUnit
  , FrontendTrace(..)
  ) where

import Control.Monad
import Control.Tracer

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
     Tracer IO (TraceWithCallStack FrontendTrace)
  -> ResolvedBindingSpec
  -> RootHeader
  -> Predicate
  -> CXTranslationUnit -> IO Ext.TranslationUnit
processTranslationUnit tracer extSpec rootHeader predicate unit = do
    (includeGraph, isMainFile) <- processIncludes rootHeader unit
    afterParse <-
      parseDecls
        (contramap (fmap FrontendParse) tracer)
        rootHeader
        predicate
        includeGraph
        isMainFile
        unit

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

    forM_ macroErrors       $ traceWithCallStack tracer . FrontendMacro
    forM_ bindingSpecErrors $ traceWithCallStack tracer . FrontendBindingSpec
    forM_ mangleErrors      $ traceWithCallStack tracer . FrontendNameMangler

    return $ finalize afterNameMangler

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

-- | Trace messages from the frontend
--
-- Most passes in the frontend have their own set of trace messages.
data FrontendTrace =
    FrontendParse ParseTrace
  | FrontendMacro MacroError
  | FrontendBindingSpec BindingSpecError
  | FrontendNameMangler MangleError

instance PrettyTrace FrontendTrace where
  prettyTrace = \case
    FrontendParse       x -> prettyTrace x
    FrontendMacro       x -> prettyTrace x
    FrontendBindingSpec x -> show x -- TODO
    FrontendNameMangler x -> show x -- TODO

instance HasDefaultLogLevel FrontendTrace where
  getDefaultLogLevel = \case
    FrontendParse       x -> getDefaultLogLevel x
    FrontendMacro       x -> getDefaultLogLevel x
    FrontendBindingSpec _ -> Error
    FrontendNameMangler _ -> Error

instance HasSource FrontendTrace where
  getSource = \case
    FrontendParse x -> getSource x
    _otherwise      -> HsBindgen
