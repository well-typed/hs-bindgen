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
import HsBindgen.Frontend.Pass.HandleTypedefs
import HsBindgen.Frontend.Pass.MangleNames
import HsBindgen.Frontend.Pass.NameAnon
import HsBindgen.Frontend.Pass.Parse (parseDecls)
import HsBindgen.Frontend.Pass.Parse.Monad
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
    (includeGraph, isMainFile, getMainHeader) <- processIncludes rootHeader unit
    afterParse <-
      parseDecls
        (contramap (fmap FrontendParse) tracer)
        rootHeader
        predicate
        includeGraph
        isMainFile
        getMainHeader
        unit

    -- writeFile "includegraph.mermaid" $ IncludeGraph.dumpMermaid includeGraph

    -- TODO receive configuration binding specifications via argument
    let confSpec :: ResolvedBindingSpec
        confSpec = BindingSpec.empty

    let (afterSort, sortErrors) =
          sortDecls afterParse
        (afterHandleMacros, macroErrors) =
          handleMacros afterSort
        afterNameAnon =
          nameAnon afterHandleMacros
        (afterResolveBindingSpec, bindingSpecErrors) =
          resolveBindingSpec confSpec extSpec afterNameAnon
        afterHandleTypedefs =
          handleTypedefs afterResolveBindingSpec
        (afterMangleNames, mangleErrors) =
          mangleNames afterHandleTypedefs

    -- writeFile "usedecl.mermaid" $
    --   UseDecl.dumpMermaid (Int.unitAnn afterSort)

    forM_ sortErrors        $ traceWithCallStack tracer . FrontendSort
    forM_ macroErrors       $ traceWithCallStack tracer . FrontendMacro
    forM_ bindingSpecErrors $ traceWithCallStack tracer . FrontendBindingSpec
    forM_ mangleErrors      $ traceWithCallStack tracer . FrontendNameMangler

    return $ finalize afterMangleNames

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

-- | Trace messages from the frontend
--
-- Most passes in the frontend have their own set of trace messages.
data FrontendTrace =
    FrontendSort SortError
  | FrontendParse ParseTrace
  | FrontendMacro MacroError
  | FrontendBindingSpec BindingSpecError
  | FrontendNameMangler MangleError
  deriving stock (Show, Eq)

instance PrettyTrace FrontendTrace where
  prettyTrace = \case
    FrontendSort        x -> prettyTrace x
    FrontendParse       x -> prettyTrace x
    FrontendMacro       x -> prettyTrace x
    FrontendBindingSpec x -> show x -- TODO
    FrontendNameMangler x -> prettyTrace x

instance HasDefaultLogLevel FrontendTrace where
  getDefaultLogLevel = \case
    FrontendSort        x -> getDefaultLogLevel x
    FrontendParse       x -> getDefaultLogLevel x
    FrontendMacro       x -> getDefaultLogLevel x
    FrontendBindingSpec _ -> Error
    FrontendNameMangler x -> getDefaultLogLevel x

instance HasSource FrontendTrace where
  getSource = \case
    FrontendParse x -> getSource x
    _otherwise      -> HsBindgen
