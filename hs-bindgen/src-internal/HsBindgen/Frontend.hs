-- | Main entry point into the clang AST folding code
--
-- Intended for unqualified import.
module HsBindgen.Frontend (
    processTranslationUnit
  , FrontendMsg(..)
  ) where

import Control.Monad

import Clang.LowLevel.Core
import HsBindgen.BindingSpec (ResolvedBindingSpec)
import HsBindgen.C.Predicate (Predicate (SelectAll))
import HsBindgen.Frontend.AST.External qualified as Ext
import HsBindgen.Frontend.AST.Finalize
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.HandleTypedefs
import HsBindgen.Frontend.Pass.MangleNames
import HsBindgen.Frontend.Pass.NameAnon
import HsBindgen.Frontend.Pass.Parse (parseDecls)
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec
import HsBindgen.Frontend.Pass.Slice
import HsBindgen.Frontend.Pass.Sort
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint (showToCtxDoc)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Frontend.
--
-- Overview of passes (see documentation of 'HsBindgen.Frontend.Pass.IsPass'):
--
-- 1. 'Parse'
-- 2. 'Sort'
-- 3. 'Slice'
-- 4. 'HandleMacros'
-- 5. 'NameAnon'
-- 6. 'ResolveBindingSpec'
-- 7. 'HandleTypedefs'
-- 8. 'MangleNames'
--
-- Although the passes and their order are subject to change, we have to honor
-- various constraints:
--
-- - 'Sort' must come before the following passes because we need to process
--   declarations before their uses.
--
-- - 'HandleMacros': The macro parser needs to know which things are in scope
--   (other macros as well as typedefs), so we must process declarations in the
--   right order; that is, 'handleMacros' must be done after sorting the
--   declarations.
--
--   In principle it could run before or after renaming: macros can neither
--   refer to nor introduce new anonymous declarations, so the relative ordering
--   of these two passes does not really matter. However, as part of renaming we
--   replace typedefs around anonymous structs by named structs:
--
--   > typedef struct { .. fields .. } foo;
--
--   On the C side however @foo@ must be referred to as @foo@, not @struct foo@;
--   to avoid confusion, it is therefore cleaner to run macro parsing and
--   declaration reparsing /prior/ to this transformation.
--
-- - 'NameAnon' must come before 'ResolveBindingSpec' because it enables users
--   to configure anonymous types using our generated names for them.
--
-- - 'ResolveBindingSpec' must come before 'HandleTypedefs' to enable users to
--   configure if a specific typedef is squashed for not.
--
-- - 'MangleNames': Name mangling depends on information from the binding spec,
--   and must therefore happen after 'ResolveBindingSpec'. It could be put into
--   the 'Hs' phase, but we have to draw the line somewhere.
processTranslationUnit ::
     Tracer IO FrontendMsg
  -> ResolvedBindingSpec -- ^ External binding specification
  -> ResolvedBindingSpec -- ^ Prescriptive binding specification
  -> RootHeader
  -> Predicate
  -> ProgramSlicing
  -> CXTranslationUnit -> IO Ext.TranslationUnit
processTranslationUnit
  tracer
  extSpec
  pSpec
  rootHeader
  selectionPredicate
  programSlicing
  unit = do

    (includeGraph, isMainFile, getMainHeader) <- processIncludes rootHeader unit
    let selectionPredicateParse = case programSlicing of
          EnableProgramSlicing  -> SelectAll
          DisableProgramSlicing -> selectionPredicate

    afterParse <-
      parseDecls
        (contramap FrontendParse tracer)
        rootHeader
        selectionPredicateParse
        includeGraph
        isMainFile
        getMainHeader
        unit

    -- writeFile "includegraph.mermaid" $ IncludeGraph.dumpMermaid includeGraph

    let (afterSort, sortErrors) =
          sortDecls afterParse
        (afterSlice, sliceErrors) =
          sliceDecls programSlicing selectionPredicate isMainFile afterSort
        (afterHandleMacros, macroErrors) =
          handleMacros afterSlice
        afterNameAnon =
          nameAnon afterHandleMacros
        (afterResolveBindingSpec, bindingSpecErrors) =
          resolveBindingSpec pSpec extSpec afterNameAnon
        afterHandleTypedefs =
          handleTypedefs afterResolveBindingSpec
        (afterMangleNames, mangleErrors) =
          mangleNames afterHandleTypedefs

    -- writeFile "usedecl.mermaid" $
    --   UseDecl.dumpMermaid (Int.unitAnn afterSort)

    forM_ sortErrors        $ traceWith tracer . FrontendSort
    forM_ sliceErrors       $ traceWith tracer . FrontendSlice
    forM_ macroErrors       $ traceWith tracer . FrontendHandleMacros
    forM_ bindingSpecErrors $ traceWith tracer . FrontendResolveBindingSpecs
    forM_ mangleErrors      $ traceWith tracer . FrontendMangleNames

    return $ finalize afterMangleNames

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

-- | Trace messages from the frontend
--
-- Most passes in the frontend have their own set of trace messages.
data FrontendMsg =
    FrontendParse ParseMsg
  | FrontendSort SortMsg
  | FrontendSlice SliceMsg
  | FrontendHandleMacros HandleMacrosMsg
  | FrontendResolveBindingSpecs ResolveBindingSpecsMsg
  | FrontendMangleNames MangleNamesMsg
  deriving stock (Show, Eq)

instance PrettyForTrace FrontendMsg where
  prettyForTrace = \case
    FrontendParse               x -> prettyForTrace x
    FrontendSort                x -> prettyForTrace x
    FrontendSlice               x -> prettyForTrace x
    FrontendHandleMacros        x -> prettyForTrace x
    FrontendResolveBindingSpecs x -> showToCtxDoc x -- TODO
    FrontendMangleNames         x -> prettyForTrace x

instance HasDefaultLogLevel FrontendMsg where
  getDefaultLogLevel = \case
    FrontendParse               x -> getDefaultLogLevel x
    FrontendSort                x -> getDefaultLogLevel x
    FrontendSlice               x -> getDefaultLogLevel x
    FrontendHandleMacros        x -> getDefaultLogLevel x
    FrontendResolveBindingSpecs _ -> Error
    FrontendMangleNames         x -> getDefaultLogLevel x

instance HasSource FrontendMsg where
  getSource = \case
    FrontendParse x -> getSource x
    _otherwise      -> HsBindgen
