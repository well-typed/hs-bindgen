-- | Main entry point into the clang AST folding code
--
-- Intended for unqualified import.
module HsBindgen.Frontend (
    frontend
  , FrontendMsg(..)
  ) where

import Control.Monad

import Clang.Enum.Bitfield
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.BindingSpec
import HsBindgen.Clang
import HsBindgen.Config
import HsBindgen.Frontend.AST.External (emptyTranslationUnit)
import HsBindgen.Frontend.AST.External qualified as Ext
import HsBindgen.Frontend.AST.Finalize
import HsBindgen.Frontend.Pass (Msg)
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.HandleTypedefs
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass
import HsBindgen.Frontend.Pass.MangleNames
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.NameAnon
import HsBindgen.Frontend.Pass.NameAnon.IsPass
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
import HsBindgen.Frontend.Pass.Select
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.Sort
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Frontend.
--
-- Overview of passes (see documentation of 'HsBindgen.Frontend.Pass.IsPass'):
--
-- 1. 'Parse' (impure; all other passes are pure)
-- 2. 'Sort'
-- 3. 'HandleMacros'
-- 4. 'NameAnon'
-- 5. 'ResolveBindingSpec'
-- 6. 'Select'
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
-- - 'Select' must come after 'ResolveBindingSpec' so that selection is done
--   after declarations have been omitted via prescriptive binding
--   specification.  It must come before 'HandleTypedefs' because selection
--   should be done before renaming to be consistent with 'ResolveBindingSpec'.
--
-- - 'MangleNames': Name mangling depends on information from the binding spec,
--   and must therefore happen after 'ResolveBindingSpec'. It could be put into
--   the 'Hs' phase, but we have to draw the line somewhere.
frontend ::
     Tracer IO FrontendMsg
  -> Config
  -> ExternalBindingSpec
  -> PrescriptiveBindingSpec
  -> [RootHeader.HashIncludeArg]
  -> IO Ext.TranslationUnit
frontend tracer Config{..} extSpec pSpec hashIncludeArgs = do
    -- Impure: parse source code with `libclang` and reify.
    mParseResult <-
      -- TODO: Maybe come up with empty values here, and avoid failing early below.
      withClang (contramap FrontendClang tracer) setup $ \unit -> Just <$> do
        (includeGraph, isMainHeader, isInMainHeaderDir, getMainHeader) <-
          processIncludes rootHeader unit
        reifiedUnit <- parseDecls
          (contramap FrontendParse tracer)
          rootHeader
          configParsePredicate
          includeGraph
          isMainHeader
          isInMainHeaderDir
          getMainHeader
          unit
        pure (reifiedUnit, isMainHeader, isInMainHeaderDir)

    case mParseResult of
      -- Possibly fail early.
      Nothing -> pure emptyTranslationUnit
      -- Pure: other passes.
      Just (afterParse, isMainHeader, isInMainHeaderDir) -> do

        -- writeFile "includegraph.mermaid" $ IncludeGraph.dumpMermaid includeGraph

        let (afterSort, msgsSort) =
              sortDecls afterParse
            (afterHandleMacros, msgsHandleMacros) =
              handleMacros afterSort
            (afterNameAnon, msgsNameAnon) =
              nameAnon afterHandleMacros
            (afterResolveBindingSpec, msgsResolveBindingSpecs) =
              resolveBindingSpec extSpec pSpec afterNameAnon
            (afterSelect, msgsSelect) =
              selectDecls isMainHeader isInMainHeaderDir selectConfig afterResolveBindingSpec
            (afterHandleTypedefs, msgsHandleTypedefs) =
              handleTypedefs afterSelect
            (afterMangleNames, msgsMangleNames) =
              mangleNames afterHandleTypedefs

        -- writeFile "usedecl.mermaid" $
        --   UseDecl.dumpMermaid (Int.unitAnn afterSort)

        -- TODO https://github.com/well-typed/hs-bindgen/issues/967: By emitting
        -- all traces in one place, we lose the callstack and timestamp
        -- information of the individual traces.
        forM_ msgsSort                $ traceWith tracer . FrontendSort
        forM_ msgsHandleMacros        $ traceWith tracer . FrontendHandleMacros
        forM_ msgsNameAnon            $ traceWith tracer . FrontendNameAnon
        forM_ msgsResolveBindingSpecs $ traceWith tracer . FrontendResolveBindingSpecs
        forM_ msgsSelect              $ traceWith tracer . FrontendSelect
        forM_ msgsHandleTypedefs      $ traceWith tracer . FrontendHandleTypedefs
        forM_ msgsMangleNames         $ traceWith tracer . FrontendMangleNames

        return $ finalize afterMangleNames
  where
    rootHeader :: RootHeader
    rootHeader = RootHeader.fromMainFiles hashIncludeArgs

    setup :: ClangSetup
    setup = (defaultClangSetup configClangArgs $ ClangInputMemory hFilePath hContent) {
          clangFlags = bitfieldEnum [
              CXTranslationUnit_SkipFunctionBodies
            , CXTranslationUnit_DetailedPreprocessingRecord
            , CXTranslationUnit_IncludeAttributedTypes
            , CXTranslationUnit_VisitImplicitAttributes
            ]
        }

    hFilePath :: FilePath
    hFilePath = getSourcePath RootHeader.name

    hContent :: String
    hContent = RootHeader.content rootHeader

    selectConfig :: SelectConfig
    selectConfig = SelectConfig configProgramSlicing configSelectPredicate

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

-- | Frontend trace messages
--
-- Most passes in the frontend have their own set of trace messages.
data FrontendMsg =
    FrontendClang ClangMsg
  | FrontendParse (Msg Parse)
  | FrontendSort (Msg Sort)
  | FrontendHandleMacros (Msg HandleMacros)
  | FrontendNameAnon (Msg NameAnon)
  | FrontendResolveBindingSpecs (Msg ResolveBindingSpec)
  | FrontendSelect (Msg Select)
  | FrontendHandleTypedefs (Msg HandleTypedefs)
  | FrontendMangleNames (Msg MangleNames)
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (PrettyForTrace, HasDefaultLogLevel, HasSource)
