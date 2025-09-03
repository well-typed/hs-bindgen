module HsBindgen.Frontend
  ( frontend
  , FrontendArtefact (..)
  , FrontendMsg(..)
  ) where

import Clang.Enum.Bitfield
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Boot
import HsBindgen.Clang
import HsBindgen.Config
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.AST.Finalize
import HsBindgen.Frontend.AST.Internal hiding (Type)
import HsBindgen.Frontend.Pass hiding (Config)
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
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer

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
  -> FrontendConfig
  -> BootArtefact
  -> IO FrontendArtefact
frontend tracer FrontendConfig{..} BootArtefact{..} = do
    -- Frontend: Impure parse pass
    (afterParse, isMainHeader, isInMainHeaderDir) <- fmap (fromMaybe emptyParseResult) $
      withClang (contramap FrontendClang tracer) setup $ \unit -> Just <$> do
        (includeGraph, isMainHeader, isInMainHeaderDir, getMainHeader) <-
          processIncludes unit
        reifiedUnit <- parseDecls
          (contramap FrontendParse tracer)
          rootHeader
          frontendParsePredicate
          includeGraph
          isMainHeader
          isInMainHeaderDir
          getMainHeader
          unit
        pure (reifiedUnit, isMainHeader, isInMainHeaderDir)

    -- Frontend: Pure passes.
    let (afterSort, msgsSort) =
          sortDecls afterParse
        (afterHandleMacros, msgsHandleMacros) =
          handleMacros afterSort
        (afterNameAnon, msgsNameAnon) =
          nameAnon afterHandleMacros
        (afterResolveBindingSpec, msgsResolveBindingSpecs) =
          resolveBindingSpec bootExternalBindingSpec bootPrescriptiveBindingSpec afterNameAnon
        (afterSelect, msgsParseDelayed, msgsSelect) =
          selectDecls isMainHeader isInMainHeaderDir selectConfig afterResolveBindingSpec
        (afterHandleTypedefs, msgsHandleTypedefs) =
          handleTypedefs afterSelect
        (afterMangleNames, msgsMangleNames) =
          mangleNames afterHandleTypedefs

    -- TODO https://github.com/well-typed/hs-bindgen/issues/967: By emitting
    -- all traces in one place, we lose the callstack and timestamp
    -- information of the individual traces.

    -- TODO: Emitting traces forces all passes.
    forM_ msgsParseDelayed        $ traceWith tracer . FrontendParse
    forM_ msgsSort                $ traceWith tracer . FrontendSort
    forM_ msgsHandleMacros        $ traceWith tracer . FrontendHandleMacros
    forM_ msgsNameAnon            $ traceWith tracer . FrontendNameAnon
    forM_ msgsResolveBindingSpecs $ traceWith tracer . FrontendResolveBindingSpecs
    forM_ msgsSelect              $ traceWith tracer . FrontendSelect
    forM_ msgsHandleTypedefs      $ traceWith tracer . FrontendHandleTypedefs
    forM_ msgsMangleNames         $ traceWith tracer . FrontendMangleNames

    let -- Unit.
        cTranslationUnit :: C.TranslationUnit
        cTranslationUnit = finalize afterMangleNames
        -- Graphs.
        frontendIncludeGraph :: IncludeGraph.IncludeGraph
        frontendIncludeGraph = unitIncludeGraph afterParse
        frontendIndex        :: DeclIndex.DeclIndex
        frontendIndex        = declIndex $ unitAnn afterSort
        frontendUseDeclGraph :: UseDeclGraph.UseDeclGraph
        frontendUseDeclGraph = declUseDecl $ unitAnn afterSort
        frontendDeclUseGraph :: DeclUseGraph.DeclUseGraph
        frontendDeclUseGraph = declDeclUse $ unitAnn afterSort
        -- Declarations.
        frontendCDecls :: [C.Decl]
        frontendCDecls = C.unitDecls cTranslationUnit
        -- Dependencies.
        frontendDependencies :: [SourcePath]
        frontendDependencies = C.unitDeps cTranslationUnit

    pure FrontendArtefact{..}
  where
    rootHeader :: RootHeader
    rootHeader = fromMainFiles bootHashIncludeArgs

    setup :: ClangSetup
    setup = (defaultClangSetup bootClangArgs $
              ClangInputMemory hFilePath hContent) {
                clangFlags = bitfieldEnum [
                    CXTranslationUnit_DetailedPreprocessingRecord
                  , CXTranslationUnit_IncludeAttributedTypes
                  , CXTranslationUnit_VisitImplicitAttributes
                  ]
              }

    hFilePath :: FilePath
    hFilePath = getSourcePath name

    hContent :: String
    hContent = content rootHeader

    selectConfig :: SelectConfig
    selectConfig =
      SelectConfig frontendProgramSlicing frontendSelectPredicate

    emptyTranslationUnit :: TranslationUnit Parse
    emptyTranslationUnit = TranslationUnit {
        unitDecls = []
      , unitIncludeGraph = IncludeGraph.empty
      , unitAnn          = emptyParseDeclMeta
      }

    emptyParseResult :: (TranslationUnit Parse, IsMainHeader, IsInMainHeaderDir)
    emptyParseResult = (emptyTranslationUnit, const False, const False)

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

data FrontendArtefact = FrontendArtefact {
    frontendIncludeGraph    :: IncludeGraph.IncludeGraph
  , frontendIndex           :: DeclIndex.DeclIndex
  , frontendUseDeclGraph    :: UseDeclGraph.UseDeclGraph
  , frontendDeclUseGraph    :: DeclUseGraph.DeclUseGraph
  , frontendCDecls          :: [C.Decl]
  , frontendDependencies    :: [SourcePath]
  }

{-------------------------------------------------------------------------------
  Trace
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
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
