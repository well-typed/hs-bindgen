-- | Process the @clang@ C AST.
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Parser qualified as C
module HsBindgen.C.Parser (
    -- * Parsing
--    ParseCHeadersException(..)
    parseCHeaders
    -- * Debugging/development
  , getTargetTriple
  ) where

import Control.Exception

import Clang.Args
import Clang.Enum.Bitfield
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.BindingSpec (ExternalBindingSpec, PrescriptiveBindingSpec)
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.Clang
import HsBindgen.Errors
import HsBindgen.Frontend (processTranslationUnit)
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Pass.Slice (ProgramSlicing)
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseCHeaders ::
     Tracer IO TraceMsg
  -> ClangArgs
  -> Predicate
  -> ProgramSlicing
  -> ExternalBindingSpec
  -> PrescriptiveBindingSpec
  -> [CHeaderIncludePath]
  -> IO C.TranslationUnit
parseCHeaders tracer args predicate programSlicing extSpec pSpec mainFiles =
    fmap (fromMaybe C.emptyTranslationUnit) $
    withClang (contramap TraceClang tracer) setup $ \unit -> Just <$> do
      processTranslationUnit
        (contramap TraceFrontend tracer)
        extSpec
        pSpec
        rootHeader
        predicate
        programSlicing
        unit
  where
    setup :: ClangSetup
    setup = (defaultClangSetup args $ ClangInputMemory hFilePath hContent) {
          clangFlags = bitfieldEnum [
              CXTranslationUnit_SkipFunctionBodies
            , CXTranslationUnit_DetailedPreprocessingRecord
            , CXTranslationUnit_IncludeAttributedTypes
            , CXTranslationUnit_VisitImplicitAttributes
            ]
        }

    rootHeader :: RootHeader
    rootHeader = RootHeader.fromMainFiles mainFiles

    hFilePath :: FilePath
    hFilePath = getSourcePath RootHeader.name

    hContent :: String
    hContent = RootHeader.content rootHeader

{-------------------------------------------------------------------------------
  Debugging/development
-------------------------------------------------------------------------------}

getTargetTriple :: Tracer IO ClangMsg -> ClangArgs -> IO Text
getTargetTriple tracer args =
    fmap (fromMaybe (panicPure "getTargetTriple failed")) $
    withClang tracer setup $ \unit -> Just <$>
      bracket
        (clang_getTranslationUnitTargetInfo unit)
        clang_TargetInfo_dispose
        clang_TargetInfo_getTriple
  where
    setup :: ClangSetup
    setup = defaultClangSetup args $ ClangInputMemory "hs-bindgen-triple.h" ""
