-- | Main entry point for using @hs-bindgen@-as-a-library
--
-- This is split into three phases:
--
-- * Input preparation (e.g. parsing of C headers)
-- * Translation from C to Haskell
-- * Output processing (e.g. writing Haskell files)
--
-- The bulk of the work happens in translation (which is a pure function).
--
-- Intended for unqualified import.
--
-- NOTE: Client code should /NOT/ have to import from @hs-bindgen-libclang@;
-- that library should be considered internal to @hs-bindgen@.
module HsBindgen.Lib (
    -- * Prepare input
    CHeader    -- opaque
  , C.Skipped  -- opaque
  , Predicate(..)
  , parseCHeader

    -- ** Clang arguments
  , ClangArgs(..)
  , CStandard(..)
  , defaultClangArgs

    -- * Translation
  , HsModuleOpts(..)
  , HsModule -- opaque
  , genModule
  , genDecls

    -- * Process output
  , HsRenderOpts(..)
  , prettyHs
  , prettyC

    -- * Common pipelines
  , Preprocess(..)
  , preprocess

    -- * Debugging
  , getTargetTriple
  , genHaskell

    -- * Logging
  , Tracer
  , nullTracer
  , contramap
  , PrettyLogMsg(..)
  , mkTracer
  , mkTracerIO
  , traceThrow
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Haskell.Exts qualified as E
import Language.Haskell.TH qualified as TH
import Text.Show.Pretty qualified as Pretty

import HsBindgen.Backend.HsSrcExts (Ann)
import HsBindgen.Backend.HsSrcExts.Render (HsRenderOpts(..))
import HsBindgen.Backend.HsSrcExts.Render qualified as Backend.E
import HsBindgen.Backend.HsSrcExts.Translation (HsModuleOpts(..))
import HsBindgen.Backend.HsSrcExts.Translation qualified as Backend.E
import HsBindgen.Backend.TH.Translation qualified as Backend.TH
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Fold qualified as C
import HsBindgen.C.Parser qualified as C
import HsBindgen.C.Predicate (Predicate(..))
import HsBindgen.Clang.Args
import HsBindgen.Clang.Util.Diagnostics qualified as C (Diagnostic)
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Translation.LowLevel qualified as LowLevel
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Type aliases

  These newtypes ensure that these types are opaque in client code. The precise
  nature of the C AST and Haskell AST is not part of the public API.
-------------------------------------------------------------------------------}

newtype CHeader = WrapCHeader {
      unwrapCHeader :: C.Header
    }
  deriving (Eq, Generic)

newtype HsModule = WrapHsModule {
      unwrapHsModule :: E.Module Ann
    }

{-------------------------------------------------------------------------------
  Prepare input

  TODO: <https://github.com/well-typed/hs-bindgen/issues/10>
  This needs to include parameters for cross compilation.

  TODO: <https://github.com/well-typed/hs-bindgen/issues/71>
  This needs to have fields with paths, preprocessor defines, etc.

  TODO: <https://github.com/well-typed/hs-bindgen/issues/75>
  Support multiple C headers.
-------------------------------------------------------------------------------}

-- | Parse C header
parseCHeader ::
     Tracer IO C.Diagnostic
  -> Tracer IO C.Skipped
  -> Predicate
  -> ClangArgs
  -> FilePath -> IO CHeader
parseCHeader traceWarnings traceSkipped p args fp =
    fmap (WrapCHeader . C.Header) $
      C.withTranslationUnit traceWarnings args fp $ \unit -> do
        (decls, _finalDeclState) <-
          C.foldTranslationUnitWith
            unit
            (C.runFoldState C.initDeclState)
            (C.foldDecls traceSkipped p unit)
        return decls

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

genModule :: HsModuleOpts -> CHeader -> HsModule
genModule opts = WrapHsModule . Backend.E.translate opts . unwrapCHeader

genDecls :: TH.Quote q => CHeader -> q [TH.Dec]
genDecls = Backend.TH.translateC . unwrapCHeader

{-------------------------------------------------------------------------------
  Processing output

  TODO: <https://github.com/well-typed/hs-bindgen/issues/75>
  We might want to support generating multiple Haskell modules.
-------------------------------------------------------------------------------}

prettyC :: CHeader -> IO ()
prettyC = Pretty.dumpIO . unwrapCHeader

prettyHs :: HsRenderOpts -> Maybe FilePath -> HsModule -> IO ()
prettyHs opts fp = Backend.E.renderIO opts fp . unwrapHsModule

{-------------------------------------------------------------------------------
  Common pipelines
-------------------------------------------------------------------------------}

data Preprocess = Preprocess {
      -- | Tracer for warnings from @libclang@
      preprocessTraceWarnings :: Tracer IO C.Diagnostic

      -- | Tracer for /our/ C parser
    , preprocessTraceSkipped :: Tracer IO C.Skipped

      -- | Select definitions
    , preprocessPredicate :: Predicate

      -- | @libclang@ options
    , preprocessClangArgs :: ClangArgs

      -- | Path to the C header
    , preprocessInputPath :: FilePath

      -- | Options for the Haskell module generation
    , preprocessModuleOpts :: HsModuleOpts

      -- | Options for rendering generated Haskell
    , preprocessRenderOpts :: HsRenderOpts

      -- | Name of the Haskell file (none for @stdout@)
    , preprocessOutputPath :: Maybe FilePath
    }

preprocess :: Preprocess -> IO ()
preprocess prep = do
    modl <- genModule (preprocessModuleOpts prep) <$>
              parseCHeader
                (preprocessTraceWarnings prep)
                (preprocessTraceSkipped  prep)
                (preprocessPredicate     prep)
                (preprocessClangArgs     prep)
                (preprocessInputPath     prep)
    prettyHs (preprocessRenderOpts prep) (preprocessOutputPath prep) modl

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Return the target triple for translation unit
getTargetTriple ::
     Tracer IO C.Diagnostic
  -> ClangArgs
  -> FilePath
  -> IO Text
getTargetTriple tracer args fp =
    C.withTranslationUnit tracer args fp $
      C.getTranslationUnitTargetTriple

-- | Generate our internal Haskell representation of the translated C header
genHaskell :: CHeader -> [Hs.Decl f]
genHaskell = LowLevel.generateDeclarations . unwrapCHeader


