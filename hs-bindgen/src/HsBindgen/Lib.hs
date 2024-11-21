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
    CXTranslationUnit -- opaque
  , Diagnostic(..)
  , FixIt(..)
  , withTranslationUnit

    -- ** Clang arguments
  , ClangArgs(..)
  , CStandard(..)
  , defaultClangArgs

    -- ** Select parts of the AST
  , Predicate(..)
  , C.Skipped -- opaque

    -- ** Process the C input
  , CHeader -- opaque
  , parseCHeader

    -- ** Development/debugging
  , getTargetTriple
  , bootstrapPrelude

    -- * Translation
  , HsModuleOpts(..)
  , HsModule(..) -- opaque, TODO: but needed to be unwrapped by rendering functions
  , genModule
  , genTH

    -- ** Development/debugging
  , genHsDecls

    -- * Process output
  , HsRenderOpts(..)
  , prettyHs
  , prettyC

    -- * Logging
  , Tracer

    -- ** Construction
  , nullTracer
  , mkTracerIO
  , mkTracerQ
  , mkTracer

    -- ** Usage
  , contramap
  , PrettyLogMsg(..)

  -- * All-in-one functions
  , templateHaskell
  , preprocessor
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Haskell.TH qualified as TH
import Text.Show.Pretty qualified as Pretty

import HsBindgen.Backend.PP.Render (HsRenderOpts(..))
import HsBindgen.Backend.PP.Render qualified as Backend.PP
import HsBindgen.Backend.PP.Translation (HsModuleOpts(..))
import HsBindgen.Backend.PP.Translation qualified as Backend.PP
import HsBindgen.Backend.TH.Translation qualified as Backend.TH
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Fold qualified as C
import HsBindgen.C.Parser qualified as C
import HsBindgen.C.Predicate (Predicate(..))
import HsBindgen.Clang.Args
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as LowLevel
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
      unwrapHsModule :: Backend.PP.HsModule
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

-- | Open C header
--
-- See section "Process the C input" for example functions you can pass as
-- arguments; the most important being 'parseCHeader'.
withTranslationUnit ::
     Tracer IO  Diagnostic       -- ^ Tracer for warnings from @libclang@
  -> ClangArgs                   -- ^ @libclang@ arguments
  -> FilePath                    -- ^ Input path
  -> (CXTranslationUnit -> IO r)
  -> IO r
withTranslationUnit = C.withTranslationUnit

parseCHeader ::
     Tracer IO C.Skipped
  -> Predicate
  -> CXTranslationUnit
  -> IO CHeader
parseCHeader traceSkipped p unit = do
    (decls, _finalDeclState) <-
      C.foldTranslationUnitWith
        unit
        (C.runFoldState C.initDeclState)
        (C.foldDecls traceSkipped p unit)
    return $ WrapCHeader (C.Header decls)

bootstrapPrelude ::
     Tracer IO String   -- ^ Warnings
  -> CXTranslationUnit
  -> IO [C.PreludeEntry]
bootstrapPrelude tracer unit = do
    cursor <- clang_getTranslationUnitCursor unit
    C.runFoldIdentity $
      HighLevel.clang_visitChildren cursor $ C.foldPrelude tracer' unit
  where
    -- We could take a tracer for 'C.GenPreludeMsg', but there is only a point
    -- in doing so if we then also export that type.
    tracer' :: Tracer IO C.GenPreludeMsg
    tracer' = contramap prettyLogMsg tracer

-- | Return the target triple for translation unit
getTargetTriple :: CXTranslationUnit -> IO Text
getTargetTriple = C.getTranslationUnitTargetTriple

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

genModule :: HsModuleOpts -> CHeader -> HsModule
genModule opts = WrapHsModule . Backend.PP.translate opts . unwrapCHeader

genTH :: TH.Quote q => CHeader -> q [TH.Dec]
genTH = Backend.TH.translateC . unwrapCHeader

genHsDecls :: CHeader -> [Hs.Decl Hs.Placeholder]
genHsDecls = LowLevel.generateDeclarations . unwrapCHeader

{-------------------------------------------------------------------------------
  Processing output

  TODO: <https://github.com/well-typed/hs-bindgen/issues/75>
  We might want to support generating multiple Haskell modules.
-------------------------------------------------------------------------------}

prettyC :: CHeader -> IO ()
prettyC = Pretty.dumpIO . unwrapCHeader

prettyHs :: HsRenderOpts -> Maybe FilePath -> HsModule -> IO ()
prettyHs opts fp = Backend.PP.renderIO opts fp . unwrapHsModule

{-------------------------------------------------------------------------------
  All in one
-------------------------------------------------------------------------------}

templateHaskell :: FilePath -> TH.Q [TH.Dec]
templateHaskell fp = do
    cheader <- TH.runIO $ withTranslationUnit nullTracer defaultClangArgs fp $
      parseCHeader nullTracer SelectFromMainFile
    genTH cheader


preprocessor :: FilePath -> IO String
preprocessor fp = do
    cheader <- withTranslationUnit nullTracer defaultClangArgs fp $
      parseCHeader nullTracer SelectFromMainFile
    return $ Backend.PP.render renderOpts $ unwrapHsModule $ genModule moduleOpts cheader
  where
    moduleOpts :: HsModuleOpts
    moduleOpts = HsModuleOpts
      { hsModuleOptsName = "Example"
      }

    renderOpts :: HsRenderOpts
    renderOpts = HsRenderOpts
      { hsLineLength = 120
      }
