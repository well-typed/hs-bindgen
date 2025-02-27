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

    -- *** Cross-compilation
  , Target(..)
  , TargetEnv(..)
  , targetTriple

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
  , LowLevel.TranslationOpts(..)
  , LowLevel.defaultTranslationOpts
  , HsModuleOpts(..)
  , HsModule(..) -- opaque, TODO: but needed to be unwrapped by rendering functions
  , genModule
  , genTH
  , genExtensions

    -- ** Development/debugging
  , genHsDecls

    -- * Process output
  , HsRenderOpts(..)
  , prettyHs
  , prettyC

    -- * Test generation
  , genTests

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

  -- * Preprocessor API
  , preprocessor

  -- * Template Haskell API
  , genBindings
  , genBindings'
  ) where

import Data.Set qualified as Set
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH (addDependentFile)
import Text.Show.Pretty qualified as Pretty

import HsBindgen.Backend.Extensions
import HsBindgen.Backend.PP.Render (HsRenderOpts(..))
import HsBindgen.Backend.PP.Render qualified as Backend.PP
import HsBindgen.Backend.PP.Translation (HsModuleOpts(..))
import HsBindgen.Backend.PP.Translation qualified as Backend.PP
import HsBindgen.Backend.TH.Translation qualified as Backend.TH
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Fold qualified as C
import HsBindgen.C.Fold.DeclState qualified as C
import HsBindgen.C.Parser qualified as C
import HsBindgen.C.Predicate (Predicate(..))
import HsBindgen.Clang.Args
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Clang.Paths
import HsBindgen.Clang.Paths.Resolve
import HsBindgen.GenTests qualified as GenTests
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as LowLevel
import HsBindgen.SHs.Translation qualified as SHs
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Type aliases

  These newtypes ensure that these types are opaque in client code. The precise
  nature of the C AST and Haskell AST is not part of the public API.
-------------------------------------------------------------------------------}

newtype CHeader = WrapCHeader {
      unwrapCHeader :: C.Header
    }
  deriving (Generic)

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
     Tracer IO Diagnostic        -- ^ Tracer for warnings from @libclang@
  -> ClangArgs                   -- ^ @libclang@ arguments
  -> SourcePath                  -- ^ Input path
  -> (CXTranslationUnit -> IO r)
  -> IO r
withTranslationUnit = C.withTranslationUnit

parseCHeader ::
     Tracer IO C.Skipped
  -> Predicate
  -> CXTranslationUnit
  -> IO CHeader
parseCHeader traceSkipped p unit = do
    (decls, finalDeclState) <-
      C.foldTranslationUnitWith
        unit
        (C.runFoldState C.initDeclState)
        (C.foldDecls traceSkipped p unit)

    let decls' = [ d | C.TypeDecl _ d <- toList (C.typeDeclarations finalDeclState) ]
    return $ WrapCHeader (C.Header $ decls ++ decls')

bootstrapPrelude ::
     Tracer IO String   -- ^ Messages
  -> Tracer IO String   -- ^ Macros
  -> CXTranslationUnit
  -> IO [C.PreludeEntry]
bootstrapPrelude msgTracer macroTracer unit = do
    cursor <- clang_getTranslationUnitCursor unit
    C.runFoldIdentity . HighLevel.clang_visitChildren cursor $
      C.foldPrelude msgTracer' macroTracer' unit
  where
    -- We could take a tracer for 'C.GenPreludeMsg', but there is only a point
    -- in doing so if we then also export that type.
    msgTracer' :: Tracer IO C.GenPreludeMsg
    msgTracer' = contramap prettyLogMsg msgTracer

    macroTracer' :: Tracer IO (MultiLoc, C.Macro)
    macroTracer' = contramap show macroTracer

-- | Return the target triple for translation unit
getTargetTriple :: CXTranslationUnit -> IO Text
getTargetTriple = C.getTranslationUnitTargetTriple

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

genModule ::
     CHeaderIncludePath
  -> LowLevel.TranslationOpts
  -> HsModuleOpts
  -> CHeader
  -> HsModule
genModule headerIncludePath topts opts =
      WrapHsModule
    . Backend.PP.translateModule opts
    . map SHs.translateDecl
    . genHsDecls headerIncludePath topts

genTH ::
     TH.Quote q
  => CHeaderIncludePath
  -> LowLevel.TranslationOpts
  -> CHeader
  -> q [TH.Dec]
genTH headerIncludePath topts cheader = do
    fmap concat (traverse Backend.TH.mkDecl sdecls)
  where
    sdecls = map SHs.translateDecl (genHsDecls headerIncludePath topts cheader)

genHsDecls ::
     CHeaderIncludePath
  -> LowLevel.TranslationOpts
  -> CHeader
  -> [Hs.Decl]
genHsDecls headerIncludePath topts =
    LowLevel.generateDeclarations headerIncludePath topts . unwrapCHeader

-- | Which extensions will be needed for the generated code.
--
-- Exposed for hs-bindgen internal tests
genExtensions ::
     CHeaderIncludePath
  -> LowLevel.TranslationOpts
  -> CHeader -> Set TH.Extension
genExtensions headerIncludePath topts cheader = do
    foldMap requiredExtensions sdecls
  where
    sdecls = map SHs.translateDecl (genHsDecls headerIncludePath topts cheader)

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
  Test generation
-------------------------------------------------------------------------------}

genTests ::
     CHeaderIncludePath
  -> CHeader
  -> HsModuleOpts
  -> HsRenderOpts
  -> FilePath -- ^ Test suite directory path
  -> IO ()
genTests
  headerIncludePath
  cHeader
  HsModuleOpts{hsModuleOptsName}
  HsRenderOpts{hsLineLength} =
    GenTests.genTests
      headerIncludePath
      (unwrapCHeader cHeader)
      hsModuleOptsName
      hsLineLength

{-------------------------------------------------------------------------------
  Preprocessor API
-------------------------------------------------------------------------------}

preprocessor ::
     [CIncludePathDir]  -- ^ System include search path directories
  -> [CIncludePathDir]  -- ^ Non-system include search path directories
  -> CHeaderIncludePath -- ^ Input header
  -> IO String
preprocessor sysIncPathDirs quoteIncPathDirs headerIncludePath = do
    src <- resolveHeader' args headerIncludePath
    cheader <-
      withTranslationUnit nullTracer args src $
        parseCHeader nullTracer SelectFromMainFile
    return $
      Backend.PP.render renderOpts $
        unwrapHsModule $ genModule headerIncludePath topts moduleOpts cheader
  where
    args :: ClangArgs
    args = defaultClangArgs {
        clangSystemIncludePathDirs = sysIncPathDirs
      , clangQuoteIncludePathDirs  = quoteIncPathDirs
      }

    topts :: LowLevel.TranslationOpts
    topts = LowLevel.defaultTranslationOpts

    moduleOpts :: HsModuleOpts
    moduleOpts = HsModuleOpts
      { hsModuleOptsName = "Example"
      }

    renderOpts :: HsRenderOpts
    renderOpts = HsRenderOpts
      { hsLineLength = 120
      }

{-------------------------------------------------------------------------------
  Template Haskell API
-------------------------------------------------------------------------------}

-- | Generate bindings for the given C header
--
-- TODO: add TranslationOpts argument
genBindings ::
     FilePath -- ^ Input header, as written in C @#include@
  -> ClangArgs
  -> TH.Q [TH.Dec]
genBindings fp args = do
    headerIncludePath <- either fail return $ parseCHeaderIncludePath fp
    src <- TH.runIO $ resolveHeader' args headerIncludePath
    cheader <- TH.runIO $
      withTranslationUnit nullTracer args src $
        parseCHeader nullTracer SelectFromMainFile

    -- record dependencies
    -- TODO: https://github.com/well-typed/hs-bindgen/issues/422
    TH.addDependentFile $ getSourcePath src

    -- extensions checks.
    -- Potential TODO: we could also check which enabled extension may interfere with the generated code. (e.g. Strict/Data)
    enabledExts <- Set.fromList <$> TH.extsEnabled
    let requiredExts = genExtensions headerIncludePath LowLevel.defaultTranslationOpts cheader
    let missingExts = requiredExts `Set.difference` enabledExts
    unless (null missingExts) $ do
      TH.reportError $ "Missing LANGUAGE extensions: " ++ unwords (map show (toList missingExts))

    -- generate TH declarations
    genTH headerIncludePath LowLevel.defaultTranslationOpts cheader

-- | Generate bindings for the given C header
--
-- This function uses default Clang arguments but allows you to add directories
-- to the include search path.  Use 'genBindings' when more configuration is
-- required.
genBindings' ::
     [FilePath] -- ^ Quote include search path directories
  -> FilePath   -- ^ Input header, as written in C @#include@
  -> TH.Q [TH.Dec]
genBindings' quoteIncPathDirs fp = genBindings fp args
  where
    args :: ClangArgs
    args = defaultClangArgs {
        clangQuoteIncludePathDirs  = CIncludePathDir <$> quoteIncPathDirs
      }
