module HsBindgen.Pipeline (
    -- * Options
    Opts(..)
  , defaultOpts
  , PPOpts(..)
  , defaultPPOpts

    -- * Translation pipeline components
  , genHsDecls
  , genSHsDecls
  , genModule
  , genPP
  , genPPString
  , genTH
  , genExtensions

    -- * Preprocessor API
  , parseCHeader
  , preprocessPure
  , preprocessIO

    -- * Template Haskell API
  , genBindings
  , genBindings'

    -- * Test generation
  , genTests
  ) where

import Data.Set qualified as Set
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH (addDependentFile)

import HsBindgen.Backend.Extensions
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
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.Paths
import HsBindgen.ExtBindings
import HsBindgen.GenTests qualified as GenTests
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.NameMangler qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Imports
import HsBindgen.Resolve
import HsBindgen.SHs.AST qualified as SHs
import HsBindgen.SHs.Translation qualified as SHs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Options for both the preprocessor and TH APIs
data Opts = Opts {
      optsClangArgs   :: ClangArgs
    , optsExtBindings :: ExtBindings
    , optsTranslation :: Hs.TranslationOpts
    , optsNameMangler :: Hs.NameMangler
    , optsPredicate   :: Predicate
    , optsDiagTracer  :: Tracer IO String
    , optsSkipTracer  :: Tracer IO String
    }

defaultOpts :: Opts
defaultOpts = Opts {
      optsClangArgs   = defaultClangArgs
    , optsExtBindings = emptyExtBindings
    , optsTranslation = Hs.defaultTranslationOpts
    , optsNameMangler = Hs.defaultNameMangler
    , optsPredicate   = SelectFromMainFile
    , optsDiagTracer  = nullTracer
    , optsSkipTracer  = nullTracer
    }

-- | Additional options for the preprocessor API
data PPOpts = PPOpts {
      ppOptsModule :: HsModuleOpts -- ^ Default module name: @Generated@
    , ppOptsRender :: HsRenderOpts -- ^ Default line length: 120
    }

defaultPPOpts :: PPOpts
defaultPPOpts = PPOpts {
      ppOptsModule = HsModuleOpts { hsModuleOptsName = "Generated" }
    , ppOptsRender = HsRenderOpts { hsLineLength = 120 }
    }

{-------------------------------------------------------------------------------
  Translation pipeline components
-------------------------------------------------------------------------------}

genHsDecls :: Opts -> CHeaderIncludePath -> C.Header -> [Hs.Decl]
genHsDecls Opts{..} headerIncludePath =
    Hs.generateDeclarations headerIncludePath optsTranslation optsNameMangler

genSHsDecls :: [Hs.Decl] -> [SHs.SDecl]
genSHsDecls = map SHs.translateDecl

genModule :: PPOpts -> [SHs.SDecl] -> Backend.PP.HsModule
genModule PPOpts{..} = Backend.PP.translateModule ppOptsModule

genPP :: PPOpts -> Maybe FilePath -> Backend.PP.HsModule -> IO ()
genPP PPOpts{..} fp = Backend.PP.renderIO ppOptsRender fp

genPPString :: PPOpts -> Backend.PP.HsModule -> String
genPPString PPOpts{..} = Backend.PP.render ppOptsRender

genTH :: TH.Quote q => [SHs.SDecl] -> q [TH.Dec]
genTH = fmap concat . traverse Backend.TH.mkDecl

genExtensions :: [SHs.SDecl] -> Set TH.Extension
genExtensions = foldMap requiredExtensions

{-------------------------------------------------------------------------------
  Preprocessor API
-------------------------------------------------------------------------------}

-- | Parse a C header
parseCHeader :: Opts -> CHeaderIncludePath -> IO ([SourcePath], C.Header)
parseCHeader Opts{..} headerIncludePath = do
    src <- resolveHeader optsClangArgs headerIncludePath
    C.withTranslationUnit diagTracer optsClangArgs src $
      C.parseCHeader skipTracer optsExtBindings optsPredicate
  where
    diagTracer :: Tracer IO Diagnostic
    diagTracer = contramap show optsDiagTracer

    skipTracer :: Tracer IO C.Skipped
    skipTracer = contramap prettyLogMsg optsSkipTracer

-- | Generate bindings for the given C header
preprocessPure ::
     Opts
  -> PPOpts
  -> CHeaderIncludePath
  -> C.Header
  -> String
preprocessPure opts ppOpts headerIncludePath =
    genPPString ppOpts
      . genModule ppOpts
      . genSHsDecls
      . genHsDecls opts headerIncludePath

-- | Generate bindings for the given C header
preprocessIO ::
     Opts
  -> PPOpts
  -> CHeaderIncludePath
  -> Maybe FilePath     -- ^ Output file or 'Nothing' for @STDOUT@
  -> C.Header
  -> IO ()
preprocessIO opts ppOpts headerIncludePath fp =
    genPP ppOpts fp
      . genModule ppOpts
      . genSHsDecls
      . genHsDecls opts headerIncludePath

{-------------------------------------------------------------------------------
  Template Haskell API
-------------------------------------------------------------------------------}

-- | Generate bindings for the given C header
genBindings ::
     Opts
  -> FilePath -- ^ Input header, as written in C @#include@
  -> TH.Q [TH.Dec]
genBindings opts fp = do
    headerIncludePath <- either (TH.runIO . throwIO) return $
      parseCHeaderIncludePath fp
    (depPaths, cheader) <- TH.runIO $ parseCHeader opts headerIncludePath

    -- record dependencies, including transitively included headers
    mapM_ (TH.addDependentFile . getSourcePath) depPaths

    let sdecls = genSHsDecls $ genHsDecls opts headerIncludePath cheader

    -- extensions checks.
    -- Potential TODO: we could also check which enabled extension may interfere with the generated code. (e.g. Strict/Data)
    enabledExts <- Set.fromList <$> TH.extsEnabled
    let requiredExts = genExtensions sdecls
        missingExts  = requiredExts `Set.difference` enabledExts
    unless (null missingExts) $ do
      TH.reportError $ "Missing LANGUAGE extensions: "
        ++ unwords (map show (toList missingExts))

    -- generate TH declarations
    genTH sdecls

-- | Generate bindings for the given C header (simple)
--
-- This function uses default Clang arguments but allows you to add directories
-- to the include search path.  Use 'genBindings' when more configuration is
-- required.
genBindings' ::
     [FilePath] -- ^ Quote include search path directories
  -> FilePath   -- ^ Input header, as written in C @#include@
  -> TH.Q [TH.Dec]
genBindings' quoteIncPathDirs = genBindings defaultOpts {
      optsClangArgs = defaultClangArgs {
          clangQuoteIncludePathDirs  = CIncludePathDir <$> quoteIncPathDirs
        }
    }

{-------------------------------------------------------------------------------
  Test generation
-------------------------------------------------------------------------------}

genTests :: PPOpts -> CHeaderIncludePath -> FilePath -> C.Header -> IO ()
genTests PPOpts{..} headerIncludePath testDir cheader =
    GenTests.genTests
      headerIncludePath
      cheader
      (hsModuleOptsName ppOptsModule)
      (hsLineLength ppOptsRender)
      testDir
