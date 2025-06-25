{-# LANGUAGE CPP #-}

module HsBindgen.Pipeline (
    -- * Options
    Opts(..)
  , PPOpts(..)

    -- * Translation pipeline components
  , parseCHeaders
  , genHsDecls
  , genSHsDecls
  , genModule
  , genPP
  , genPPString
  , genTH
  , genExtensions

    -- * Preprocessor API
  , translateCHeaders
  , preprocessPure
  , preprocessIO

    -- * Template Haskell API
  , QuoteIncludeDir (..)
  , HashIncludeOpts (..)
  , hashInclude
  , hashInclude'
  , hashIncludeWith
  , genBindingsFromCHeader

    -- * External bindings
  , genExtBindings
  , StdlibBindingSpecs (..)
  , loadExtBindings

    -- * Test generation
  , genTests
  ) where

import Control.Monad ((<=<))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH
import System.FilePath ((</>))

import Clang.Args
import Clang.Paths
import HsBindgen.Backend.Extensions
import HsBindgen.Backend.PP.Render (HsRenderOpts (..))
import HsBindgen.Backend.PP.Render qualified as Backend.PP
import HsBindgen.Backend.PP.Translation (HsModuleOpts (..))
import HsBindgen.Backend.PP.Translation qualified as Backend.PP
import HsBindgen.Backend.TH.Translation qualified as Backend.TH
import HsBindgen.BindingSpec (ResolvedBindingSpec, UnresolvedBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen (genBindingSpec)
import HsBindgen.BindingSpec.Stdlib qualified as Stdlib
import HsBindgen.C.Parser qualified as C
import HsBindgen.C.Predicate (Predicate (..))
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Pass.Slice (ProgramSlicing (DisableProgramSlicing))
import HsBindgen.GenTests qualified as GenTests
import HsBindgen.Guasi
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import HsBindgen.ModuleUnique
import HsBindgen.SHs.AST qualified as SHs
import HsBindgen.SHs.Translation qualified as SHs
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

#ifdef MIN_VERSION_th_compat
import Language.Haskell.TH.Syntax.Compat (getPackageRoot)
#else
import Language.Haskell.TH.Syntax (getPackageRoot)
#endif

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Options for both the preprocessor and TH APIs
data Opts = Opts {
      optsClangArgs      :: ClangArgs
    , optsExtBindings    :: ResolvedBindingSpec
    , optsTranslation    :: Hs.TranslationOpts
    , optsPredicate      :: Predicate
    , optsProgramSlicing :: ProgramSlicing
    , optsTracer         :: Tracer IO TraceMsg
    }

instance Default Opts where
  def = Opts {
      optsClangArgs      = def
    , optsExtBindings    = BindingSpec.empty
    , optsTranslation    = def
    , optsPredicate      = SelectFromMainFiles
    , optsProgramSlicing = DisableProgramSlicing
    , optsTracer         = nullTracer
    }

-- | Additional options for the preprocessor API
data PPOpts = PPOpts {
      ppOptsModule :: HsModuleOpts -- ^ Default module name: @Generated@
    , ppOptsRender :: HsRenderOpts -- ^ Default line length: 120
    }

instance Default PPOpts where
  def = PPOpts {
      ppOptsModule = HsModuleOpts { hsModuleOptsName = "Generated" }
    , ppOptsRender = HsRenderOpts { hsLineLength = 120 }
    }

{-------------------------------------------------------------------------------
  Translation pipeline components
-------------------------------------------------------------------------------}

-- | Parse C headers
parseCHeaders ::
      Opts
   -> [CHeaderIncludePath]
   -> IO C.TranslationUnit
parseCHeaders Opts{..} =
    C.parseCHeaders optsTracer optsClangArgs optsPredicate optsProgramSlicing optsExtBindings

-- | Generate @Hs@ declarations
genHsDecls :: ModuleUnique -> Opts -> [C.Decl] -> [Hs.Decl]
genHsDecls mu Opts{..} = Hs.generateDeclarations optsTranslation mu

-- | Generate @SHs@ declarations
genSHsDecls :: [Hs.Decl] -> [SHs.SDecl]
genSHsDecls = SHs.translateDecls

-- | Generate a preprocessor 'Backend.PP.HsModule'
genModule :: PPOpts -> [SHs.SDecl] -> Backend.PP.HsModule
genModule PPOpts{..} = Backend.PP.translateModule ppOptsModule

-- | Generate bindings source code, written to a file or @STDOUT@
genPP :: PPOpts -> Maybe FilePath -> Backend.PP.HsModule -> IO ()
genPP PPOpts{..} fp = Backend.PP.renderIO ppOptsRender fp

-- | Generate bindings source code
genPPString :: PPOpts -> Backend.PP.HsModule -> String
genPPString PPOpts{..} = Backend.PP.render ppOptsRender

-- | Generate Template Haskell declarations
genTH :: Guasi q => [SHs.SDecl] -> q [TH.Dec]
genTH = fmap concat . traverse Backend.TH.mkDecl

-- | Generate set of required extensions
genExtensions :: [SHs.SDecl] -> Set TH.Extension
genExtensions = foldMap requiredExtensions

{-------------------------------------------------------------------------------
  Preprocessor API
-------------------------------------------------------------------------------}

-- | Parse a C header and generate @Hs@ declarations
translateCHeaders :: ModuleUnique -> Opts -> [CHeaderIncludePath] -> IO [Hs.Decl]
translateCHeaders mu opts headerIncludePaths = do
    C.TranslationUnit{unitDecls} <- parseCHeaders opts headerIncludePaths
    return $ genHsDecls mu opts unitDecls

-- | Generate bindings for the given C header
preprocessPure :: PPOpts -> [Hs.Decl] -> String
preprocessPure ppOpts = genPPString ppOpts . genModule ppOpts . genSHsDecls

-- | Generate bindings for the given C header
preprocessIO ::
     PPOpts
  -> Maybe FilePath     -- ^ Output file or 'Nothing' for @STDOUT@
  -> [Hs.Decl]
  -> IO ()
preprocessIO ppOpts fp = genPP ppOpts fp . genModule ppOpts . genSHsDecls

{-------------------------------------------------------------------------------
  Template Haskell API
-------------------------------------------------------------------------------}

-- Potential TODO: Make this an opaque type, ensure path exists, and construct
-- normal file path right away.

-- | Project-specific (quoted) C include directory
data QuoteIncludeDir =
    -- | Relative to package root.
    PackageRoot FilePath
  | QuoteIncludeDir FilePath
  deriving stock (Eq, Show)

-- | Options
newtype HashIncludeOpts = HashIncludeOpts {
    extraIncludeDirs :: [QuoteIncludeDir]
  }
  deriving stock (Eq, Show)

instance Default HashIncludeOpts where
  def = HashIncludeOpts { extraIncludeDirs = [] }

-- | Generate bindings for the given C headers (simple)
--
-- Use default options ('Opts').
--
-- In particular, do not add custom C include directories.
--
-- Please see 'hashInclude' or 'hashIncludeWith' for customized binding
-- generation.
hashInclude' ::
     [FilePath]  -- ^ Input headers, as written in C @#include@
  -> TH.Q [TH.Dec]
hashInclude' fps = hashInclude fps def

-- | Generate bindings for the given C headers (custom C include directories)
--
-- Use default options ('Opts') but allow specification of custom C include
-- directories.
--
-- Please see 'hashInclude' (simple interface) or 'hashIncludeWith' (customized
-- binding generation).
hashInclude ::
     [FilePath]  -- ^ Input headers, as written in C @#include@
  -> HashIncludeOpts
  -> TH.Q [TH.Dec]
hashInclude fps HashIncludeOpts {..} = do
  quoteIncludeDirs <- toFilePaths extraIncludeDirs
  let args = def {
          clangQuoteIncludePathDirs = CIncludePathDir <$> quoteIncludeDirs
        }
      tracerConf = defaultTracerConf { tVerbosity = Verbosity Warning }
  extBindings <-
    TH.runIO . withTracerStdOut tracerConf DefaultLogLevel $ \tracer ->
      loadExtBindings tracer args UseStdlibBindingSpecs []
  let opts :: Opts
      opts = def {
          optsClangArgs   = args
        , optsExtBindings = extBindings
        }
  hashIncludeWith opts fps
  where
    toFilePath :: FilePath -> QuoteIncludeDir -> FilePath
    toFilePath root (PackageRoot     x) = root </> x
    toFilePath _    (QuoteIncludeDir x) = x

    toFilePaths :: [QuoteIncludeDir] -> TH.Q [FilePath]
    toFilePaths xs = do
      root <- getPackageRoot
      pure $ map (toFilePath root) xs

-- | Generate bindings for the given C headers
hashIncludeWith ::
     Opts
  -> [FilePath] -- ^ Input headers, as written in C @#include@
  -> TH.Q [TH.Dec]
hashIncludeWith opts fps = do
    headerIncludePaths <-
      mapM (either (TH.runIO . throwIO) return . parseCHeaderIncludePath) fps
    unit <- TH.runIO $ parseCHeaders opts headerIncludePaths
    genBindingsFromCHeader opts unit

-- | Non-IO part of 'hashIncludeWith'
genBindingsFromCHeader
    :: Guasi q
    => Opts
    -> C.TranslationUnit
    -> q [TH.Dec]
genBindingsFromCHeader opts unit = do
    -- record dependencies, including transitively included headers
    mapM_ (addDependentFile . getSourcePath) unitDeps

    mu <- getModuleUnique
    let sdecls = genSHsDecls $ genHsDecls mu opts unitDecls

    -- extensions checks.
    -- Potential TODO: we could also check which enabled extension may interfere with the generated code. (e.g. Strict/Data)
    enabledExts <- Set.fromList <$> extsEnabled
    let requiredExts = genExtensions sdecls
        missingExts  = requiredExts `Set.difference` enabledExts
    unless (null missingExts) $ do
      reportError $ "Missing LANGUAGE extensions: "
        ++ unwords (map show (toList missingExts))

    -- generate TH declarations
    genTH sdecls
  where
    C.TranslationUnit{unitDecls, unitDeps} = unit

{-------------------------------------------------------------------------------
  External bindings
-------------------------------------------------------------------------------}

-- | Generate external bindings configuration
genExtBindings ::
     PPOpts
  -> [CHeaderIncludePath]
  -> FilePath
  -> [Hs.Decl]
  -> IO ()
genExtBindings PPOpts{..} headerIncludePaths path =
        either (throwIO . HsBindgenException) return
    <=< BindingSpec.writeFile path
    .   genBindingSpec headerIncludePaths moduleName
  where
    moduleName :: HsModuleName
    moduleName = HsModuleName $ Text.pack (hsModuleOptsName ppOptsModule)

data StdlibBindingSpecs =
    -- | Automatically include @stdlib@.
    UseStdlibBindingSpecs
  | NoStdlibBindingSpecs
  deriving stock (Show, Eq)

-- | Load external bindings
loadExtBindings ::
     Tracer IO TraceMsg
  -> ClangArgs
  -> StdlibBindingSpecs
  -> [FilePath]
  -> IO ResolvedBindingSpec
loadExtBindings tracer args stdlibSpecs =
    BindingSpec.load
      (contramap TraceResolveBindingSpec tracer)
      BindingSpec.ResolveExternalBindingSpecHeader
      args stdSpec
  where
    stdSpec :: UnresolvedBindingSpec
    stdSpec = case stdlibSpecs of
      UseStdlibBindingSpecs -> Stdlib.bindings
      NoStdlibBindingSpecs  -> BindingSpec.empty

{-------------------------------------------------------------------------------
  Test generation
-------------------------------------------------------------------------------}

-- | Generate tests
genTests :: PPOpts -> [CHeaderIncludePath] -> FilePath -> [Hs.Decl] -> IO ()
genTests PPOpts{..} headerIncludePaths testDir decls =
    GenTests.genTests
      headerIncludePaths
      decls
      (hsModuleOptsName ppOptsModule)
      (hsLineLength ppOptsRender)
      testDir
