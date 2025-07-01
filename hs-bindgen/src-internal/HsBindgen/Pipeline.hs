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
  , QuoteIncludePathDir (..)
  , HashIncludeOpts (..)
  , hashInclude
  , hashInclude'
  , hashIncludeWith
  , genBindingsFromCHeader

    -- * Binding specifications
  , BindingSpec (..)
  , emptyBindingSpec
  , StdlibBindingSpecConf (..)
  , loadExtBindingSpecs
  , getStdlibBindingSpec
  , encodeBindingSpecJson
  , encodeBindingSpecYaml
  , genBindingSpec

    -- * Test generation
  , genTests
  ) where

import Data.ByteString qualified as BSS
import Data.ByteString.Lazy qualified as BSL
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
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen qualified as BindingSpec
import HsBindgen.BindingSpec.Stdlib qualified as Stdlib
import HsBindgen.C.Parser qualified as C
import HsBindgen.C.Predicate (Predicate (..))
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
import HsBindgen.SHs.Simplify (simplifySHs)
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
    , optsExtBindingSpec :: BindingSpec
    , optsTranslation    :: Hs.TranslationOpts
    , optsPredicate      :: Predicate
    , optsProgramSlicing :: ProgramSlicing
    , optsTracer         :: Tracer IO TraceMsg
    }

instance Default Opts where
  def = Opts {
      optsClangArgs      = def
    , optsExtBindingSpec = emptyBindingSpec
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
    C.parseCHeaders
      optsTracer
      optsClangArgs
      optsPredicate
      optsProgramSlicing
      (bindingSpecResolved optsExtBindingSpec)

-- | Generate @Hs@ declarations
genHsDecls :: ModuleUnique -> Opts -> [C.Decl] -> [Hs.Decl]
genHsDecls mu Opts{..} = Hs.generateDeclarations optsTranslation mu

-- | Generate @SHs@ declarations
genSHsDecls :: [Hs.Decl] -> [SHs.SDecl]
genSHsDecls = simplifySHs . SHs.translateDecls

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
data QuoteIncludePathDir =
    -- | Relative to package root.
    PackageRoot FilePath
  | QuoteIncludeDir FilePath
  deriving stock (Eq, Show)

-- | Options
newtype HashIncludeOpts = HashIncludeOpts {
    extraIncludeDirs :: [QuoteIncludePathDir]
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
  let args :: ClangArgs
      args = def {
          clangQuoteIncludePathDirs = CIncludePathDir <$> quoteIncludeDirs
        }
      tracerConf :: TracerConf
      tracerConf = def { tVerbosity = Verbosity Warning }
  extBindingSpec <-
    TH.runIO . withTracerStdOut tracerConf DefaultLogLevel $ \tracer ->
      loadExtBindingSpecs tracer args UseStdlibBindingSpec []
  let opts :: Opts
      opts = def {
          optsClangArgs      = args
        , optsExtBindingSpec = extBindingSpec
        }
  hashIncludeWith opts fps
  where
    toFilePath :: FilePath -> QuoteIncludePathDir -> FilePath
    toFilePath root (PackageRoot     x) = root </> x
    toFilePath _    (QuoteIncludeDir x) = x

    toFilePaths :: [QuoteIncludePathDir] -> TH.Q [FilePath]
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
  Binding specifications
-------------------------------------------------------------------------------}

-- | Binding specification
--
-- A binding specification serves two purposes:
--
-- * A /prescriptive binding specification/ is used to configure how bindings
--   are generated.
-- * An /external binding specification/ is used to specify existing bindings
--   that should be used, /external/ from the module being generated.
--
-- Note that a /generated binding specification/ may be used for either/both of
-- these two purposes.
data BindingSpec = BindingSpec {
      bindingSpecUnresolved :: BindingSpec.UnresolvedBindingSpec
    , bindingSpecResolved   :: BindingSpec.ResolvedBindingSpec
    }

-- | Empty binding specification
emptyBindingSpec :: BindingSpec
emptyBindingSpec = BindingSpec {
      bindingSpecUnresolved = BindingSpec.empty
    , bindingSpecResolved   = BindingSpec.empty
    }

-- | Configure if the @stdlib@ binding specification should be used
data StdlibBindingSpecConf =
    -- | Automatically include @stdlib@
    UseStdlibBindingSpec
  | NoStdlibBindingSpec
  deriving stock (Show, Eq)

-- | Load external binding specifications
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
loadExtBindingSpecs ::
     Tracer IO TraceMsg
  -> ClangArgs
  -> StdlibBindingSpecConf
  -> [FilePath]
  -> IO BindingSpec
loadExtBindingSpecs tracer args stdlibSpec =
      fmap (uncurry BindingSpec)
    . BindingSpec.load
        (contramap TraceBindingSpec tracer)
        BindingSpec.ResolveExternalBindingSpecHeader
        args
        stdSpec
  where
    stdSpec :: BindingSpec.UnresolvedBindingSpec
    stdSpec = case stdlibSpec of
      UseStdlibBindingSpec -> Stdlib.bindingSpec
      NoStdlibBindingSpec  -> BindingSpec.empty

getStdlibBindingSpec ::
     Tracer IO TraceMsg
  -> ClangArgs
  -> IO BindingSpec
getStdlibBindingSpec tracer args =
    loadExtBindingSpecs tracer args UseStdlibBindingSpec []

encodeBindingSpecJson :: BindingSpec -> BSL.ByteString
encodeBindingSpecJson = BindingSpec.encodeJson . bindingSpecUnresolved

encodeBindingSpecYaml :: BindingSpec -> BSS.ByteString
encodeBindingSpecYaml = BindingSpec.encodeYaml . bindingSpecUnresolved

-- | Generate binding specification
genBindingSpec ::
     Tracer IO TraceMsg
  -> PPOpts
  -> [CHeaderIncludePath]
  -> FilePath
  -> [Hs.Decl]
  -> IO ()
genBindingSpec tracer PPOpts{..} headerIncludePaths path =
      BindingSpec.writeFile tracer' path
    . BindingSpec.genBindingSpec headerIncludePaths moduleName
  where
    moduleName :: HsModuleName
    moduleName = HsModuleName $ Text.pack (hsModuleOptsName ppOptsModule)

    tracer' :: Tracer IO BindingSpec.WriteBindingSpecMsg
    tracer' =
      contramap (TraceBindingSpec . BindingSpec.WriteBindingSpecMsg) tracer

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
