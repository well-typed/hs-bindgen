{-# LANGUAGE CPP #-}

module HsBindgen.Pipeline (
    -- * Translation pipeline components
    parseCHeaders
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
  , loadPrescriptiveBindingSpec
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
import HsBindgen.BindingSpec
import HsBindgen.BindingSpec.Gen qualified as BindingSpec
import HsBindgen.BindingSpec.Internal qualified as BindingSpec
import HsBindgen.BindingSpec.Stdlib qualified as Stdlib
import HsBindgen.C.Parser qualified as C
import HsBindgen.Config (Config (..))
import HsBindgen.Frontend.AST.External qualified as C
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
  Translation pipeline components
-------------------------------------------------------------------------------}

-- | Parse C headers
parseCHeaders ::
      Tracer IO TraceMsg
   -> Config
   -> ExternalBindingSpec
   -> PrescriptiveBindingSpec
   -> [CHeaderIncludePath]
   -> IO C.TranslationUnit
parseCHeaders tracer config extSpec pSpec =
    C.parseCHeaders
      tracer
      config
      extSpec
      pSpec

-- | Generate @Hs@ declarations
genHsDecls :: ModuleUnique -> Config -> [C.Decl] -> [Hs.Decl]
genHsDecls mu Config{..} = Hs.generateDeclarations configTranslation mu

-- | Generate @SHs@ declarations
genSHsDecls :: [Hs.Decl] -> [SHs.SDecl]
genSHsDecls = simplifySHs . SHs.translateDecls

-- | Generate a preprocessor 'Backend.PP.HsModule'
genModule :: Config -> [SHs.SDecl] -> Backend.PP.HsModule
genModule Config{..} = Backend.PP.translateModule configHsModuleOpts

-- | Generate bindings source code, written to a file or @STDOUT@
genPP :: Config -> Maybe FilePath -> Backend.PP.HsModule -> IO ()
genPP Config{..} fp = Backend.PP.renderIO configHsRenderOpts fp

-- | Generate bindings source code
genPPString :: Config -> Backend.PP.HsModule -> String
genPPString Config{..} = Backend.PP.render configHsRenderOpts

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
translateCHeaders
  :: ModuleUnique
  -> Tracer IO TraceMsg
  -> Config
  -> ExternalBindingSpec
  -> PrescriptiveBindingSpec
  -> [CHeaderIncludePath]
  -> IO [Hs.Decl]
translateCHeaders mu tracer config extSpec pSpec headerIncludePaths = do
    C.TranslationUnit{unitDecls} <-
      parseCHeaders tracer config extSpec pSpec headerIncludePaths
    return $ genHsDecls mu config unitDecls

-- | Generate bindings for the given C header
preprocessPure :: Config -> [Hs.Decl] -> String
preprocessPure config = genPPString config . genModule config . genSHsDecls

-- | Generate bindings for the given C header
preprocessIO ::
     Config
  -> Maybe FilePath     -- ^ Output file or 'Nothing' for @STDOUT@
  -> [Hs.Decl]
  -> IO ()
preprocessIO config fp = genPP config fp . genModule config . genSHsDecls

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
  let tracerConf :: TracerConfig
      tracerConf = def { tVerbosity = Verbosity Warning }
  maybeDecls <- withTracerStdOut tracerConf $ \tracer -> do
    let args :: ClangArgs
        args = def {
            clangQuoteIncludePathDirs = CIncludePathDir <$> quoteIncludeDirs
          }
        config :: Config
        config = def { configClangArgs = args }
    hashIncludeWith tracer config fps
  case maybeDecls of
    Nothing    -> TH.reportError "An error happened (see above)" >> pure []
    Just decls -> pure decls
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
     Tracer TH.Q TraceMsg
  -> Config
  -> [FilePath] -- ^ Input headers, as written in C @#include@
  -> TH.Q [TH.Dec]
hashIncludeWith tracer config@Config{..} fps = do
    headerIncludePaths <-
      mapM (either (TH.runIO . throwIO) return . parseCHeaderIncludePath) fps
    let tracerIO = natTracer TH.runQ tracer
    -- TODO #703: For now, we only load binding spec defaults. We should
    -- however, have configuration options.
    extBindingSpec <- liftIO $
      loadExtBindingSpecs tracerIO configClangArgs UseStdlibBindingSpec []
    unit <- TH.runIO $
      parseCHeaders
        tracerIO
        config
        extBindingSpec
        emptyBindingSpec
        headerIncludePaths
    genBindingsFromCHeader config unit

-- | Non-IO part of 'hashIncludeWith'
genBindingsFromCHeader
    :: Guasi q
    => Config
    -> C.TranslationUnit
    -> q [TH.Dec]
genBindingsFromCHeader config unit = do
    -- record dependencies, including transitively included headers
    mapM_ (addDependentFile . getSourcePath) unitDeps

    mu <- getModuleUnique
    let sdecls = genSHsDecls $ genHsDecls mu config unitDecls

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
        BindingSpec.BindingSpecResolveExternalHeader
        args
        stdSpec
  where
    stdSpec :: BindingSpec.UnresolvedBindingSpec
    stdSpec = case stdlibSpec of
      UseStdlibBindingSpec -> Stdlib.bindingSpec
      NoStdlibBindingSpec  -> BindingSpec.empty

-- | Load prescriptive binding specification
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
loadPrescriptiveBindingSpec ::
     Tracer IO TraceMsg
  -> ClangArgs
  -> FilePath
  -> IO BindingSpec
loadPrescriptiveBindingSpec tracer args path = uncurry BindingSpec <$>
    BindingSpec.load
      (contramap TraceBindingSpec tracer)
      BindingSpec.BindingSpecResolvePrescriptiveHeader
      args
      BindingSpec.empty
      [path]

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
     Config
  -> [CHeaderIncludePath]
  -> FilePath
  -> [Hs.Decl]
  -> IO ()
genBindingSpec Config{..} headerIncludePaths path =
      BindingSpec.writeFile path
    . BindingSpec.genBindingSpec headerIncludePaths moduleName
  where
    moduleName :: HsModuleName
    moduleName = HsModuleName $ Text.pack (hsModuleOptsName configHsModuleOpts)

{-------------------------------------------------------------------------------
  Test generation
-------------------------------------------------------------------------------}

-- | Generate tests
genTests :: Config -> [CHeaderIncludePath] -> FilePath -> [Hs.Decl] -> IO ()
genTests Config{..} headerIncludePaths testDir decls =
    GenTests.genTests
      headerIncludePaths
      decls
      (hsModuleOptsName configHsModuleOpts)
      (hsLineLength configHsRenderOpts)
      testDir
