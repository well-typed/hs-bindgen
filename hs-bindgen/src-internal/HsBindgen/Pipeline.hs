{-# LANGUAGE CPP #-}

module HsBindgen.Pipeline (
    -- * Options
    Opts(..)
  , PPOpts(..)

    -- * Translation pipeline components
  , parseCHeader
  , genHsDecls
  , genSHsDecls
  , genModule
  , genPP
  , genPPString
  , genTH
  , genExtensions

    -- * Preprocessor API
  , translateCHeader
  , preprocessPure
  , preprocessIO

    -- * Template Haskell API
  , QuoteIncludeDir (..)
  , HashIncludeOpts (..)
  , hashInclude
  , hashInclude'
  , hashIncludeWith
  , genBindingsFromCHeader

    -- * External bindings generation
  , genExtBindings

    -- * Test generation
  , genTests
  ) where

import Control.Monad ((<=<))
import Control.Tracer (Tracer, nullTracer)
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
import HsBindgen.BindingSpec (ResolvedBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen (genBindingSpec)
import HsBindgen.C.Parser qualified as C
import HsBindgen.C.Predicate (Predicate (..))
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.GenTests qualified as GenTests
import HsBindgen.Guasi
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import HsBindgen.ModuleUnique
import HsBindgen.SHs.AST qualified as SHs
import HsBindgen.SHs.Translation qualified as SHs
import HsBindgen.Util.Trace (Trace)
import HsBindgen.Util.Tracer (TraceWithCallStack)

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
      optsClangArgs   :: ClangArgs
    , optsExtBindings :: ResolvedBindingSpec
    , optsTranslation :: Hs.TranslationOpts
    , optsPredicate   :: Predicate
    , optsTracer      :: Tracer IO (TraceWithCallStack Trace)
    }

instance Default Opts where
  def = Opts {
      optsClangArgs   = def
    , optsExtBindings = BindingSpec.empty
    , optsTranslation = def
    , optsPredicate   = SelectFromMainFiles
    , optsTracer      = nullTracer
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

-- | Parse a C header
parseCHeader ::
      HasCallStack
   => Opts
   -> CHeaderIncludePath
   -> IO C.TranslationUnit
parseCHeader Opts{..} headerIncludePath =
    C.parseCHeaders
      optsTracer
      optsClangArgs
      optsPredicate
      optsExtBindings
      [headerIncludePath]

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
translateCHeader :: HasCallStack
  => ModuleUnique -> Opts -> CHeaderIncludePath -> IO [Hs.Decl]
translateCHeader mu opts headerIncludePath = do
    C.TranslationUnit{unitDecls} <- parseCHeader opts headerIncludePath
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

-- | Generate bindings for the given C header (simple)
--
-- Use default options ('Opts').
--
-- In particular, do not add custom C include directories.
--
-- Please see 'hashInclude' or 'hashIncludeWith' for customized binding
-- generation.
hashInclude' ::
     FilePath   -- ^ Input header, as written in C @#include@
  -> TH.Q [TH.Dec]
hashInclude' fp = hashInclude fp def

-- | Generate bindings for the given C header (custom C include directories)
--
-- Use default options ('Opts') but allow specification of custom C include
-- directories.
--
-- Please see 'hashInclude' (simple interface) or 'hashIncludeWith' (customized
-- binding generation).
hashInclude ::
     FilePath   -- ^ Input header, as written in C @#include@
  -> HashIncludeOpts
  -> TH.Q [TH.Dec]
hashInclude fp HashIncludeOpts {..} = do
  quoteIncludeDirs <- toFilePaths extraIncludeDirs
  let opts :: Opts
      opts = def {
        optsClangArgs = def {
            clangQuoteIncludePathDirs  = CIncludePathDir <$> quoteIncludeDirs
          }
      }
  hashIncludeWith opts fp
  where
    toFilePath :: FilePath -> QuoteIncludeDir -> FilePath
    toFilePath root (PackageRoot     x) = root </> x
    toFilePath _    (QuoteIncludeDir x) = x

    toFilePaths :: [QuoteIncludeDir] -> TH.Q [FilePath]
    toFilePaths xs = do
      root <- getPackageRoot
      pure $ map (toFilePath root) xs

-- | Generate bindings for the given C header
hashIncludeWith :: HasCallStack =>
     Opts
  -> FilePath -- ^ Input header, as written in C @#include@
  -> TH.Q [TH.Dec]
hashIncludeWith opts fp = do
    headerIncludePath <- either (TH.runIO . throwIO) return $
      parseCHeaderIncludePath fp
    unit <- TH.runIO $ parseCHeader opts headerIncludePath
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
  External bindings generation
-------------------------------------------------------------------------------}

-- | Generate external bindings configuration
genExtBindings ::
     PPOpts
  -> CHeaderIncludePath
  -> FilePath
  -> [Hs.Decl]
  -> IO ()
genExtBindings PPOpts{..} headerIncludePath path =
        either (throwIO . HsBindgenException) return
    <=< BindingSpec.writeFile path
    .   genBindingSpec headerIncludePath moduleName
  where
    moduleName :: HsModuleName
    moduleName = HsModuleName $ Text.pack (hsModuleOptsName ppOptsModule)

{-------------------------------------------------------------------------------
  Test generation
-------------------------------------------------------------------------------}

-- | Generate tests
genTests :: PPOpts -> CHeaderIncludePath -> FilePath -> [Hs.Decl] -> IO ()
genTests PPOpts{..} headerIncludePath testDir decls =
    GenTests.genTests
      headerIncludePath
      decls
      (hsModuleOptsName ppOptsModule)
      (hsLineLength ppOptsRender)
      testDir
