module HsBindgen.Pipeline (
    -- * Options
    Opts(..)
  , defaultOpts
  , PPOpts(..)
  , defaultPPOpts

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
  , genBindings
  , genBindingsFromCHeader
  , genBindings'

    -- * External bindings generation
  , genExtBindings

    -- * Test generation
  , genTests
  ) where

import Control.Monad ((<=<))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH

import Clang.Args
import Clang.Paths
import HsBindgen.Backend.Extensions
import HsBindgen.Backend.PP.Render (HsRenderOpts(..))
import HsBindgen.Backend.PP.Render qualified as Backend.PP
import HsBindgen.Backend.PP.Translation (HsModuleOpts(..))
import HsBindgen.Backend.PP.Translation qualified as Backend.PP
import HsBindgen.Backend.TH.Translation qualified as Backend.TH
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Parser qualified as C
import HsBindgen.C.Predicate (Predicate(..))
import HsBindgen.Errors
import HsBindgen.ExtBindings
import HsBindgen.ExtBindings.Gen qualified as GenExtBindings
import HsBindgen.GenTests qualified as GenTests
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.NameMangler (NameMangler)
import HsBindgen.Hs.NameMangler qualified as NameMangler
import HsBindgen.Hs.NameMangler.DSL qualified as NameMangler.DSL
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Imports
import HsBindgen.SHs.AST qualified as SHs
import HsBindgen.SHs.Translation qualified as SHs
import HsBindgen.Util.Tracer
import HsBindgen.Guasi
import HsBindgen.ModuleUnique

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Options for both the preprocessor and TH APIs
data Opts = Opts {
      optsClangArgs   :: ClangArgs
    , optsExtBindings :: ExtBindings
    , optsTranslation :: Hs.TranslationOpts
    , optsNameMangler :: NameMangler
    , optsPredicate   :: Predicate
    , optsDiagTracer  :: Tracer IO String
    , optsSkipTracer  :: Tracer IO String
    }

-- | Default 'Opts'
defaultOpts :: Opts
defaultOpts = Opts {
      optsClangArgs   = defaultClangArgs
    , optsExtBindings = emptyExtBindings
    , optsTranslation = Hs.defaultTranslationOpts
    , optsNameMangler = nameMangler
    , optsPredicate   = SelectFromMainFile
    , optsDiagTracer  = nullTracer
    , optsSkipTracer  = nullTracer
    }
  where
    -- TODO: Make it possible to specify overrides through the CLI
    nameMangler :: NameMangler
    nameMangler =
        NameMangler.DSL.applyOverrides NameMangler.DSL.overridesNone $
          NameMangler.nameManglerDefault

-- | Additional options for the preprocessor API
data PPOpts = PPOpts {
      ppOptsModule :: HsModuleOpts -- ^ Default module name: @Generated@
    , ppOptsRender :: HsRenderOpts -- ^ Default line length: 120
    }

-- | Default 'PPOpts'
defaultPPOpts :: PPOpts
defaultPPOpts = PPOpts {
      ppOptsModule = HsModuleOpts { hsModuleOptsName = "Generated" }
    , ppOptsRender = HsRenderOpts { hsLineLength = 120 }
    }

{-------------------------------------------------------------------------------
  Translation pipeline components
-------------------------------------------------------------------------------}

-- | Parse a C header
parseCHeader :: Opts -> CHeaderIncludePath -> IO ([SourcePath], C.Header)
parseCHeader Opts{..} headerIncludePath =
    C.parseCHeaders
      (contramap show optsDiagTracer)
      (contramap prettyLogMsg optsSkipTracer)
      optsClangArgs
      optsPredicate
      optsExtBindings
      [headerIncludePath]

-- | Generate @Hs@ declarations
genHsDecls :: ModuleUnique -> Opts -> C.Header -> [Hs.Decl]
genHsDecls mu Opts{..} = Hs.generateDeclarations optsTranslation mu optsNameMangler

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
translateCHeader :: ModuleUnique -> Opts -> CHeaderIncludePath -> IO [Hs.Decl]
translateCHeader mu opts headerIncludePath = do
    (_depPaths, header) <- parseCHeader opts headerIncludePath
    return $ genHsDecls mu opts header

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

-- | Generate bindings for the given C header
genBindings ::
     Opts
  -> FilePath -- ^ Input header, as written in C @#include@
  -> TH.Q [TH.Dec]
genBindings opts fp = do
    headerIncludePath <- either (TH.runIO . throwIO) return $
      parseCHeaderIncludePath fp
    (depPaths, cheader) <- TH.runIO $ parseCHeader opts headerIncludePath
    genBindingsFromCHeader opts depPaths cheader

-- | Non-IO part of 'genBindings'
genBindingsFromCHeader
    :: Guasi q
    => Opts
    -> [SourcePath]
    -> C.Header
    -> q [TH.Dec]
genBindingsFromCHeader opts depPaths cheader = do
    -- record dependencies, including transitively included headers
    mapM_ (addDependentFile . getSourcePath) depPaths

    mu <- getModuleUnique
    let sdecls = genSHsDecls $ genHsDecls mu opts cheader

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
    <=< writeUnresolvedExtBindings path
    .   GenExtBindings.genExtBindings headerIncludePath moduleName
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
