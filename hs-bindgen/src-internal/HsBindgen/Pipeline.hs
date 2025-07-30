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
  , IncludeDir (..)
  , HashIncludeOpts ( extraQuoteIncludeDirs
                    , extraSystemIncludeDirs
                    , tracerOutputConfigQ
                    , tracerCustomLogLevel
                    , tracerTracerConfig
                    )
  , withHsBindgen
  , hashInclude
  , genBindingsFromCHeader

    -- * Test generation
  , genTests
  ) where

import Control.Exception (Exception (displayException))
import Control.Monad.State (State, execState, modify)
import Data.Set qualified as Set
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
import HsBindgen.BindingSpec (BindingSpecConfig, ExternalBindingSpec,
                              PrescriptiveBindingSpec)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.C.Parser qualified as C
import HsBindgen.Config (Config (..))
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.RootHeader
import HsBindgen.GenTests qualified as GenTests
import HsBindgen.Guasi
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Imports
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
   -> [HashIncludeArg]
   -> IO C.TranslationUnit
parseCHeaders = C.parseCHeaders

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
  -> [HashIncludeArg]
  -> IO [Hs.Decl]
translateCHeaders mu tracer config extSpec pSpec hashIncludeArgs = do
    C.TranslationUnit{unitDecls} <-
      parseCHeaders tracer config extSpec pSpec hashIncludeArgs
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

-- IDEA: Make 'QuoteIncludePathDir' an opaque type, ensure path exists, and
-- construct normal file path right away.

-- | Project-specific C include directory.
--
-- Will be added either to the C sytem include search path or the C quote
-- include search path, depending on whether it is used in
-- 'extraQuoteIncludeDirs', or 'extraSystemIncludeDirs'.
data IncludeDir =
    IncludeDir    FilePath
    -- | Include directory relative to package root.
  | RelativeToPkgRoot FilePath
  deriving stock (Eq, Show)

-- | Options (opaque, but with record selector functions exported).
data HashIncludeOpts = HashIncludeOpts {
    extraSystemIncludeDirs :: [IncludeDir]
  , extraQuoteIncludeDirs  :: [IncludeDir]
    -- * Binding specifications
  , bindingSpecConfig      :: BindingSpecConfig
    -- * Tracer
  , tracerOutputConfigQ    :: OutputConfig TH.Q
  , tracerCustomLogLevel   :: CustomLogLevel TraceMsg
  , tracerTracerConfig     :: TracerConfig
  }

instance Default HashIncludeOpts where
  def = HashIncludeOpts {
      extraSystemIncludeDirs = []
    , extraQuoteIncludeDirs  = []
    , bindingSpecConfig      = def
    , tracerTracerConfig     = def { tVerbosity = Verbosity Notice }
    , tracerCustomLogLevel   = mempty
    , tracerOutputConfigQ    = outputConfigQ
    }

-- | Internal! See 'withHsBindgen'.
newtype WithHsBindgenM a =  WithHsBindgenM {
    getWithHsBindgenM :: State WithHsBindgenState a
  }

-- | Internal! State manipulated by monadic 'hashInclude' directives.
newtype WithHsBindgenState = WithHsBindgenState {
    hashIncludeArgs :: [HashIncludeArg]
  }

-- | Generate bindings for given C header include paths at compile-time.
--
-- Use together with 'hashInclude'.
--
-- For example,
--
-- > withHsBindgen def $ do
-- >   hashInclude "a.h"
-- >   hashInclude "b.h"
withHsBindgen :: HashIncludeOpts -> WithHsBindgenM () -> TH.Q [TH.Dec]
withHsBindgen HashIncludeOpts{..} hashIncludes = do
  checkHsBindgenRuntimePreludeIsInScope
  quoteIncludeDirs <- toFilePaths extraQuoteIncludeDirs
  systemIncludeDirs <- toFilePaths extraSystemIncludeDirs
  let clangArgs :: ClangArgs
      clangArgs = def {
          clangExtraSystemIncludeDirs = CIncludeDir <$> systemIncludeDirs
        , clangExtraQuoteIncludeDirs  = CIncludeDir <$> quoteIncludeDirs
        }
      config :: Config
      config = def { configClangArgs = clangArgs }
  maybeDecls <- withTracer $ \tracer -> do
    hashIncludeArgsValidated <- mapM getHeaderIncludePathOrFail hashIncludeArgsReversed
    let tracerIO = natTracer TH.runQ tracer
    (extBindingSpec, prescriptiveBindingSpec) <- TH.runIO $
      BindingSpec.loadBindingSpecs
        (contramap TraceBindingSpec tracerIO)
        clangArgs
        bindingSpecConfig
    unit <- TH.runIO $
      parseCHeaders
        tracerIO
        config
        extBindingSpec
        prescriptiveBindingSpec
        hashIncludeArgsValidated
    genBindingsFromCHeader config unit
  case maybeDecls of
    Nothing    -> TH.reportError "An error happened (see above)" >> pure []
    Just decls -> pure decls
  where
    toFilePath :: FilePath -> IncludeDir -> FilePath
    toFilePath root (RelativeToPkgRoot x) = root </> x
    toFilePath _    (IncludeDir        x) = x

    toFilePaths :: [IncludeDir] -> TH.Q [FilePath]
    toFilePaths xs = do
      root <- getPackageRoot
      pure $ map (toFilePath root) xs

    withTracer :: (Tracer TH.Q TraceMsg -> TH.Q b) -> TH.Q (Maybe b)
    withTracer = withTracerCustom
                   tracerOutputConfigQ
                   tracerCustomLogLevel
                   tracerTracerConfig

    getHeaderIncludePathOrFail :: HashIncludeArg -> TH.Q HashIncludeArg
    getHeaderIncludePathOrFail =
      either (fail . displayException) pure . validateHashIncludeArg

    withHsBindgenState :: WithHsBindgenState
    withHsBindgenState =
      execState (getWithHsBindgenM hashIncludes) (WithHsBindgenState [])

    -- Restore order of include directives.
    hashIncludeArgsReversed :: [HashIncludeArg]
    hashIncludeArgsReversed = reverse $ hashIncludeArgs withHsBindgenState

-- | "Hash include" (i.e., generate bindings for) a file.
--
-- See 'withHsBindgen'.
hashInclude :: HashIncludeArg -> WithHsBindgenM ()
hashInclude arg = WithHsBindgenM $ modify addArg
  where -- Prepend the include directive to the list. That is, the order of
        -- include directives will be reversed.
        addArg :: WithHsBindgenState -> WithHsBindgenState
        addArg = WithHsBindgenState . (arg :) . hashIncludeArgs

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
  Test generation
-------------------------------------------------------------------------------}

-- | Generate tests
genTests :: Config -> [HashIncludeArg] -> FilePath -> [Hs.Decl] -> IO ()
genTests Config{..} hashIncludeArgs testDir decls =
    GenTests.genTests
      hashIncludeArgs
      decls
      (hsModuleOptsName configHsModuleOpts)
      (hsLineLength configHsRenderOpts)
      testDir

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- See discussion of the PR https://github.com/well-typed/hs-bindgen/pull/957,
-- in particular https://gitlab.haskell.org/ghc/ghc/-/issues/25774, and
-- https://gitlab.haskell.org/ghc/ghc/-/issues/8510.
checkHsBindgenRuntimePreludeIsInScope :: TH.Q ()
checkHsBindgenRuntimePreludeIsInScope = do
  maybeTypeName <- TH.lookupTypeName (qualifier ++ "." ++ uniqueTypeName)
  when (isNothing maybeTypeName) $ fail errMsg
  where
    qualifier :: String
    qualifier = "HsBindgen.Runtime.Prelude"

    uniqueTypeName :: String
    uniqueTypeName = "HsBindgenRuntimePreludeIsInScope"

    errMsg :: String
    errMsg = unlines [
        "'HsBindgen.Runtime.Prelude' is out of scope."
      , "    Please add the following import to your module:"
      , ""
      , "      import qualified HsBindgen.Runtime.Prelude"
      ]
