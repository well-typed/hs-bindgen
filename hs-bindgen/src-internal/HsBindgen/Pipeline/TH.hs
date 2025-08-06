{-# LANGUAGE CPP #-}

module HsBindgen.Pipeline.TH (
    -- * Template Haskell API
    IncludeDir (..)
  , BindgenOpts ( extraIncludeDirs
                , baseConfig
                , tracerOutputConfigQ
                , tracerCustomLogLevel
                , tracerTracerConfig
                )
  , withHsBindgen
  , hashInclude
  , genBindingsFromCHeader
  ) where

import Control.Monad.RWS (MonadReader (..), RWST, execRWST, modify)
import Data.Set qualified as Set
import Language.Haskell.TH qualified as TH
import System.FilePath ((</>))

import Clang.Args
import Clang.Paths
import HsBindgen.BindingSpec (BindingSpecConfig)
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config (Config (..))
import HsBindgen.Frontend
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.RootHeader
import HsBindgen.Guasi
import HsBindgen.Imports
import HsBindgen.Pipeline.Lib qualified as Lib
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

#ifdef MIN_VERSION_th_compat
import Language.Haskell.TH.Syntax.Compat (getPackageRoot)
#else
import Language.Haskell.TH.Syntax (getPackageRoot)
#endif

{-------------------------------------------------------------------------------
  Template Haskell API
-------------------------------------------------------------------------------}

-- | Project-specific C include directory.
--
-- Will be added either to the C include search path.
data IncludeDir =
    IncludeDir        FilePath
    -- | Include directory relative to package root.
  | RelativeToPkgRoot FilePath
  deriving stock (Eq, Show)

-- | Options (opaque, but with record selector functions exported).
data BindgenOpts = BindgenOpts {
    -- * General configuration
    extraIncludeDirs     :: [IncludeDir]
  , baseConfig           :: Config
    -- * Binding specifications
  , bindingSpecConfig    :: BindingSpecConfig
    -- * Tracer
  , tracerOutputConfigQ  :: OutputConfig TH.Q
  , tracerCustomLogLevel :: CustomLogLevel TraceMsg
  , tracerTracerConfig   :: TracerConfig
  }

instance Default BindgenOpts where
  def = BindgenOpts {
      extraIncludeDirs     = []
    , baseConfig           = def
    , bindingSpecConfig    = def
    , tracerTracerConfig   = def { tVerbosity = Verbosity Notice }
    , tracerCustomLogLevel = mempty
    , tracerOutputConfigQ  = outputConfigQ
    }

-- | Internal! See 'withHsBindgen'.
type Bindgen a = RWST BindgenEnv () BindgenState TH.Q a

-- | Internal! Environment available to 'hashInclude'.
data BindgenEnv = BindgenEnv {
    bindgenEnvTracer :: Tracer IO TraceMsg
  }

-- | Internal! State manipulated by 'hashInclude'.
data BindgenState = BindgenState {
    bindgenStateHashIncludeArgs :: [HashIncludeArg]
  }

-- | Generate bindings for given C headers at compile-time.
--
-- Use together with 'hashInclude'.
--
-- For example,
--
-- > withHsBindgen def $ do
-- >   hashInclude "a.h"
-- >   hashInclude "b.h"
withHsBindgen :: BindgenOpts -> Bindgen () -> TH.Q [TH.Dec]
withHsBindgen BindgenOpts{..} hashIncludes = do
  checkHsBindgenRuntimePreludeIsInScope
  includeDirs <- toFilePaths extraIncludeDirs
  let clangArgs :: ClangArgs
      clangArgs = def {
          clangExtraIncludeDirs = CIncludeDir <$> includeDirs
        }
      config :: Config
      config = def { configClangArgs = clangArgs }

  maybeDecls <- withTracer $ \tracer -> do
    let tracerIO = natTracer TH.runQ tracer
    -- Traverse #include directives.
    bindgenState <- fst <$> execRWST
      hashIncludes
      (BindgenEnv tracerIO)
      (BindgenState [])
    -- Restore original order of include directives.
    let hashIncludeArgs :: [HashIncludeArg]
        hashIncludeArgs = reverse $ bindgenStateHashIncludeArgs bindgenState
    -- External and prescriptive binding specifications.
    (extBindingSpec, prescriptiveBindingSpec) <- TH.runIO $
      BindingSpec.loadBindingSpecs
        (contramap TraceBindingSpec tracerIO)
        clangArgs
        bindingSpecConfig
    -- Parse translation unit.
    unit <- TH.runIO $
      frontend
        (contramap TraceFrontend tracerIO)
        config
        extBindingSpec
        prescriptiveBindingSpec
        hashIncludeArgs
    -- Generate bindings.
    genBindingsFromCHeader config unit
  -- Error handling.
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

-- | @#include@ (i.e., generate bindings for) a C header.
--
-- For example, the Haskell code,
--
-- > hashInclude "a.h"
--
-- corresponds to the following C code using angular brackets,
--
-- > #include <a.h>
--
-- See 'withHsBindgen'.
hashInclude :: FilePath -> Bindgen ()
hashInclude arg = do
  tracer       <- contramap TraceHashIncludeArg . bindgenEnvTracer <$> ask
  -- We never fail, but emit a trace if C headers contain unexpected characters
  -- such as backslashes.
  argValidated <- liftIO $ hashIncludeArgWithTrace tracer arg
  let -- Prepend the C header to the list. That is, the order of include
      -- directives will be reversed.
      prependArg :: BindgenState -> BindgenState
      prependArg = BindgenState . (argValidated :) . bindgenStateHashIncludeArgs
  modify prependArg

-- | Internal API; exported for testing; non-IO part of 'withHsBindgen'
genBindingsFromCHeader
    :: Guasi q
    => Config
    -> C.TranslationUnit
    -> q [TH.Dec]
genBindingsFromCHeader config unit = do
    -- Record dependencies, including transitively included headers.
    mapM_ (addDependentFile . getSourcePath) unitDeps

    mu <- getModuleUnique
    let sdecls = Lib.genSHsDecls $ Lib.genHsDecls mu config unitDecls

    -- Extensions checks.
    -- Potential TODO: we could also check which enabled extension may interfere
    -- with the generated code. (e.g. Strict/Data)
    enabledExts <- Set.fromList <$> extsEnabled
    let requiredExts = Lib.genExtensions sdecls
        missingExts  = requiredExts `Set.difference` enabledExts
    unless (null missingExts) $ do
      reportError $ "Missing LANGUAGE extensions: "
        ++ unwords (map show (toList missingExts))

    -- Generate TH declarations.
    Lib.genTH sdecls
  where
    C.TranslationUnit{unitDecls, unitDeps} = unit

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
