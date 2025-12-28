module HsBindgen.TH.Internal (
    -- * Template Haskell API
    Config
  , IncludeDir(..)
  , withHsBindgen
  , hashInclude

   -- * Internal artefacts
  , getExtensions
  , getThDecls
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Foldable qualified as Foldable
import Data.Set qualified as Set
import Language.Haskell.TH qualified as TH
import System.FilePath ((</>))

import Clang.Paths

import HsBindgen
import HsBindgen.Backend.Category
import HsBindgen.Backend.Extensions
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.TH.Translation
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.Errors
import HsBindgen.Frontend.RootHeader
import HsBindgen.Guasi
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.TH
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Template Haskell API
-------------------------------------------------------------------------------}

-- | Configuration with C include directories
--
-- C include directories can be provided relative to the package root (see the
-- 'IncludeDir' data constructor 'Pkg').
type Config = Config_ IncludeDir

-- | C include directory added to the C include search path
data IncludeDir =
    Dir FilePath
    -- | Include directory relative to package root
  | Pkg FilePath
  deriving stock (Eq, Show, Generic)

toFilePath :: FilePath -> IncludeDir -> FilePath
toFilePath root (Pkg x) = root </> x
toFilePath _    (Dir x) = x

-- | Generate bindings for given C headers at compile-time
--
-- Use together with 'hashInclude', which acts in the `BindgenM` monad.
--
-- For example,
--
-- > withHsBindgen def def $ do
-- >   hashInclude "foo.h"
-- >   hashInclude "bar.h"
withHsBindgen :: Config -> ConfigTH -> BindgenM -> TH.Q [TH.Dec]
withHsBindgen config configTH hashIncludes = do
    checkHsBindgenRuntimePreludeIsInScope
    packageRoot <- getPackageRoot

    bindgenConfig <- toBindgenConfigTH config packageRoot configTH.categoryChoice

    let tracerConfigUnsafe :: TracerConfig Level TraceMsg
        tracerConfigUnsafe =
          tracerConfigDefTH
            & #verbosity      .~ configTH.verbosity
            & #customLogLevel .~ getCustomLogLevel configTH.customLogLevels

    let tracerConfigSafe :: TracerConfig SafeLevel SafeTraceMsg
        tracerConfigSafe =
          tracerConfigDefTH
            & #verbosity .~ configTH.verbosity

        -- Traverse #include directives.
        bindgenState :: BindgenState
        bindgenState = execState hashIncludes (BindgenState [])

        -- Restore original order of include directives.
        uncheckedHashIncludeArgs :: [UncheckedHashIncludeArg]
        uncheckedHashIncludeArgs = reverse bindgenState.hashIncludeArgs

        artefact :: Artefact ([SourcePath], ([CWrapper], [SHs.SDecl]))
        artefact = (,) <$> Dependencies <*> (Foldable.fold <$> FinalDecls)

    (deps, decls) <- liftIO $
      hsBindgen
        tracerConfigUnsafe
        tracerConfigSafe
        bindgenConfig
        uncheckedHashIncludeArgs
        artefact

    let requiredExts = uncurry getExtensions decls
    checkLanguageExtensions requiredExts
    uncurry (getThDecls deps) decls

-- | @#include@ (i.e., generate bindings for) a C header
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
hashInclude :: FilePath -> BindgenM
hashInclude arg = do
    -- Prepend the C header to the list (the order will be reversed)
    modify $ #hashIncludeArgs %~ (arg:)

{-------------------------------------------------------------------------------
  Internal artefacts
-------------------------------------------------------------------------------}

-- | Get required extensions
getExtensions :: [CWrapper] -> [SHs.SDecl] -> Set TH.Extension
getExtensions wrappers decls = userlandCapiExt <> foldMap requiredExtensions decls
  where
    userlandCapiExt = case wrappers of
      []  -> Set.empty
      _xs -> Set.singleton TH.TemplateHaskell

-- | Get Template Haskell declarations
--
-- Internal!
--
-- Non-IO part of 'withHsBindgen'.
getThDecls
    :: Guasi q
    => [SourcePath]
    -> [CWrapper]
    -> [SHs.SDecl]
    -> q [TH.Dec]
getThDecls deps wrappers decls = do
    -- Record dependencies, including transitively included headers.
    mapM_ (addDependentFile . getSourcePath) deps

    -- Add userland-CAPI wrappers source code.
    addCSource wrapperSrc

    -- Generate TH declarations.
    fmap concat $ traverse mkDecl decls
  where
    wrapperSrc :: String
    wrapperSrc = getCWrappersSource wrappers

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | The default tracer configuration in Q uses 'outputConfigTH'
tracerConfigDefTH :: TracerConfig l a
tracerConfigDefTH = def{outputConfig = outputConfigTH}

-- | State monad used by `withBindgen`
--
-- Internal!
type BindgenM = State BindgenState ()

-- | State manipulated by 'hashInclude'
--
-- Internal!
data BindgenState = BindgenState {
      hashIncludeArgs :: [UncheckedHashIncludeArg]
    }
  deriving stock (Generic)

-- See discussion of the PR https://github.com/well-typed/hs-bindgen/pull/957,
-- in particular https://gitlab.haskell.org/ghc/ghc/-/issues/25774, and
-- https://gitlab.haskell.org/ghc/ghc/-/issues/8510.
checkHsBindgenRuntimePreludeIsInScope :: TH.Q ()
checkHsBindgenRuntimePreludeIsInScope = do
  maybeTypeName <- TH.lookupTypeName (qualifier ++ "." ++ uniqueTypeName)
  when (isNothing maybeTypeName) $ failQ errMsg
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

-- NOTE: We could also check which enabled extension may interfere with the
-- generated code (e.g. Strict/Data).
checkLanguageExtensions :: Set TH.Extension -> TH.Q ()
checkLanguageExtensions requiredExts = do
    enabledExts <- Set.fromList <$> extsEnabled
    let missingExts  = requiredExts `Set.difference` enabledExts

    unless (null missingExts) $ do
      failQ $ unlines $
        "Missing language extension(s): " :
          (map (("    - " ++) . show) (toList missingExts))

toBindgenConfigTH :: Config -> FilePath -> ByCategory Choice -> TH.Q BindgenConfig
toBindgenConfigTH config packageRoot choice = do
    uniqueId <- getUniqueId
    hsModuleName <- fromString . TH.loc_module <$> TH.location
    let bindgenConfig :: BindgenConfig
        bindgenConfig =
          toBindgenConfig
            (toFilePath packageRoot <$> config)
            uniqueId
            hsModuleName
            choice
    pure bindgenConfig
  where
    getUniqueId :: TH.Q UniqueId
    getUniqueId = UniqueId . TH.loc_package <$> TH.location
