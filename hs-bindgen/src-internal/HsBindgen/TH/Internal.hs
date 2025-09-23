{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}

module HsBindgen.TH.Internal (
    -- * Template Haskell API
    IncludeDir(..)
  , withHsBindgen
  , hashInclude

   -- * Internal artefacts
  , getExtensions
  , getThDecls
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Set qualified as Set
import Language.Haskell.TH qualified as TH
import System.FilePath ((</>))

import Clang.Paths
import HsBindgen
import HsBindgen.Backend.Extensions
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Backend.Hs.Translation
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.TH.Translation
import HsBindgen.Backend.UniqueId
import HsBindgen.BindingSpec
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.Errors
import HsBindgen.Frontend.RootHeader
import HsBindgen.Guasi
import HsBindgen.Imports
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
  deriving stock (Eq, Show, Generic)

-- | Generate bindings for given C headers at compile-time.
--
-- Use together with 'hashInclude'.
--
-- For example,
--
-- > withHsBindgen def $ do
-- >   hashInclude "a.h"
-- >   hashInclude "b.h"
withHsBindgen :: Config IncludeDir -> ConfigTH -> Bindgen () -> TH.Q [TH.Dec]
withHsBindgen config ConfigTH{..} hashIncludes = do
    checkHsBindgenRuntimePreludeIsInScope

    bindgenConfig <- toBindgenConfigTH config

    let -- Traverse #include directives.
        bindgenState :: BindgenState
        bindgenState = execState hashIncludes (BindgenState [])

        -- Restore original order of include directives.
        uncheckedHashIncludeArgs :: [UncheckedHashIncludeArg]
        uncheckedHashIncludeArgs =
          reverse $ bindgenStateUncheckedHashIncludeArgs bindgenState

        artefacts = Dependencies :* FinalDecls :* Nil

    (I deps :* I decls' :* Nil) <- liftIO $
      hsBindgen
        tracerConfigDefTH
        bindgenConfig
        uncheckedHashIncludeArgs
        artefacts
    let decls = mergeDecls configTHSafety decls'
        requiredExts = uncurry getExtensions $ decls
    checkLanguageExtensions requiredExts
    uncurry (getThDecls deps) decls

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
  let -- Prepend the C header to the list. That is, the order of include
      -- directives will be reversed.
      prependArg :: BindgenState -> BindgenState
      prependArg = BindgenState . (arg :) . bindgenStateUncheckedHashIncludeArgs
  modify prependArg

{-------------------------------------------------------------------------------
  Internal artefacts
-------------------------------------------------------------------------------}

-- | Get required extensions.
getExtensions :: [UserlandCapiWrapper] -> [SHs.SDecl] -> Set TH.Extension
getExtensions wrappers decls = userlandCapiExt <> foldMap requiredExtensions decls
  where
    userlandCapiExt = case wrappers of
      []  -> Set.empty
      _xs -> Set.singleton TH.TemplateHaskell

-- | Internal; Get Template Haskell declarations; non-IO part of
-- 'withHsBindgen'.
getThDecls
    :: Guasi q
    => [SourcePath]
    -> [UserlandCapiWrapper]
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
    wrapperSrc = getUserlandCapiWrappersSource wrappers

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | The default tracer configuration in Q uses 'outputConfigTH'.
tracerConfigDefTH :: TracerConfig IO l a
tracerConfigDefTH = def {
        tOutputConfig = outputConfigTH
      }

-- | Internal! See 'withHsBindgen'.
type Bindgen a = State BindgenState a

-- | Internal! State manipulated by 'hashInclude'.
data BindgenState = BindgenState {
    bindgenStateUncheckedHashIncludeArgs :: [UncheckedHashIncludeArg]
  }

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


toBindgenConfigTH :: Config IncludeDir -> TH.Q BindgenConfig
toBindgenConfigTH config = do
    packageRoot <- getPackageRoot
    let Config{..} = toFilePath packageRoot <$> config

    uniqueId <- getUniqueId

    let bootConfig = BootConfig {
            bootClangArgsConfig   = configClangArgsConfig
          , bootBindingSpecConfig = BindingSpecConfig {
                bindingSpecStdlibSpec              = configStdlibSpec
              , bindingSpecCompatibility           = configCompatibility
              , bindingSpecExtBindingSpecs         = configExtBindingSpecs
              , bindingSpecPrescriptiveBindingSpec = configPrescriptiveBindingSpec
              }
          }
        frontendConfig = FrontendConfig {
            frontendParsePredicate  = configParsePredicate
          , frontendSelectPredicate = configSelectPredicate
          , frontendProgramSlicing  = configProgramSlicing
          }
        backendConfig = BackendConfig {
            backendTranslationOpts = def {
                translationUniqueId = uniqueId
              }
          , backendHsModuleOpts = def
          , backendHaddockConfig = HaddockConfig {
                pathStyle = configHaddockPathStyle
              }
          }

    pure $ BindgenConfig bootConfig frontendConfig backendConfig
  where
    toFilePath :: FilePath -> IncludeDir -> FilePath
    toFilePath root (RelativeToPkgRoot x) = root </> x
    toFilePath _    (IncludeDir        x) = x

    getUniqueId :: TH.Q UniqueId
    getUniqueId = UniqueId . TH.loc_package <$> TH.location
