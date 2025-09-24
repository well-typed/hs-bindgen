{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}

module HsBindgen.TH.Internal (
    -- * Template Haskell API
    IncludeDir (..)
  , BindgenOpts (
      baseBootConfig
    , extraIncludeDirs
    , baseFrontendConfig
    , safety
    , baseBackendConfig
    )
  , tracerConfigDefTH
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
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.Backend.Extensions
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Translation
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.TH.Translation
import HsBindgen.Backend.UniqueId
import HsBindgen.Config
import HsBindgen.Config.ClangArgs
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
  deriving stock (Eq, Show)

-- | Options (opaque, but with record selector functions exported).
data BindgenOpts = BindgenOpts {
    -- * Binding specifications
    baseBootConfig     :: BootConfig
    -- * Frontend configuration
  , extraIncludeDirs   :: [IncludeDir]
  , baseFrontendConfig :: FrontendConfig
    -- * Backend configuration
  , safety             :: SHs.Safety
  , baseBackendConfig  :: BackendConfig
  }

instance Default BindgenOpts where
  def = BindgenOpts {
      baseBootConfig      = def
    , extraIncludeDirs    = []
    , baseFrontendConfig  = def
    , safety              = SHs.Safe
    , baseBackendConfig   = def
    }

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
    let clangArgsConfig :: ClangArgsConfig
        clangArgsConfig = def {
            clangExtraIncludeDirs = CIncludeDir <$> includeDirs
          }
        bootConfig = baseBootConfig { bootClangArgsConfig = clangArgsConfig }
    backendConfig <- ensureUniqueId baseBackendConfig

    let bindgenConfig =
          BindgenConfig bootConfig baseFrontendConfig backendConfig

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
    let decls = mergeDecls safety decls'
        requiredExts = uncurry getExtensions $ decls
    checkLanguageExtensions requiredExts
    uncurry (getThDecls deps) decls
  where
    toFilePath :: FilePath -> IncludeDir -> FilePath
    toFilePath root (RelativeToPkgRoot x) = root </> x
    toFilePath _    (IncludeDir        x) = x

    toFilePaths :: [IncludeDir] -> TH.Q [FilePath]
    toFilePaths xs = do
      root <- getPackageRoot
      pure $ map (toFilePath root) xs

    ensureUniqueId :: BackendConfig -> TH.Q BackendConfig
    ensureUniqueId backendConfig = do
      let translationOpts = backendTranslationOpts backendConfig
      uniqueId <- updateUniqueId $ translationUniqueId translationOpts
      pure backendConfig{
          backendTranslationOpts =
            translationOpts{ translationUniqueId = uniqueId }
        }

    updateUniqueId :: UniqueId -> TH.Q UniqueId
    updateUniqueId uniqueId@(UniqueId val)
      | null val  = getUniqueId
      | otherwise = pure uniqueId

    getUniqueId :: TH.Q UniqueId
    getUniqueId = UniqueId . TH.loc_package <$> TH.location

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

-- NOTE: We could also check which enabled extension may interfere with the
-- generated code (e.g. Strict/Data).
checkLanguageExtensions :: Set TH.Extension -> TH.Q ()
checkLanguageExtensions requiredExts = do
    enabledExts <- Set.fromList <$> extsEnabled
    let missingExts  = requiredExts `Set.difference` enabledExts

    unless (null missingExts) $ do
      fail $ unlines $
        "Missing language extension(s): " :
          (map (("    - " ++) . show) (toList missingExts))
