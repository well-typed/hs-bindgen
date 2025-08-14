{-# LANGUAGE CPP #-}

module HsBindgen.TH.Internal (
    -- * Template Haskell API
    IncludeDir (..)
  , BindgenOpts ( extraIncludeDirs
                , baseConfig
                , bindingSpecConfig
                , tracerConfig
                )
  , tracerConfigDefQ
  , withHsBindgen
  , hashInclude
    -- * Artefacts
  , getExtensions

    -- * Internal
  , genBindingsFromCHeader
  ) where

import Control.Monad.State (State, execState, modify)
import Data.Set qualified as Set
import Language.Haskell.TH qualified as TH
import System.FilePath ((</>))

import Clang.Args
import Clang.Paths
import HsBindgen
import HsBindgen.Backend.Artefact.TH.Translation
import HsBindgen.Backend.Extensions
import HsBindgen.Backend.Hs.Translation
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.UniqueId
import HsBindgen.BindingSpec (BindingSpecConfig)
import HsBindgen.Config (Config (..))
import HsBindgen.Frontend.RootHeader
import HsBindgen.Guasi
import HsBindgen.Imports
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
    extraIncludeDirs  :: [IncludeDir]
  , baseConfig        :: Config
    -- * Binding specifications
  , bindingSpecConfig :: BindingSpecConfig
    -- * Tracer
  , tracerConfig      :: TracerConfig TH.Q Level TraceMsg
  }

instance Default BindgenOpts where
  def = BindgenOpts {
      extraIncludeDirs  = []
    , baseConfig        = def
    , bindingSpecConfig = def
    , tracerConfig      = tracerConfigDefQ
    }

-- | The default tracer configuration in Q has verbosity 'Notice' and uses
-- 'outputConfigQ'.
tracerConfigDefQ :: TracerConfig TH.Q Level TraceMsg
tracerConfigDefQ = def {
        tVerbosity = Verbosity Notice
      , tOutputConfig = outputConfigQ
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
    let clangArgs :: ClangArgs
        clangArgs = def {
            clangExtraIncludeDirs = CIncludeDir <$> includeDirs
          }
    config <- ensureUniqueId $ baseConfig { configClangArgs = clangArgs}

    let -- Traverse #include directives.
        bindgenState :: BindgenState
        bindgenState = execState hashIncludes (BindgenState [])

        -- Restore original order of include directives.
        uncheckedHashIncludeArgs :: [UncheckedHashIncludeArg]
        uncheckedHashIncludeArgs =
          reverse $ bindgenStateUncheckedHashIncludeArgs bindgenState
    let artefacts = Dependencies :* FinalDecls :* getExtensions :* Nil
    (I deps :* I decls :* I requiredExts :* Nil) <-
      hsBindgenQ
        tracerConfig
        config
        bindingSpecConfig
        uncheckedHashIncludeArgs
        artefacts
    genBindingsFromCHeader deps decls requiredExts
  where
    toFilePath :: FilePath -> IncludeDir -> FilePath
    toFilePath root (RelativeToPkgRoot x) = root </> x
    toFilePath _    (IncludeDir        x) = x

    toFilePaths :: [IncludeDir] -> TH.Q [FilePath]
    toFilePaths xs = do
      root <- getPackageRoot
      pure $ map (toFilePath root) xs

    ensureUniqueId :: Config -> TH.Q Config
    ensureUniqueId config = do
      let translationOpts = configTranslation config
      uniqueId <- updateUniqueId $ translationUniqueId translationOpts
      pure config{
          configTranslation = translationOpts{ translationUniqueId = uniqueId }
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
  Artefacts
-------------------------------------------------------------------------------}

-- | Get required extensions.
getExtensions :: Artefact (Set TH.Extension)
getExtensions =
  Lift
    (FinalDecls :* Nil)
    (\(I decls :* Nil) -> pure $ foldMap requiredExtensions decls)

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

-- | Internal API; exported for testing; non-IO part of 'withHsBindgen'
genBindingsFromCHeader
    :: Guasi q
    => [SourcePath]
    -> [SHs.SDecl]
    -> Set TH.Extension
    -> q [TH.Dec]
genBindingsFromCHeader deps decls requiredExts = do
    -- Record dependencies, including transitively included headers.
    mapM_ (addDependentFile . getSourcePath) deps

    -- Check language extensions.
    --
    -- TODO: We could also check which enabled extension may interfere with the
    -- generated code (e.g. Strict/Data).
    enabledExts <- Set.fromList <$> extsEnabled
    let missingExts  = requiredExts `Set.difference` enabledExts
    -- TODO: The following error should be a trace.
    unless (null missingExts) $ do
      reportError $ "Missing LANGUAGE extensions: "
        ++ unwords (map show (toList missingExts))

    -- Generate TH declarations.
    fmap concat $ traverse mkDecl decls

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
