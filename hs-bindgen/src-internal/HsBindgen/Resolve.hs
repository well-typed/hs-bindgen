module HsBindgen.Resolve (
    -- * Trace messages
    ResolveHeaderMsg(..)
    -- * API
  , resolveHeader
  ) where

import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text

import Clang.Args
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.Clang
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint (hang, hsep, string)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ResolveHeaderMsg =
    ResolveHeaderClang ClangMsg
  | ResolveHeaderSuccess  HashIncludeArg SourcePath
  | ResolveHeaderNotFound HashIncludeArg
  deriving stock (Eq, Show)

instance HasDefaultLogLevel ResolveHeaderMsg where
  getDefaultLogLevel = \case
    ResolveHeaderClang x    -> getDefaultLogLevel x
    ResolveHeaderSuccess{}  -> Info
    ResolveHeaderNotFound{} -> Error

instance HasSource ResolveHeaderMsg where
  getSource = \case
    ResolveHeaderClang x    -> getSource x
    ResolveHeaderSuccess{}  -> HsBindgen
    ResolveHeaderNotFound{} -> HsBindgen

instance PrettyForTrace ResolveHeaderMsg where
  prettyForTrace = \case
    ResolveHeaderClang msg -> hang
      "during header resolution:" 2 (prettyForTrace msg)
    ResolveHeaderSuccess header path -> hsep [
        "header"
      , string $ getHashIncludeArg header
      , "resolved to"
      , string $ getSourcePath path
      ]
    ResolveHeaderNotFound header -> hsep [
        "header"
      , string $ getHashIncludeArg header
      , "not found"
      ]

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Resolve a header
resolveHeader ::
     Tracer IO ResolveHeaderMsg
  -> ClangArgs
  -> HashIncludeArg -- ^ The header we want to resolve
  -> IO (Maybe SourcePath)
resolveHeader tracer args target =
    withClang (contramap ResolveHeaderClang tracer) setup $ \unit -> do
      rootCursor <- clang_getTranslationUnitCursor unit
      mPath <- listToMaybe <$> HighLevel.clang_visitChildren rootCursor visit
      traceWith tracer $
        maybe (ResolveHeaderNotFound target) (ResolveHeaderSuccess target) mPath
      return mPath
  where
    setup :: ClangSetup
    setup = defaultClangSetup args $ ClangInputMemory "hs-bindgen-resolve.h" $
        case target of
          CHeaderSystemIncludePath path -> "#include <"  ++ path ++ ">"
          CHeaderQuoteIncludePath  path -> "#include \"" ++ path ++ "\""

    visit :: Fold IO SourcePath
    visit = simpleFold $ \curr -> do
        mResolved <- tryResolve target curr
        case mResolved of
          Nothing   -> foldContinue
          Just path -> foldBreakWith path

{-------------------------------------------------------------------------------
  Internal: look for the relevant part of the clang AST
-------------------------------------------------------------------------------}

-- | Try to resolve the target using the current cursor, if possible
tryResolve ::
     HashIncludeArg  -- ^ Path we want to resolve
  -> CXCursor
  -> IO (Maybe SourcePath)
tryResolve target curr = runMaybeT $ do
    srcPath     <- singleLocPath  <$> HighLevel.clang_getCursorLocation' curr
    eCursorKind <- fromSimpleEnum <$> clang_getCursorKind curr
    displayName <- clang_getCursorDisplayName curr
    path        <- clang_getFileName =<< clang_getIncludedFile curr

    -- Skip builtin macros
    when (nullSourcePath srcPath) $
      cannotResolve
    -- Only parse the generated header
    unless (srcPath == SourcePath (Text.pack "hs-bindgen-resolve.h")) $
      cannotResolve
    -- Only parse the inclusion directive
    unless (eCursorKind == Right CXCursor_InclusionDirective) $
      cannotResolve
    -- Check that the inclusion directive is for the specified header
    unless (displayName == target') $
      cannotResolve
    -- Check that the included header was found
    when (Text.null path) $
      cannotResolve

    return $ SourcePath path
  where
    target' :: Text
    target' = Text.pack $ getHashIncludeArg target

    cannotResolve :: MaybeT IO ()
    cannotResolve = MaybeT $ return Nothing
