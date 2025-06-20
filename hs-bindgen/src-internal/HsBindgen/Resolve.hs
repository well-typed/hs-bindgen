module HsBindgen.Resolve (
    -- * Error type
    ResolveHeaderException(..)
    -- * API
  , resolveHeader'
  , resolveHeader
  ) where

import Control.Exception (Exception (displayException))
import Control.Monad ((<=<))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text

import Clang.Args
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import Control.Tracer (Tracer)
import HsBindgen.Clang.Args (ExtraClangArgsLog, withExtraClangArgs)
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer (HasDefaultLogLevel (getDefaultLogLevel),
                              HasSource (getSource), Level (..),
                              PrettyTrace (prettyTrace), Source (HsBindgen),
                              TraceWithCallStack)

{-------------------------------------------------------------------------------
  Error type
-------------------------------------------------------------------------------}

-- | Failed to resolve a header
newtype ResolveHeaderException =
    ResolveHeaderNotFound CHeaderIncludePath
  deriving stock (Eq, Ord, Show)

instance Exception ResolveHeaderException where
  displayException = \case
    ResolveHeaderNotFound headerIncludePath ->
      "header not found: " ++ getCHeaderIncludePath headerIncludePath

instance PrettyTrace ResolveHeaderException where
  prettyTrace = displayException

instance HasDefaultLogLevel ResolveHeaderException where
  -- 'Warning', not 'Error'; see https://github.com/well-typed/hs-bindgen/issues/479.
  getDefaultLogLevel = const Warning

instance HasSource ResolveHeaderException where
  getSource = const HsBindgen

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Resolve a header
resolveHeader' ::
     Tracer IO (TraceWithCallStack ExtraClangArgsLog)
  -> ClangArgs
  -> CHeaderIncludePath -- ^ The header we want to resolve
  -> IO (Either ResolveHeaderException SourcePath)
resolveHeader' tracer args target =
    withExtraClangArgs tracer args                           $ \args' ->
    HighLevel.withIndex DontDisplayDiagnostics               $ \index ->
    HighLevel.withUnsavedFile generatedName generatedContent $ \file  ->
    withTranslationUnit index args' file                     $ \unit  -> do
      rootCursor <- clang_getTranslationUnitCursor unit
      maybe (Left (ResolveHeaderNotFound target)) Right
        .   listToMaybe
        <$> HighLevel.clang_visitChildren rootCursor visit
  where
    onFailure :: SimpleEnum CXErrorCode -> IO a
    onFailure err = panicPure $
           "Clang parse translation unit error during header resolution: "
        ++ show err

    withTranslationUnit ::
         CXIndex
      -> ClangArgs
      -> CXUnsavedFile
      -> (CXTranslationUnit -> IO a)
      -> IO a
    withTranslationUnit index args' file =
        HighLevel.withTranslationUnit2
          index
          (Just $ SourcePath $ Text.pack generatedName)
          args'
          [file]
          opts
          onFailure

    visit :: Fold IO SourcePath
    visit = simpleFold $ \curr -> do
        mResolved <- tryResolve generatedName target curr
        case mResolved of
          Nothing   -> foldContinue
          Just path -> foldBreakWith path

    generatedName :: FilePath
    generatedName = "hs-bindgen-resolve.h"

    generatedContent :: String
    generatedContent = case target of
      CHeaderSystemIncludePath path -> "#include <"  ++ path ++ ">"
      CHeaderQuoteIncludePath  path -> "#include \"" ++ path ++ "\""

    opts :: BitfieldEnum CXTranslationUnit_Flags
    opts = bitfieldEnum [CXTranslationUnit_DetailedPreprocessingRecord]

-- | Resolve a header, throwing an 'HsBindgenException' on error
resolveHeader :: Tracer IO (TraceWithCallStack ExtraClangArgsLog)
              -> ClangArgs
              -> CHeaderIncludePath
              -> IO SourcePath
resolveHeader tracer args =
    either (throwIO . HsBindgenException) return <=< resolveHeader' tracer args

{-------------------------------------------------------------------------------
  Internal: look for the relevant part of the clang AST
-------------------------------------------------------------------------------}

-- | Try to resolve the target using the current cursor, if possible
tryResolve ::
     FilePath            -- ^ Generated header
  -> CHeaderIncludePath  -- ^ Path we want to resolve
  -> CXCursor
  -> IO (Maybe SourcePath)
tryResolve generatedName target curr = runMaybeT $ do
    srcPath     <- singleLocPath  <$> HighLevel.clang_getCursorLocation' curr
    eCursorKind <- fromSimpleEnum <$> clang_getCursorKind curr
    displayName <- clang_getCursorDisplayName curr
    path        <- clang_getFileName =<< clang_getIncludedFile curr

    -- Skip builtin macros
    when (nullSourcePath srcPath) $
      cannotResolve
    -- Only parse the generated header
    unless (srcPath == SourcePath (Text.pack generatedName)) $
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
    target' = Text.pack $ getCHeaderIncludePath target

    cannotResolve :: MaybeT IO ()
    cannotResolve = MaybeT $ return Nothing
