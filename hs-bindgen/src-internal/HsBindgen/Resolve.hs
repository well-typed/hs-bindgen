module HsBindgen.Resolve (
    -- * Error type
    ResolveHeaderException(..)
    -- * API
  , resolveHeader'
  , resolveHeader
  ) where

import Control.Exception (Exception (displayException))
import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT, throwError)
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
  -> CHeaderIncludePath
  -> IO (Either ResolveHeaderException SourcePath)
resolveHeader' tracer args headerIncludePath =
  withExtraClangArgs tracer args $ \args' ->
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withUnsavedFile headerName headerContent $ \file -> do
        let onFailure :: SimpleEnum CXErrorCode -> IO a
            onFailure err = panicPure $
                   "Clang parse translation unit error during header resolution: "
                ++ show err
        HighLevel.withTranslationUnit2 index (Just headerSourcePath) args' [file] opts onFailure $ \unit -> do
          rootCursor <- clang_getTranslationUnitCursor unit
          maybe (Left (ResolveHeaderNotFound headerIncludePath)) Right
            .   listToMaybe
            <$> HighLevel.clang_visitChildren rootCursor visit
  where
    visit :: CXCursor -> IO (Next IO SourcePath)
    visit cursor = either return return <=< runExceptT $ do
      srcPath <-
            fmap singleLocPath . HighLevel.clang_getExpansionLocation
        =<< clang_getCursorLocation cursor
      -- skip builtin macros
      when (nullSourcePath srcPath) $ throwError (Continue Nothing)
      -- only parse the generated header
      unless (srcPath == headerSourcePath) $
        throwError (Break Nothing)
      -- only parse the inclusion directive
      eCursorKind <- fromSimpleEnum <$> clang_getCursorKind cursor
      unless (eCursorKind == Right CXCursor_InclusionDirective) $
        throwError (Break Nothing)
      -- check that the inclusion directive is for the specified header
      displayName <- clang_getCursorDisplayName cursor
      unless (displayName == headerIncludePath') $ throwError (Break Nothing)
      path <- clang_getFileName =<< clang_getIncludedFile cursor
      -- check that the included header was found
      when (Text.null path) $ throwError (Break Nothing)
      return $ Break (Just (SourcePath path))

    headerIncludePath' :: Text
    headerIncludePath' = Text.pack $ getCHeaderIncludePath headerIncludePath

    headerName :: FilePath
    headerName = "hs-bindgen-resolve.h"

    headerSourcePath :: SourcePath
    headerSourcePath = SourcePath $ Text.pack headerName

    headerContent :: String
    headerContent = case headerIncludePath of
      CHeaderSystemIncludePath path -> "#include <" ++ path ++ ">"
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
