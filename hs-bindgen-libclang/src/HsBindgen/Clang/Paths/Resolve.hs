module HsBindgen.Clang.Paths.Resolve (
    resolveHeader
  , resolveHeader'
  ) where

import Control.Monad ((<=<), unless, when)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

import HsBindgen.Clang.Args
import HsBindgen.Clang.HighLevel (withTranslationUnit2)
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Clang.Paths
import HsBindgen.Runtime.Enum.Bitfield
import HsBindgen.Runtime.Enum.Simple

--------------------------------------------------------------------------------

-- | Resolve a header
resolveHeader :: ClangArgs -> CHeaderIncludePath -> IO (Maybe SourcePath)
resolveHeader args headerIncludePath =
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withUnsavedFile headerName headerContent $ \unsavedFile ->
        withTranslationUnit2 index headerSourcePath args [unsavedFile] opts $
          \case
            Left err -> fail $ show err
            Right unit -> do
              rootCursor <- clang_getTranslationUnitCursor unit
              listToMaybe <$> HighLevel.clang_visitChildren rootCursor visit
  where
    visit :: CXCursor -> IO (Next IO SourcePath)
    visit cursor = either return return <=< runExceptT $ do
      srcPath <- liftIO $
            fmap singleLocPath . HighLevel.clang_getExpansionLocation
        =<< clang_getCursorLocation cursor
      -- skip builtin macros
      when (nullSourcePath srcPath) $ throwError (Continue Nothing)
      -- only parse the generated header
      unless (srcPath == headerSourcePath) $
        throwError (Break Nothing)
      -- only parse the inclusion directive
      eCursorKind <- liftIO $ fromSimpleEnum <$> clang_getCursorKind cursor
      unless (eCursorKind == Right CXCursor_InclusionDirective) $
        throwError (Break Nothing)
      -- check that the inclusion directive is for the specified header
      displayName <- liftIO $ clang_getCursorDisplayName cursor
      unless (displayName == headerIncludePath') $ throwError (Break Nothing)
      path <- liftIO $ clang_getFileName =<< clang_getIncludedFile cursor
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

-- | Resolve a header, failing if not found
resolveHeader' :: ClangArgs -> CHeaderIncludePath -> IO SourcePath
resolveHeader' args headerIncludePath = do
    mSrc <- resolveHeader args headerIncludePath
    case mSrc of
      Just src -> return src
      Nothing  -> fail $
        "header not found: " ++ renderCHeaderIncludePath headerIncludePath
