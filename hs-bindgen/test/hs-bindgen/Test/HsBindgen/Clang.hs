-- | Running @libclang@ over header text held in a string
--
-- There is no file on disk and no golden file: the header is handed to
-- @libclang@ as an unsaved file, so a test can write the C it needs inline.
module Test.HsBindgen.Clang (
    -- * Calling clang
    ClangError (..)
  , withClang
  , withClang'
    -- * Tokenization
  , tokenize
  , collectMacroTokens
  ) where

import Control.Exception (Exception, throwIO)
import Data.Text (Text)
import Data.Text qualified as Text

import Clang.Args (ClangArgs)
import Clang.Enum.Bitfield (BitfieldEnum, bitfieldEnum)
import Clang.Enum.Simple (SimpleEnum, fromSimpleEnum)
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types (Diagnostic, Fold, MultiLoc (multiLocExpansion),
                              Token, TokenSpelling, diagnosticIsError,
                              foldContinue, foldContinueWith, simpleFold)
import Clang.LowLevel.Core (CXCursorKind (CXCursor_MacroDefinition),
                            CXErrorCode, CXIndex, CXTranslationUnit,
                            CXTranslationUnit_Flags (CXTranslationUnit_DetailedPreprocessingRecord),
                            CXUnsavedFile,
                            DisplayDiagnostics (DontDisplayDiagnostics),
                            clang_Location_isFromMainFile, clang_getCursorKind,
                            clang_getCursorLocation, clang_getCursorSpelling,
                            clang_getTranslationUnitCursor)
import Clang.Paths (SourcePath (SourcePath))

{-------------------------------------------------------------------------------
  Calling clang
-------------------------------------------------------------------------------}

data ClangError =
    ClangErrorDiagnostics [Diagnostic]
  | ClangErrorCode ErrorCode
  deriving stock (Show, Eq)
  deriving anyclass Exception

type ErrorCode = SimpleEnum CXErrorCode

-- | Parse the given header contents, throwing 'ClangError' on failure
--
-- The preprocessing record is always requested, so macro definitions are
-- visible in the translation unit.
withClang :: forall a.
     ClangArgs
  -> String
  -> (CXTranslationUnit -> IO a)
  -> IO a
withClang args contents k = do
    mRes <- withClang' args contents $ \unit -> do
      diags <- HighLevel.clang_getDiagnostics unit Nothing
      if any diagnosticIsError diags
      then throwIO $ ClangErrorDiagnostics diags
      else k unit
    case mRes of
      Left e    -> throwIO $ ClangErrorCode e
      Right res -> pure res

-- | Variant of 'withClang' that reports the error code rather than throwing,
-- and that does not inspect the diagnostics
withClang' :: forall a.
     ClangArgs
  -> String
  -> (CXTranslationUnit -> IO a)
  -> IO (Either ErrorCode a)
withClang' args contents k =
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
    HighLevel.withUnsavedFile path contents $ \file  ->
      withUnit index file
  where
    flags :: BitfieldEnum CXTranslationUnit_Flags
    flags = bitfieldEnum [CXTranslationUnit_DetailedPreprocessingRecord]

    path :: FilePath
    path = "virtual.h"

    onErrorCode :: ErrorCode -> IO (Either ErrorCode a)
    onErrorCode err = pure $ Left err

    withUnit :: CXIndex -> CXUnsavedFile -> IO (Either ErrorCode a)
    withUnit index unsaved =
      HighLevel.withTranslationUnit2
        index
        (Just $ SourcePath $ Text.pack path)
        args
        [unsaved]
        flags
        onErrorCode
        (fmap Right . k)

{-------------------------------------------------------------------------------
  Tokenization
-------------------------------------------------------------------------------}

-- | Tokenize the whole header
--
-- To tokenize individual declarations instead, visit the children of the
-- translation unit cursor; 'collectMacroTokens' does that for macros.
tokenize :: ClangArgs -> String -> IO [Token TokenSpelling]
tokenize args contents = withClang args contents $ \unit -> do
    root  <- clang_getTranslationUnitCursor unit
    range <- HighLevel.clang_getCursorExtent root
    HighLevel.clang_tokenize unit (multiLocExpansion <$> range)

-- | The name and tokens of every @#define@ in the header, in source order
--
-- The tokens are those of the macro definition cursor, which start at the macro
-- name: the @#define@ itself is not part of them. This is the same token stream
-- that the @Parse@ pass feeds to 'HsBindgen.Macro.Syntax.splitMacro'.
collectMacroTokens ::
     ClangArgs
  -> String
  -> IO [(Text, [Token TokenSpelling])]
collectMacroTokens args contents = withClang args contents $ \unit -> do
    root <- clang_getTranslationUnitCursor unit
    HighLevel.clang_visitChildren root (macroFold unit)

macroFold ::
     CXTranslationUnit
  -> Fold IO (Text, [Token TokenSpelling])
macroFold unit = simpleFold $ \cursor -> do
    loc    <- clang_getCursorLocation cursor
    inMain <- clang_Location_isFromMainFile loc
    kind   <- fromSimpleEnum <$> clang_getCursorKind cursor
    case kind of
      Right CXCursor_MacroDefinition | inMain -> do
        name   <- clang_getCursorSpelling cursor
        range  <- HighLevel.clang_getCursorExtent cursor
        tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
        foldContinueWith (name, tokens)
      _otherwise ->
        foldContinue
