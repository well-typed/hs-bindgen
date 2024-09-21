module HsBindgen.Bootstrap.Prelude (
    genPrelude
  ) where

import Control.Exception
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath ((</>))

import HsBindgen.C.Macro qualified as Macro
import HsBindgen.C.Parser
import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.Fold
import HsBindgen.Clang.Util.SourceLoc (SourceLoc(..))
import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
import HsBindgen.Clang.Util.Tokens qualified as Tokens
import HsBindgen.Patterns
import HsBindgen.Util.Tracer

import Paths_hs_bindgen

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data PreludeEntry
  deriving (Show)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

genPrelude :: Tracer IO Unrecognized -> IO ()
genPrelude tracer = do
    dataDir <- getDataDir
    let standardHeaders :: FilePath
        standardHeaders = (dataDir </> "bootstrap" </> "standard_headers.h")

    entries <- withTranslationUnit
        (show `contramap` mkTracerIO True)
        []
        standardHeaders $
        \unit -> do
      cursor <- clang_getTranslationUnitCursor unit
      clang_fold cursor $ fold tracer standardHeaders unit
    print entries

fold ::
     Tracer IO Unrecognized
  -> FilePath -- ^ Path to @standard_headers.h@
  -> CXTranslationUnit
  -> Fold PreludeEntry
fold tracer standardHeaders unit = go
  where
    go :: Fold PreludeEntry
    go = checkLoc $ \loc _parent current -> do
        kind <- clang_getCursorKind current
        case fromSimpleEnum kind of
          Right CXCursor_InclusionDirective ->
            return $ Continue Nothing
          Right CXCursor_MacroExpansion -> do
            putStrLn $ "Skipping macro expansion on " ++ show loc
            return $ Continue Nothing
          Right CXCursor_MacroDefinition -> do
            cursorExtent <- clang_getCursorExtent current
            tokens <- Tokens.clang_tokenize unit cursorExtent
            case Macro.parse tokens of
              Right macro ->
                appendFile "macros-recognized.log" (show (loc, macro) ++ "\n")
              Left err -> do
                appendFile "macros-unrecognized.log" (show (Macro.unrecognizedMacroTokens err) ++ "\n" ++ Macro.unrecognizedMacroError err ++ "\n")
            return $ Continue Nothing
          _otherwise ->
            Continue <$> unrecognized tracer current

    checkLoc :: (SourceLoc -> Fold a) -> Fold a
    checkLoc k parent current = do
        loc <- SourceLoc.clang_getCursorLocation current
        let fp :: FilePath
            fp = Text.unpack (SourceLoc.getSourcePath $ sourceLocFile loc)

        if | fp == "" ->
               -- TODO: Should we do anything with the macro definitions from
               -- @llvm@? (Things like @__GNUC__@, @__LITTLE_ENDIAN__@,
               -- @__INT_WIDTH__@, ..)
               return $ Continue Nothing

           | fp == standardHeaders ->
               -- Ignore our own file (we are only interested in the imports)
               return $ Continue Nothing

           | otherwise ->
               k loc parent current

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

data Unrecognized =
    UnrecognizedElement {
        unrecognizedKind :: SimpleEnum CXCursorKind
      , unrecognizedName :: Text
      , unrecognizedLoc  :: SourceLoc
      }
  deriving stock (Show)
  deriving anyclass (Exception)

unrecognized :: Tracer IO Unrecognized -> CXCursor -> IO (Maybe a)
unrecognized tracer current = do
    unrecognizedKind <- clang_getCursorKind current
    unrecognizedName <- clang_getCursorSpelling current
    unrecognizedLoc  <- SourceLoc.clang_getCursorLocation current
    traceWith tracer Error $ UnrecognizedElement {
        unrecognizedKind
      , unrecognizedName
      , unrecognizedLoc
      }
    return Nothing


