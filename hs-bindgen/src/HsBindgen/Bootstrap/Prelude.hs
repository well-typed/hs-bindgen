module HsBindgen.Bootstrap.Prelude (
    genPrelude
  ) where

import Control.Exception
import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath ((</>))

import HsBindgen.C.Parser
import HsBindgen.C.Parser.Macro (UnrecognizedMacro)
import HsBindgen.C.Parser.Macro qualified as Macro
import HsBindgen.Clang.Args
import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.Fold
import HsBindgen.Clang.Util.SourceLoc.Type
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

genPrelude :: Tracer IO GenPreludeMsg -> IO ()
genPrelude tracer = do
    dataDir <- getDataDir
    let standardHeaders :: FilePath
        standardHeaders = (dataDir </> "bootstrap" </> "standard_headers.h")

    entries <- withTranslationUnit
        (show `contramap` mkTracerIO True)
        defaultClangArgs
        standardHeaders $
        \unit -> do
      cursor <- clang_getTranslationUnitCursor unit
      clang_fold cursor $ fold tracer standardHeaders unit
    print entries

fold ::
     Tracer IO GenPreludeMsg
  -> FilePath -- ^ Path to @standard_headers.h@
  -> CXTranslationUnit
  -> Fold PreludeEntry
fold tracer standardHeaders unit = go
  where
    go :: Fold PreludeEntry
    go = checkLoc $ \loc _parent current -> do
        kind <- clang_getCursorKind current

        let skip :: IO (Next a)
            skip = do
                traceWith tracer Info $ Skipping loc kind
                return $ Continue Nothing

        case fromSimpleEnum kind of
          Right CXCursor_InclusionDirective -> skip
          Right CXCursor_MacroExpansion     -> skip

          Right CXCursor_MacroDefinition -> do
            cursorExtent <- SourceLoc.clang_getCursorExtent current
            tokens       <- Tokens.clang_tokenize
                              unit
                              (multiLocExpansion <$> cursorExtent)
            case Macro.parse tokens of
              Right macro ->
                appendFile "macros-recognized.log" (show (loc, macro) ++ "\n")
              Left err -> do
                traceWith tracer Warning $ UnrecognizedMacro err
            return $ Continue Nothing

          _otherwise ->
            Continue <$> unrecognized tracer current

    checkLoc :: (MultiLoc -> Fold a) -> Fold a
    checkLoc k parent current = do
        loc <- SourceLoc.clang_getCursorLocation current
        let fp :: FilePath
            fp = Text.unpack . getSourcePath . singleLocPath $
                   multiLocExpansion loc

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

data GenPreludeMsg =
    Skipping MultiLoc (SimpleEnum CXCursorKind)
  | UnrecognizedElement MultiLoc (SimpleEnum CXCursorKind) Text
  | UnrecognizedMacro UnrecognizedMacro
  deriving stock (Show)
  deriving anyclass (Exception)

instance PrettyLogMsg GenPreludeMsg where
  prettyLogMsg (Skipping loc kind) = concat [
        "Skipped "
      , show kind
      , " at "
      , show loc
      ]

  prettyLogMsg (UnrecognizedElement loc kind name) = concat [
        show name
      , ": unrecognized "
      , show kind
      , " at "
      , show loc
      ]

  prettyLogMsg (UnrecognizedMacro err) =
      prettyLogMsg err

unrecognized :: Tracer IO GenPreludeMsg -> CXCursor -> IO (Maybe a)
unrecognized tracer current = do
    loc  <- SourceLoc.clang_getCursorLocation current
    kind <- clang_getCursorKind current
    name <- clang_getCursorSpelling current
    traceWith tracer Error $ UnrecognizedElement loc kind name
    return Nothing


