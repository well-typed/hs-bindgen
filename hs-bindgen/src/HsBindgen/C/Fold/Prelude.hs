module HsBindgen.C.Fold.Prelude (
    PreludeEntry
  , GenPreludeMsg
  , foldPrelude
  ) where

import Control.Exception
import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as Text

import HsBindgen.C.Reparse
import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.Fold
import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
import HsBindgen.Clang.Util.SourceLoc.Type
import HsBindgen.Clang.Util.Tokens qualified as Tokens
import HsBindgen.Patterns
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data PreludeEntry
  deriving (Show)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

foldPrelude ::
     Tracer IO GenPreludeMsg
  -> CXTranslationUnit
  -> Fold Identity PreludeEntry
foldPrelude tracer unit = go
  where
    go :: Fold Identity PreludeEntry
    go = checkLoc $ \loc current -> do
        kind <- liftIO $ clang_getCursorKind current

        let skip :: FoldM Identity (Next Identity a)
            skip = liftIO $ do
                traceWith tracer Info $ Skipping loc kind
                return $ Continue Nothing

        case fromSimpleEnum kind of
          Right CXCursor_InclusionDirective -> skip
          Right CXCursor_MacroExpansion     -> skip

          Right CXCursor_MacroDefinition -> do
            processMacro tracer unit loc current
            return $ Continue Nothing

          _otherwise ->
            Continue <$> unrecognized tracer current

    checkLoc :: (MultiLoc -> Fold m a) -> Fold m a
    checkLoc k current = do
        loc <- liftIO $ SourceLoc.clang_getCursorLocation current
        let fp :: FilePath
            fp = Text.unpack . getSourcePath . singleLocPath $
                   multiLocExpansion loc

        if | fp == "" ->
               -- TODO: Should we do anything with the macro definitions from
               -- @llvm@? (Things like @__GNUC__@, @__LITTLE_ENDIAN__@,
               -- @__INT_WIDTH__@, ..)
               return $ Continue Nothing

           | otherwise ->
               k loc current

processMacro ::
     MonadIO m
  => Tracer IO GenPreludeMsg
  -> CXTranslationUnit
  -> MultiLoc
  -> CXCursor -> m ()
processMacro tracer unit loc current = liftIO $ do
    cursorExtent <- SourceLoc.clang_getCursorExtent current
    tokens       <- Tokens.clang_tokenize
                      unit
                      (multiLocExpansion <$> cursorExtent)
    case reparseWith reparseMacro tokens of
      Right macro ->
        appendFile "macros-recognized.log" (show (loc, macro) ++ "\n")
      Left err -> do
        traceWith tracer Warning $ UnrecognizedMacro err

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

data GenPreludeMsg =
    Skipping MultiLoc (SimpleEnum CXCursorKind)
  | UnrecognizedElement MultiLoc (SimpleEnum CXCursorKind) Text
  | UnrecognizedMacro ReparseError
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

unrecognized :: MonadIO m => Tracer IO GenPreludeMsg -> CXCursor -> m (Maybe a)
unrecognized tracer current = liftIO $ do
    loc  <- SourceLoc.clang_getCursorLocation current
    kind <- clang_getCursorKind current
    name <- clang_getCursorSpelling current
    traceWith tracer Error $ UnrecognizedElement loc kind name
    return Nothing


