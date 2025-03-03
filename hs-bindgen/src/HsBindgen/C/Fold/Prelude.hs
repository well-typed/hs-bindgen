module HsBindgen.C.Fold.Prelude (
    PreludeEntry
  , GenPreludeMsg
  , foldPrelude
  ) where

import HsBindgen.C.AST qualified as C
import HsBindgen.Imports
import HsBindgen.C.Reparse
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Clang.Paths
import HsBindgen.Runtime.Enum.Simple
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data PreludeEntry
  deriving (Show)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

foldPrelude :: forall m.
     MonadIO m
  => Tracer IO GenPreludeMsg
  -> Tracer IO (MultiLoc, C.Macro)
  -> CXTranslationUnit
  -> Fold m PreludeEntry
foldPrelude msgTracer macroTracer unit = go
  where
    go :: Fold m PreludeEntry
    go = checkLoc $ \loc current -> do
        kind <- liftIO $ clang_getCursorKind current

        let skip :: m (Next m a)
            skip = liftIO $ do
                traceWith msgTracer Info $ Skipping loc kind
                return $ Continue Nothing

        case fromSimpleEnum kind of
          Right CXCursor_InclusionDirective -> skip
          Right CXCursor_MacroExpansion     -> skip

          Right CXCursor_MacroDefinition -> do
            processMacro msgTracer macroTracer unit loc current
            return $ Continue Nothing

          _otherwise ->
            Continue <$> unrecognized msgTracer current

    checkLoc :: (MultiLoc -> Fold m a) -> Fold m a
    checkLoc k current = do
        loc <- liftIO $ HighLevel.clang_getCursorLocation current
        let fp :: FilePath
            fp = getSourcePath . singleLocPath $ multiLocExpansion loc

        if | fp == "" ->
               -- TODO: Should we do anything with the macro definitions from
               -- @llvm@? (Things like @__GNUC__@, @__LITTLE_ENDIAN__@,
               -- @__INT_WIDTH__@, ..)
               return $ Continue Nothing

           | otherwise ->
               k loc current

processMacro ::
     HasCallStack
  => MonadIO m
  => Tracer IO GenPreludeMsg
  -> Tracer IO (MultiLoc, C.Macro)
  -> CXTranslationUnit
  -> MultiLoc
  -> CXCursor
  -> m ()
processMacro msgTracer macroTracer unit loc current = liftIO $ do
    cursorExtent <- HighLevel.clang_getCursorExtent current
    tokens       <- HighLevel.clang_tokenize
                      unit
                      (multiLocExpansion <$> cursorExtent)
    case reparseWith reparseMacro tokens of
      Right macro -> traceWith macroTracer Info (loc, macro)
      Left err -> do
        traceWith msgTracer Warning $ UnrecognizedMacro err

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

unrecognized ::
     MonadIO m
  => Tracer IO GenPreludeMsg
  -> CXCursor
  -> m (Maybe a)
unrecognized msgTracer current = liftIO $ do
    loc  <- HighLevel.clang_getCursorLocation current
    kind <- clang_getCursorKind current
    name <- clang_getCursorSpelling current
    traceWith msgTracer Error $ UnrecognizedElement loc kind name
    return Nothing
