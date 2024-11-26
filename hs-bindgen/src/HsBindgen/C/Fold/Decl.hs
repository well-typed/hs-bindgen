-- | Translate @libclang@ declarations
module HsBindgen.C.Fold.Decl (
    DeclState(..)
  , initDeclState
  , foldDecls
  ) where

import Control.Monad.State

import HsBindgen.Imports
import HsBindgen.C.AST
import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.DeclState
import HsBindgen.C.Fold.Type
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.C.Reparse
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Patterns
import HsBindgen.Util.Tracer
import HsBindgen.C.Tc.Macro (tcMacro)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

foldDecls ::
     HasCallStack
  => Tracer IO Skipped
  -> Predicate
  -> CXTranslationUnit
  -> Fold (State DeclState) Decl
foldDecls tracer p unit = checkPredicate tracer p $ \current -> do
    cursorKind <- liftIO $ clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_StructDecl -> do
        -- Emit struct declration only if it's the definition.
        defn <- liftIO $ clang_getCursorDefinition current
        if defn == nullCursor
        then do
          mtag <- liftIO $ fmap CName . getUserProvided <$> HighLevel.clang_getCursorSpelling current
          return $ Continue $ case mtag of
            Nothing -> Nothing
            Just tag -> Just (DeclOpaqueStruct tag)
        else do
          if current == defn
          then do
            decl <- declStruct <$> mkStructHeader current
            return $ Recurse (continue $ mkStructField unit) (Just . decl)
          else do
            -- not a definition declaration, skip.
            return $ Continue Nothing

      Right CXCursor_EnumDecl -> do
        decl <- declEnum <$> mkEnumHeader current
        return $ Recurse (continue mkEnumValue) (Just . decl)
      Right CXCursor_TypedefDecl -> do
        typedef <- mkTypedef current
        return $ Continue (Just (DeclTypedef typedef))
      Right CXCursor_MacroDefinition -> do
        mbMExpr <- mkMacro unit current
        macro <- case mbMExpr of
          Left err -> return $ MacroReparseError err
          Right macro@( Macro _ mVar mArgs mExpr ) -> do
            macroTyEnv <- macroTypes <$> get
            let tcRes = tcMacro macroTyEnv mVar mArgs mExpr
            case tcRes of
              Left err ->
                return $ MacroTcError macro err
              Right ty -> do
                modify $ registerMacroType mVar ty
                return $ MacroDecl macro ty
        return $ Continue $ Just $ DeclMacro macro
      Right CXCursor_MacroExpansion -> do
        loc <- liftIO $ HighLevel.clang_getCursorLocation current
        modify $ registerMacroExpansion loc
        return $ Continue Nothing
      Right CXCursor_InclusionDirective ->
        -- The inclusion directive merely tells us that we are now going to
        -- process a #include-d file; we don't need to do anything special at
        -- this point so we can just ignore it (for each declaration we see we
        -- are anyway told from which file it originates, which we use for
        -- filtering).
        return $ Continue Nothing

      Right CXCursor_FunctionDecl -> do
        -- TODO: function declaration
        return $ Continue Nothing

      Right CXCursor_VarDecl -> do
        -- TODO: extern int i;
        return $ Continue Nothing

      _otherwise -> do
        unrecognizedCursor current


{-------------------------------------------------------------------------------
  Type declarations
-------------------------------------------------------------------------------}

declStruct :: ([StructField] -> Struct) -> [Maybe StructField] -> Decl
declStruct partial = DeclStruct . partial . catMaybes

declEnum :: ([EnumValue] -> Enu) -> [Maybe EnumValue] -> Decl
declEnum partial = DeclEnum . partial . catMaybes

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

mkMacro ::
     MonadIO m
  => CXTranslationUnit -> CXCursor -> m (Either ReparseError Macro)
mkMacro unit current = liftIO $ do
    range  <- HighLevel.clang_getCursorExtent current
    tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
    return $ reparseWith reparseMacro tokens
