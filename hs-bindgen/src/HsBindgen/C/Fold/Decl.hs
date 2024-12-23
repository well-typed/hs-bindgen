-- | Translate @libclang@ declarations
module HsBindgen.C.Fold.Decl (
    DeclState(..)
  , initDeclState
  , foldDecls
  ) where

import Control.Monad.State
import Data.Text qualified as Text
import System.FilePath (takeFileName)

import HsBindgen.Imports
import HsBindgen.Eff
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
  => Maybe FilePath -- ^ Directory to make paths relative to
  -> Tracer IO Skipped
  -> Predicate
  -> CXTranslationUnit
  -> Fold (Eff (State DeclState)) Decl
foldDecls relPath tracer p unit = checkPredicate relPath tracer p $ \current -> do
    loc <- liftIO $ clang_getCursorLocation current
    sloc <- liftIO $ HighLevel.clang_getExpansionLocation relPath loc
    cursorKind <- liftIO $ clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_TypedefDecl -> typeDecl current
      Right CXCursor_StructDecl  -> typeDecl current
      Right CXCursor_EnumDecl    -> typeDecl current

      Right CXCursor_MacroDefinition -> do
        mbMExpr <- mkMacro relPath unit current
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
                return MacroDecl {
                    macroDeclMacro     = macro
                  , macroDeclMacroTy   = ty
                  , macroDeclSourceLoc = sloc
                  }
        return $ Continue $ Just $ DeclMacro macro
      Right CXCursor_MacroExpansion -> do
        mloc <- liftIO $ HighLevel.clang_getCursorLocation relPath current
        modify $ registerMacroExpansion mloc
        return $ Continue Nothing
      Right CXCursor_InclusionDirective ->
        -- The inclusion directive merely tells us that we are now going to
        -- process a #include-d file; we don't need to do anything special at
        -- this point so we can just ignore it (for each declaration we see we
        -- are anyway told from which file it originates, which we use for
        -- filtering).
        return $ Continue Nothing

      Right CXCursor_FunctionDecl -> do
        spelling <- liftIO $ clang_getCursorSpelling current
        ty <- liftIO $ clang_getCursorType current
        ty' <- processTypeDecl relPath unit ty
        (path, _, _) <- liftIO $ clang_getPresumedLocation loc

        return $ Continue $ Just $ DeclFunction $ Function
          { functionName      = CName spelling
          , functionType      = ty'
          , functionHeader    = takeFileName (Text.unpack path)
          , functionSourceLoc = sloc
          }

      Right CXCursor_VarDecl -> do
        -- TODO: extern int i;
        return $ Continue Nothing

      _otherwise -> do
        unrecognizedCursor relPath current
  where
    typeDecl :: CXCursor -> Eff (State DeclState) (Next m a)
    typeDecl current = do
      ty <- liftIO $ clang_getCursorType current
      -- TODO: add assert at ty is not invalid type.
      void $ processTypeDecl relPath unit ty
      return $ Continue Nothing

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

mkMacro ::
     MonadIO m
  => Maybe FilePath -- ^ Directory to make paths relative to
  -> CXTranslationUnit
  -> CXCursor
  -> m (Either ReparseError Macro)
mkMacro relPath unit current = liftIO $ do
    range  <- HighLevel.clang_getCursorExtent relPath current
    tokens <- HighLevel.clang_tokenize relPath unit (multiLocExpansion <$> range)
    return $ reparseWith reparseMacro tokens
