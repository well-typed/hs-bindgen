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
import HsBindgen.Ref
import HsBindgen.Runtime.Enum.Simple
import HsBindgen.Util.Tracer
import HsBindgen.C.Tc.Macro (tcMacro)
import C.Type (buildPlatform)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

foldDecls ::
     HasCallStack
  => Tracer IO Skipped
  -> Predicate
  -> CXTranslationUnit
  -> Fold (Eff (State DeclState)) Decl
foldDecls tracer p unit = checkPredicate tracer p $ \current -> do
    loc <- liftIO $ clang_getCursorLocation current
    sloc <- liftIO $ HighLevel.clang_getExpansionLocation loc
    cursorKind <- liftIO $ clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_TypedefDecl -> typeDecl current
      Right CXCursor_StructDecl  -> typeDecl current
      Right CXCursor_EnumDecl    -> typeDecl current

      Right CXCursor_MacroDefinition ->
        if isBuiltinMacro sloc
          then return $ Continue Nothing
          else do
            mbMExpr <- mkMacro unit current
            macro <- case mbMExpr of
              Left err -> return $ MacroReparseError err
              Right macro@( Macro _ mVar mArgs mExpr ) -> do
                macroTyEnv <- macroTypes <$> get
                let tcRes = tcMacro buildPlatform macroTyEnv mVar mArgs mExpr
                  -- TODO (cross-compilation): use of 'buildPlatform'.
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
        mloc <- liftIO $ HighLevel.clang_getCursorLocation current
        modify $ registerMacroExpansion mloc
        return $ Continue Nothing

      Right CXCursor_InclusionDirective -> do
        let header = CHeaderAbsPath $ getSourcePath (singleLocPath sloc)
        incHeader <- liftIO $
              fmap CHeaderAbsPath . clang_getFileName
          =<< clang_getIncludedFile current
        modify $ registerInclude header incHeader
        return $ Continue Nothing

      Right CXCursor_FunctionDecl -> do
        spelling <- liftIO $ clang_getCursorSpelling current
        ty <- liftIO $ clang_getCursorType current
        ty' <- processTypeDecl unit ty
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
        unrecognizedCursor current
  where
    typeDecl :: CXCursor -> Eff (State DeclState) (Next m a)
    typeDecl current = do
      ty <- liftIO $ clang_getCursorType current
      -- TODO: add assert at ty is not invalid type.
      void $ processTypeDecl unit ty
      return $ Continue Nothing

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

-- | Determine if a macro definition is for a builtin macro from its source
-- location
--
-- The source path for builtin macros is the empty string.
--
-- This hack is necessary because @clang_Cursor_isMacroBuiltin@ is not working.
isBuiltinMacro :: SingleLoc -> Bool
isBuiltinMacro = Text.null . getSourcePath . singleLocPath

mkMacro ::
     MonadIO m
  => CXTranslationUnit
  -> CXCursor
  -> m (Either ReparseError Macro)
mkMacro unit current = liftIO $ do
    range  <- HighLevel.clang_getCursorExtent current
    tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
    return $ reparseWith reparseMacro tokens
