-- | Translate @libclang@ declarations
module HsBindgen.C.Fold.Decl (
    DeclState(..)
  , initDeclState
  , foldDecls
  ) where

import Control.Monad.State

import HsBindgen.Imports
import HsBindgen.Eff
import HsBindgen.Errors
import HsBindgen.C.AST
import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.DeclState
import HsBindgen.C.Fold.Type
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.C.Reparse
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Clang.Paths
import HsBindgen.ExtBindings
import HsBindgen.Runtime.Enum.Simple
import HsBindgen.Util.Compat ((!?))
import HsBindgen.Util.Tracer
import HsBindgen.C.Tc.Macro (tcMacro)
import C.Type (hostPlatform)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

foldDecls ::
     HasCallStack
  => Tracer IO Skipped
  -> Predicate
  -> ExtBindings
  -> [CHeaderIncludePath]
  -> CXTranslationUnit
  -> Fold (Eff (State DeclState)) Decl
foldDecls tracer p extBindings headerIncludePaths unit current = do
    loc <- liftIO $ clang_getCursorLocation current
    sloc <- liftIO $ HighLevel.clang_getExpansionLocation loc
    eCursorKind <- liftIO $ fromSimpleEnum <$> clang_getCursorKind current

    -- process include directives even when predicate does not match
    when (eCursorKind == Right CXCursor_InclusionDirective) $ do
      -- update the include graph
      incHeader <- liftIO $
            fmap SourcePath . clang_getFileName
        =<< clang_getIncludedFile current
      modify $ registerInclude (singleLocPath sloc) incHeader

      -- update the current main header
      isFromMainFile <- liftIO $ clang_Location_isFromMainFile loc
      when isFromMainFile $
        case headerIncludePaths !? (singleLocLine sloc - 1) of
          Just headerIncludePath ->
            modify $ registerMainHeader headerIncludePath incHeader
          Nothing -> panicIO "root header unknown include"

    mHeader <- gets currentMainHeader
    whenPredicateMatches tracer p mHeader current sloc $ \headerIncludePath ->
      case eCursorKind of
        Right CXCursor_TypedefDecl -> typeDecl
        Right CXCursor_StructDecl  -> typeDecl
        Right CXCursor_EnumDecl    -> typeDecl
        Right CXCursor_UnionDecl   -> typeDecl

        Right CXCursor_MacroDefinition ->
          if isBuiltinMacro sloc
            then return $ Continue Nothing
            else do
              mbMExpr <- mkMacro unit current
              macro <- case mbMExpr of
                Left err -> return $ MacroReparseError err
                Right macro@( Macro _ mVar mArgs mExpr ) -> do
                  macroTyEnv <- macroTypes <$> get
                  let tcRes = tcMacro hostPlatform macroTyEnv mVar mArgs mExpr
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

        Right CXCursor_InclusionDirective ->
          -- no children, recurse and continue have same behavior
          return $ Continue Nothing

        Right CXCursor_FunctionDecl -> do
          spelling <- liftIO $ clang_getCursorSpelling current
          ty <- liftIO $ clang_getCursorType current
          ty' <- processTypeDecl extBindings unit (Just current) ty

          return $ Continue $ Just $ DeclFunction $ Function
            { functionName      = CName spelling
            , functionType      = ty'
            , functionHeader    = headerIncludePath
            , functionSourceLoc = sloc
            }

        Right CXCursor_VarDecl -> do
          -- TODO: extern int i;
          return $ Continue Nothing

        _otherwise -> do
          unrecognizedCursor current
  where
    typeDecl :: Eff (State DeclState) (Next m a)
    typeDecl = do
      ty <- liftIO $ clang_getCursorType current
      -- TODO: add assert at ty is not invalid type.
      void $ processTypeDecl extBindings unit ( Just current ) ty
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
isBuiltinMacro = nullSourcePath . singleLocPath

mkMacro ::
     HasCallStack
  => MonadIO m
  => CXTranslationUnit
  -> CXCursor
  -> m (Either ReparseError Macro)
mkMacro unit current = liftIO $ do
    range  <- HighLevel.clang_getCursorExtent current
    tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
    return $ reparseWith reparseMacro tokens
