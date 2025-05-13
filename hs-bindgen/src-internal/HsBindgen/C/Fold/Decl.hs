-- | Translate @libclang@ declarations
module HsBindgen.C.Fold.Decl (
    DeclState(..)
  , initDeclState
  , foldDecls
  ) where

import Control.Monad.State
import Data.List.Compat ((!?))
import Data.Vec.Lazy qualified as Vec

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.BindingSpecs
import HsBindgen.C.AST
import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.DeclState
import HsBindgen.C.Fold.Type
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.C.Reparse
import HsBindgen.C.Tc.Macro (tcMacro)
import HsBindgen.Eff
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import HsBindgen.C.Tc.Macro qualified as Macro

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

foldDecls ::
     HasCallStack
  => Tracer IO Skipped
  -> Predicate
  -> IBindingSpecs SourcePath -- ^ Binding specs for configuration
  -> IBindingSpecs SourcePath -- ^ External binding specs
  -> [CHeaderIncludePath]
  -> CXTranslationUnit
  -> Fold (Eff (State DeclState)) Decl
foldDecls tracer p specs extSpecs headerIncludePaths unit current = do
    loc <- clang_getCursorLocation current
    sloc <- HighLevel.clang_getExpansionLocation loc

    let typeDecl :: Eff (State DeclState) (Next m a)
        typeDecl = do
          ty <- clang_getCursorType current
          -- TODO: add assert at ty is not invalid type.
          void $
            processTypeDecl specs extSpecs unit (Precise sloc) (Just current) ty
          return $ Continue Nothing

    eCursorKind <- fromSimpleEnum <$> clang_getCursorKind current

    -- process include directives even when predicate does not match
    when (eCursorKind == Right CXCursor_InclusionDirective) $ do
      -- update the include graph
      incHeader <-
            fmap SourcePath . clang_getFileName
        =<< clang_getIncludedFile current
      modify $ registerInclude (singleLocPath sloc) incHeader

      -- update the current main header
      isFromMainFile <- clang_Location_isFromMainFile loc
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
              macroTyEnv <- macroTypeEnv <$> get
              mbMExpr <- mkMacro unit current macroTyEnv
              macro <- case mbMExpr of
                Left err -> return MacroReparseError {
                    macroReparseError          = err
                  , macroReparseErrorSourceLoc = sloc
                  }
                Right macro@( Macro _ mVar mArgsList mBody ) -> Vec.reifyList mArgsList $ \ mArgs -> do
                  let tcRes = tcMacro macroTyEnv mVar mArgs mBody
                  case tcRes of
                    Left err ->
                      return MacroTcError {
                          macroTcErrorMacro     = macro
                        , macroTcError          = err
                        , macroTcErrorSourceLoc = sloc
                        }
                    Right ty -> do
                      modify $ registerMacroType mVar ty
                      return MacroDecl {
                          macroDeclMacro     = macro
                        , macroDeclMacroTy   = fmap snd ty
                        , macroDeclSourceLoc = sloc
                        }
              return $ Continue $ Just $ DeclMacro macro
        Right CXCursor_MacroExpansion -> do
          mloc <- HighLevel.clang_getCursorLocation current
          modify $ registerMacroExpansion mloc
          return $ Continue Nothing

        Right CXCursor_InclusionDirective ->
          -- no children, recurse and continue have same behavior
          return $ Continue Nothing

        Right CXCursor_FunctionDecl -> do
          spelling <- clang_getCursorSpelling current
          ty  <- clang_getCursorType current
          ty' <- processTypeDecl specs extSpecs unit (Precise sloc) (Just current) ty

          (args, res) <- case ty' of
            TypeFun args res -> return (args, res)
            _                -> panicIO $ "function declaration with a non-function type: " ++ show ty'

          return $ Continue $ Just $ DeclFunction $ Function
            { functionName      = CName spelling
            , functionArgs      = args
            , functionRes       = res
            , functionHeader    = headerIncludePath
            , functionSourceLoc = sloc
            }

        Right CXCursor_VarDecl -> do
          -- TODO: extern int i;
          return $ Continue Nothing

        _otherwise -> do
          unrecognizedCursor current

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
  -> Macro.TypeEnv
  -> m (Either ReparseError (Macro Ps))
mkMacro unit current macroTys = do
    range  <- HighLevel.clang_getCursorExtent current
    tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
    return $ reparseWith (reparseMacro macroTys) tokens
