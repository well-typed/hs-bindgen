-- | Translate @libclang@ declarations
module HsBindgen.C.Fold.Decl (
    DeclState(..)
  , initDeclState
  , foldDecls
  ) where

import Control.Monad.State
import Data.Maybe (catMaybes)
import GHC.Stack

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
        decl <- declStruct <$> mkStructHeader current
        return $ Recurse (continue $ mkStructField unit) (Just . decl)
      Right CXCursor_EnumDecl -> do
        decl <- declEnum <$> mkEnumHeader current
        return $ Recurse (continue mkEnumValue) (Just . decl)
      Right CXCursor_TypedefDecl -> do
        typedefHeader <- mkTypedefHeader current
        case typedefHeader of
          TypedefPrim typedef ->
            return $ Continue $ Just $ DeclTypedef typedef
          TypedefElaborated mkTypedef ->
            return $ Recurse (foldTypeDecl unit) (Just . declTypedef mkTypedef)
      Right CXCursor_MacroDefinition -> do
        decl <- declMacro <$> mkMacro unit current
        return $ Continue (Just decl)
      Right CXCursor_MacroExpansion -> do
        loc <- liftIO $ HighLevel.clang_getCursorLocation current
        modify $ registerMacroExpansion loc
        return $ Continue Nothing
      Right CXCursor_InclusionDirective ->
        -- Ignore
        return $ Continue Nothing
      _otherwise -> do
        unrecognizedCursor current

{-------------------------------------------------------------------------------
  Type declarations
-------------------------------------------------------------------------------}

declStruct :: ([StructField] -> Struct) -> [StructField] -> Decl
declStruct partial = DeclStruct . partial

declEnum :: ([EnumValue] -> Enu) -> [Maybe EnumValue] -> Decl
declEnum partial = DeclEnum . partial . catMaybes

declTypedef :: (Typ -> Typedef) -> [Typ] -> Decl
declTypedef partial [typ] = DeclTypedef $ partial typ
declTypedef _       types = error $ "declTypedef: unexpected " ++ show types

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

declMacro :: Either ReparseError Macro -> Decl
declMacro = DeclMacro

mkMacro ::
     MonadIO m
  => CXTranslationUnit -> CXCursor -> m (Either ReparseError Macro)
mkMacro unit current = liftIO $ do
    range  <- HighLevel.clang_getCursorExtent current
    tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
    return $ reparseWith reparseMacro tokens
