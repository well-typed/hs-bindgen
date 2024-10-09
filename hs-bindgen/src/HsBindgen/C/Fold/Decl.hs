-- | Translate @libclang@ declarations
module HsBindgen.C.Fold.Decl (
    DeclState(..)
  , initDeclState
  , foldDecls
  ) where

import Control.Monad.State
import GHC.Stack

import HsBindgen.C.AST
import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.Type
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.C.Reparse
import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.Fold
import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
import HsBindgen.Clang.Util.Tokens qualified as Tokens
import HsBindgen.Patterns
import HsBindgen.Util.Tracer
import Data.Maybe (catMaybes)

{-------------------------------------------------------------------------------
  Monad used for the traversal
-------------------------------------------------------------------------------}

type M = State DeclState

data DeclState = DeclState

initDeclState :: DeclState
initDeclState = DeclState

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

foldDecls ::
     HasCallStack
  => Tracer IO Skipped
  -> Predicate
  -> CXTranslationUnit -> Fold M Decl
foldDecls tracer p unit = checkPredicate tracer p $ \current -> do
    cursorKind <- liftIO $ clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_StructDecl -> do
        decl <- declStruct <$> mkStructHeader current
        return $ Recurse (continue mkStructField) (Just . decl)
      Right CXCursor_EnumDecl -> do
        decl <- declEnum <$> mkEnumHeader current
        return $ Recurse (continue mkEnumValue) (Just . decl)
      Right CXCursor_TypedefDecl -> do
        decl <- declTypedef <$> mkTypedefHeader current
        return $ Recurse foldTypeDecl (Just . decl)
      Right CXCursor_MacroDefinition -> do
        decl <- declMacro <$> mkMacro unit current
        return $ Continue (Just decl)
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
    range  <- SourceLoc.clang_getCursorExtent current
    tokens <- Tokens.clang_tokenize unit (multiLocExpansion <$> range)
    return $ reparseWith reparseMacro tokens
