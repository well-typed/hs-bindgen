-- | Parse functions related to struct and union fields
module HsBindgen.Frontend.Pass.Parse.Decl.Field (
    structFieldDecl
  , unionFieldDecl
  , getFieldInfo
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Documentation qualified as CDoc
import Clang.LowLevel.Core (CXCursor, clang_Cursor_getOffsetOfField,
                            clang_Cursor_isBitField, clang_getCursorDisplayName,
                            clang_getCursorType, clang_getFieldDeclBitWidth)

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming (CScopedName (CScopedName))
import HsBindgen.Frontend.Pass.Parse.Context (ParseCtx)
import HsBindgen.Frontend.Pass.Parse.Decl.Comment (parseCommentReferences)
import HsBindgen.Frontend.Pass.Parse.Decl.Reparse (getReparseInfo)
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Parse.Monad.Decl (ParseDecl)
import HsBindgen.Frontend.Pass.Parse.Type (fromCXType)

structFieldDecl :: ParseCtx -> CXCursor -> ParseDecl (C.StructField Parse)
structFieldDecl ctx = \curr -> do
    structFieldInfo   <- getFieldInfo curr
    structFieldType   <- fromCXType ctx =<< clang_getCursorType curr
    structFieldOffset <- fromIntegral <$> clang_Cursor_getOffsetOfField curr
    structFieldAnn    <- getReparseInfo curr
    structFieldWidth  <- structWidth curr
    pure C.StructField{
        info   = structFieldInfo
      , typ    = structFieldType
      , offset = structFieldOffset
      , width  = structFieldWidth
      , ann    = structFieldAnn
      }

structWidth :: CXCursor -> ParseDecl (Maybe Int)
structWidth = \curr -> do
    isBitField <- clang_Cursor_isBitField curr
    if isBitField
      then Just . fromIntegral <$> clang_getFieldDeclBitWidth curr
      else return Nothing

unionFieldDecl :: ParseCtx -> CXCursor -> ParseDecl (C.UnionField Parse)
unionFieldDecl ctx = \curr -> do
    unionFieldInfo <- getFieldInfo curr
    unionFieldType <- fromCXType ctx =<< clang_getCursorType curr
    unionFieldAnn  <- getReparseInfo curr
    pure C.UnionField{
        info = unionFieldInfo
      , typ  = unionFieldType
      , ann  = unionFieldAnn
      }

getFieldInfo :: CXCursor -> ParseDecl (C.FieldInfo Parse)
getFieldInfo = \curr -> do
    fieldLoc     <- HighLevel.clang_getCursorLocation' curr
    fieldName    <- CScopedName <$> clang_getCursorDisplayName curr
    fieldComment <- fmap parseCommentReferences <$> CDoc.clang_getComment curr

    return C.FieldInfo {
        loc     = fieldLoc
      , name    = fieldName
      , comment = fieldComment
      }
