-- | Parse functions related to struct and union fields
module HsBindgen.Frontend.Pass.Parse.Decl.Field (
    explicitFieldDecl
  , getFieldInfo
  ) where

import Control.Monad.IO.Class (MonadIO)

import Clang.HighLevel qualified as HighLevel
import Clang.LowLevel.Core (CXCursor, clang_Cursor_getOffsetOfField,
                            clang_Cursor_isBitField, clang_getCursorDisplayName,
                            clang_getCursorType, clang_getFieldDeclBitWidth)

import HsBindgen.Frontend.Pass.Parse.Context (ParseCtx)
import HsBindgen.Frontend.Pass.Parse.Decl.Macro (getReparseInfo)
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Parse.Monad.Decl (ParseDecl)
import HsBindgen.Frontend.Pass.Parse.Type (fromCXType)
import HsBindgen.IR.C qualified as C

explicitFieldDecl :: ParseCtx -> CXCursor -> ParseDecl (C.ExplicitField Parse)
explicitFieldDecl ctx = \curr -> do
    info   <- getFieldInfo curr
    typ    <- fromCXType ctx =<< clang_getCursorType curr
    offset <- fromIntegral <$> clang_Cursor_getOffsetOfField curr
    width  <- getFieldWidth curr
    ann    <- getReparseInfo curr
    pure C.ExplicitField{
        info   = info
      , typ    = typ
      , offset = offset
      , width  = width
      , ann    = ann
      }

getFieldWidth :: MonadIO m => CXCursor -> m (Maybe Int)
getFieldWidth = \curr -> do
    isBitField <- clang_Cursor_isBitField curr
    if isBitField
      then Just . fromIntegral <$> clang_getFieldDeclBitWidth curr
      else return Nothing

-- | Get field info from a cursor
--
-- Comments are filled in later by the 'EnrichComments' pass.
getFieldInfo :: MonadIO m => CXCursor -> m (C.FieldInfo Parse)
getFieldInfo = \curr -> do
    fieldLoc  <- HighLevel.clang_getCursorLocation' curr
    fieldName <- C.ScopedName <$> clang_getCursorDisplayName curr
    return C.FieldInfo {
        loc     = fieldLoc
      , name    = fieldName
      , comment = ()
      }
