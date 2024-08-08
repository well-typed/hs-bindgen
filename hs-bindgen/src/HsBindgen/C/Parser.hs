-- | High-level bindings to @libclang@
--
-- Intended for qualified import.
--
-- > import Hsbindgen.C.Parser qualified as C
module HsBindgen.C.Parser (parseHeader) where

import Control.Monad
import Data.IORef

import HsBindgen.C.AST qualified as C
import HsBindgen.C.Clang
import HsBindgen.Patterns
import HsBindgen.Spec
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

-- | Parse C header
parseHeader ::
     Tracer IO LogMsg
  -> ClangArgs -> FilePath -> IO C.Header
parseHeader tracer args fp = do
    index  <- clang_createIndex DontDisplayDiagnostics
    unit   <- clang_parseTranslationUnit index fp args flags
    cursor <- clang_getTranslationUnitCursor unit

    decls :: IORef [C.Decl] <- newIORef []

    void $ clang_visitChildren cursor $ \current _parent -> do
      cursorType <- clang_getCursorType current
      case fromSimpleEnum $ cxtKind cursorType of
        Right CXType_Record -> do
          sizeof    <- fromIntegral <$> clang_Type_getSizeOf  cursorType
          alignment <- fromIntegral <$> clang_Type_getAlignOf cursorType
          let decl = C.DeclStruct C.Struct{sizeof, alignment}
          modifyIORef decls $ (decl :)
          -- TODO: We should use Recurse here rather than Continue,
          -- so that we process the fields
          return $ simpleEnum CXChildVisit_Continue
        _otherwise -> do
          traceWith tracer Warning $ Skipping (cxtKind cursorType)
          return $ simpleEnum CXChildVisit_Continue

    C.Header <$> readIORef decls
  where
    flags :: CXTranslationUnit_Flags
    flags = bitfieldEnum [
          CXTranslationUnit_SkipFunctionBodies
        ]

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data LogMsg =
  -- | We skipped over an element in the Clang AST we did not recognize
  Skipping (SimpleEnum CXTypeKind)

instance PrettyLogMsg LogMsg where
  prettyLogMsg (Skipping kind) =
    "Skipping over unrecognized " ++ show kind