-- | High-level bindings to @libclang@
--
-- Intended for qualified import.
--
-- > import Hsbindgen.C.Parser qualified as C
module HsBindgen.C.Parser (parseHeader) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.Char8

import HsBindgen.C.AST qualified as C
import HsBindgen.C.Clang
import HsBindgen.Patterns
import HsBindgen.Spec
import HsBindgen.Util.Tracer
import HsBindgen.C.Clang.Fold

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

    C.Header <$> clang_fold cursor (topLevel tracer)
  where
    flags :: CXTranslationUnit_Flags
    flags = bitfieldEnum [
          CXTranslationUnit_SkipFunctionBodies
        ]

topLevel :: Tracer IO LogMsg -> Fold C.Decl
topLevel tracer current = do
    cursorType <- clang_getCursorType current
    case fromSimpleEnum $ cxtKind cursorType of
      Right CXType_Record -> do
        sizeof    <- fromIntegral <$> clang_Type_getSizeOf  cursorType
        alignment <- fromIntegral <$> clang_Type_getAlignOf cursorType
        let decl fields = C.DeclStruct C.Struct{sizeof, alignment, fields}
        return $ Recurse (structFields tracer) (return . decl)
      _otherwise -> do
        traceWith tracer Warning $ Skipping (cxtKind cursorType)
        return $ Continue Nothing

structFields :: Tracer IO LogMsg -> Fold C.StructField
structFields tracer current = do
    cursorType <- clang_getCursorType current
    case primType $ cxtKind cursorType of
      Just fieldType -> do
        fieldName <- decodeString <$> clang_getCursorDisplayName current
        let field = C.StructField{fieldName, fieldType}
        return $ Continue (Just field)
      _otherwise -> do
        traceWith tracer Warning $ UnrecognizedStructField (cxtKind cursorType)
        return $ Continue Nothing

primType :: SimpleEnum CXTypeKind -> Maybe C.PrimType
primType = either (const Nothing) aux . fromSimpleEnum
  where
    aux :: CXTypeKind -> Maybe C.PrimType
    aux CXType_Int    = Just C.PrimInt
    aux CXType_Char_S = Just C.PrimChar
    aux CXType_Float  = Just C.PrimFloat
    aux _             = Nothing

-- TODO: <https://github.com/well-typed/hs-bindgen/issues/87>
-- Deal with file encodings other than ASCII
decodeString :: Strict.ByteString -> String
decodeString = BS.Strict.Char8.unpack

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data LogMsg =
    -- | We skipped over an element in the Clang AST we did not recognize
    Skipping (SimpleEnum CXTypeKind)

    -- | Struct contained an element we did not recognize
  | UnrecognizedStructField (SimpleEnum CXTypeKind)

instance PrettyLogMsg LogMsg where
  prettyLogMsg (Skipping kind) =
    "Unrecognized top-level declaration: " ++ show kind
  prettyLogMsg (UnrecognizedStructField kind) =
    "Unrecognized struct field: " ++ show kind