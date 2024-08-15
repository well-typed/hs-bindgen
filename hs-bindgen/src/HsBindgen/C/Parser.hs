-- | High-level bindings to @libclang@
--
-- Intended for qualified import.
--
-- > import Hsbindgen.C.Parser qualified as C
module HsBindgen.C.Parser (
    parseHeader
  , dumpClangAST
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Foreign
import GHC.Stack

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
parseHeader tracer args fp =
    C.Header <$> parseHeaderWith args fp (foldTopLevel tracer)

parseHeaderWith ::
     ClangArgs
  -> FilePath
  -> Fold a
  -> IO [a]
parseHeaderWith args fp fold = do
    index  <- clang_createIndex DontDisplayDiagnostics
    unit   <- clang_parseTranslationUnit index fp args flags
    cursor <- clang_getTranslationUnitCursor unit

    clang_fold cursor fold
  where
    flags :: CXTranslationUnit_Flags
    flags = bitfieldEnum [
          CXTranslationUnit_SkipFunctionBodies
        ]

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

foldTopLevel :: Tracer IO LogMsg -> Fold C.Decl
foldTopLevel tracer current = do
    cursorType <- clang_getCursorType current
    case fromSimpleEnum $ cxtKind cursorType of
      Right CXType_Record -> do
        mkStruct <- parseStruct current
        let mkDecl :: [C.StructField] -> IO (Maybe C.Decl)
            mkDecl = return . Just . C.DeclStruct . mkStruct
        return $ Recurse (foldStructFields tracer) mkDecl
      Right CXType_Typedef -> do
        mkTypedef <- parseTypedef current
        let mkDecl :: [C.Typ] -> IO (Maybe C.Decl)
            mkDecl [typ] = return $ Just (C.DeclTypedef $ mkTypedef typ)
            mkDecl types = error $ "mkTypedef: unexpected " ++ show types
        return $ Recurse (foldTyp tracer) mkDecl
      _otherwise -> do
        traceWith tracer Warning $ Skipping callStack (cxtKind cursorType)
        return $ Continue Nothing

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

-- | Parse struct
--
-- Implementation note: It seems libclang will give us a name for the struct if
-- the struct it a tag, but also when it's anonymous but the surrounding typedef
-- has a name.
parseStruct :: ForeignPtr CXCursor -> IO ([C.StructField] -> C.Struct)
parseStruct current = do
    cursorType      <- clang_getCursorType current
    structName      <- decodeString <$> clang_getCursorDisplayName current
    structSizeof    <- fromIntegral <$> clang_Type_getSizeOf  cursorType
    structAlignment <- fromIntegral <$> clang_Type_getAlignOf cursorType
    return $ \structFields -> C.Struct{
        structName
      , structSizeof
      , structAlignment
      , structFields
      }

foldStructFields :: Tracer IO LogMsg -> Fold C.StructField
foldStructFields tracer current = do
    cursorType <- clang_getCursorType current
    case primType $ cxtKind cursorType of
      Just fieldType -> do
        fieldName <- decodeString <$> clang_getCursorDisplayName current
        let field = C.StructField{fieldName, fieldType}
        return $ Continue (Just field)
      _otherwise -> do
        traceWith tracer Warning $ Skipping callStack (cxtKind cursorType)
        return $ Continue Nothing

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

parseTypedef :: ForeignPtr CXCursor -> IO (C.Typ -> C.Typedef)
parseTypedef current = do
    typedefName <- decodeString <$> clang_getCursorDisplayName current
    return $ \typedefType -> C.Typedef{
          typedefName
        , typedefType
        }

foldTyp :: Tracer IO LogMsg -> Fold C.Typ
foldTyp tracer current = do
    cursorType <- clang_getCursorType current
    case fromSimpleEnum $ cxtKind cursorType of
      Right CXType_Record -> do
        mkStruct <- parseStruct current
        let mkDecl :: [C.StructField] -> IO (Maybe C.Typ)
            mkDecl = return . Just . C.TypStruct . mkStruct
        return $ Recurse (foldStructFields tracer) mkDecl
      _otherwise -> do
        traceWith tracer Warning $ Skipping callStack (cxtKind cursorType)
        return $ Continue Nothing

primType :: SimpleEnum CXTypeKind -> Maybe C.PrimType
primType = either (const Nothing) aux . fromSimpleEnum
  where
    aux :: CXTypeKind -> Maybe C.PrimType
    aux CXType_Int    = Just C.PrimInt
    aux CXType_Char_S = Just C.PrimChar
    aux CXType_Float  = Just C.PrimFloat
    aux _             = Nothing

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Parse C header
dumpClangAST :: ClangArgs -> FilePath -> IO ()
dumpClangAST args fp = const () <$> parseHeaderWith args fp foldDumpAST

-- | Fold that simply tries to dump the @libclang@ AST to the console
--
-- We can use this at the top-level in 'dumpClangAST', but it is also useful
-- more locally when trying to figure out what the ATS looks like underneath
-- a certain node. For example, suppose we are working on `foldTyp`, and we're
-- not exactly sure what the @struct@ case looks like; we might temporarily use
--
-- > foldTyp :: Tracer IO LogMsg -> Fold C.Typ
-- > foldTyp tracer current = do
-- >     cursorType <- clang_getCursorType current
-- >     case fromSimpleEnum $ cxtKind cursorType of
-- >       Right CXType_Record -> do
-- >         return $ recurse_ foldDumpAST
--
-- to see the AST under the @struct@ parent node.
foldDumpAST :: Fold ()
foldDumpAST = go 0
  where
    go :: Int -> Fold ()
    go n current = do
        displayName <- clang_getCursorDisplayName current
        cursorType  <- clang_getCursorType current
        typeKind    <- clang_getTypeKindSpelling (cxtKind cursorType)

        putStrLn $ mconcat [
            replicate (n * 2) ' '
          , show displayName
          , " :: "
          , show typeKind
          ]

        return $ recurse_ (go (n + 1))

{-------------------------------------------------------------------------------
  Auxiliary: strings
-------------------------------------------------------------------------------}

-- | Decode string
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/87>
-- Deal with file encodings other than ASCII
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/96>
-- We could consider trying to deduplicate.
decodeString :: Strict.ByteString -> String
decodeString = BS.Strict.Char8.unpack

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data LogMsg =
    -- | We skipped over an element in the Clang AST we did not recognize
    Skipping CallStack (SimpleEnum CXTypeKind)

instance PrettyLogMsg LogMsg where
  prettyLogMsg (Skipping cs kind) =
    "Skipping over " ++ show kind ++ " at " ++ prettyCallStack cs