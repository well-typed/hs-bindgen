-- | High-level bindings to @libclang@
--
-- Intended for qualified import.
--
-- > import Hsbindgen.C.Parser qualified as C
module HsBindgen.C.Parser (
    parseHeaderWith
  , foldDecls
    -- * Debugging
  , Element(..)
  , foldClangAST
    -- * Logging
  , ParseMsg(..)
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Data.Tree
import GHC.Stack

import HsBindgen.C.AST qualified as C
import HsBindgen.Clang.Args
import HsBindgen.Clang.Aux.Fold
import HsBindgen.Clang.Aux.SourceLoc (SourceRange)
import HsBindgen.Clang.Aux.SourceLoc qualified as SourceLoc
import HsBindgen.Clang.Core
import HsBindgen.Patterns
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

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

foldDecls :: HasCallStack => Tracer IO ParseMsg -> Fold C.Decl
foldDecls tracer current = do
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
parseStruct :: CXCursor -> IO ([C.StructField] -> C.Struct)
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

foldStructFields :: HasCallStack => Tracer IO ParseMsg -> Fold C.StructField
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

parseTypedef :: CXCursor -> IO (C.Typ -> C.Typedef)
parseTypedef current = do
    typedefName <- decodeString <$> clang_getCursorDisplayName current
    return $ \typedefType -> C.Typedef{
          typedefName
        , typedefType
        }

foldTyp :: HasCallStack => Tracer IO ParseMsg -> Fold C.Typ
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

-- | An element in the @libclang@ AST
data Element = Element {
      elementSourceRange       :: !SourceRange
    , elementSpelling          :: !Strict.ByteString
    , elementSpellingNameRange :: !SourceRange
    , elementDisplayName       :: !Strict.ByteString
    , elementTypeKind          :: !(SimpleEnum CXTypeKind)
    , elementTypeKindSpelling  :: !Strict.ByteString
    , elementRawComment        :: !Strict.ByteString
    , elementBriefComment      :: !Strict.ByteString
    , elementIsAnonymous       :: !Bool
    , elementIsDefinition      :: !Bool
    }
  deriving (Show)

-- | Fold that returns the raw @libclang@ AST
--
-- We can use this at the top-level in 'dumpClangAST', but it is also useful
-- more locally when trying to figure out what the ATS looks like underneath
-- a certain node. For example, suppose we are working on `foldTyp`, and we're
-- not exactly sure what the @struct@ case looks like; we might temporarily use
--
-- > foldTyp :: Tracer IO ParseMsg -> Fold C.Typ
-- > foldTyp tracer current = do
-- >     cursorType <- clang_getCursorType current
-- >     case fromSimpleEnum $ cxtKind cursorType of
-- >       Right CXType_Record -> do
-- >         return $ Recurse foldClangAST $ \t -> print t >> return Nothing
--
-- to see the AST under the @struct@ parent node.
foldClangAST :: Fold (Tree Element)
foldClangAST = go
  where
    go :: Fold (Tree Element)
    go current = do
        elementSourceRange       <- SourceLoc.clang_getCursorExtent             current
        elementSpelling          <- clang_getCursorSpelling                     current
        elementSpellingNameRange <- SourceLoc.clang_Cursor_getSpellingNameRange current
        elementDisplayName       <- clang_getCursorDisplayName                  current
        elementTypeKind          <- cxtKind <$> clang_getCursorType             current
        elementTypeKindSpelling  <- clang_getTypeKindSpelling elementTypeKind
        elementRawComment        <- clang_Cursor_getRawCommentText              current
        elementBriefComment      <- clang_Cursor_getBriefCommentText            current
        elementIsAnonymous       <- clang_Cursor_isAnonymous                    current
        elementIsDefinition      <- clang_isCursorDefinition                    current

        let element :: Element
            element = Element {
                elementSourceRange
              , elementSpelling
              , elementSpellingNameRange
              , elementDisplayName
              , elementTypeKind
              , elementTypeKindSpelling
              , elementRawComment
              , elementBriefComment
              , elementIsAnonymous
              , elementIsDefinition
              }

        return $ Recurse go (return . Just . Node element)

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

data ParseMsg =
    -- | We skipped over an element in the Clang AST we did not recognize
    Skipping CallStack (SimpleEnum CXTypeKind)

instance PrettyLogMsg ParseMsg where
  prettyLogMsg (Skipping cs kind) =
    "Skipping over " ++ show kind ++ " at " ++ prettyCallStack cs
