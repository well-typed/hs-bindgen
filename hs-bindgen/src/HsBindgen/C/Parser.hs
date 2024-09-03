-- | High-level bindings to @libclang@
--
-- Intended for qualified import.
--
-- > import Hsbindgen.C.Parser qualified as C
module HsBindgen.C.Parser (
    parseHeaderWith
  , withTranslationUnit
  , foldDecls
    -- * Debugging
  , Element(..)
  , foldClangAST
  , getTranslationUnitTargetTriple
    -- * Logging
  , ParseMsg(..)
  ) where

import Control.Exception (bracket)
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Data.Tree
import GHC.Stack

import HsBindgen.C.AST qualified as C
import HsBindgen.Clang.Args
import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.Classification
import HsBindgen.Clang.Util.Fold
import HsBindgen.Patterns
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  General setup
-------------------------------------------------------------------------------}

withTranslationUnit ::
     ClangArgs
  -> FilePath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withTranslationUnit args fp kont = do
    index  <- clang_createIndex DontDisplayDiagnostics
    unit   <- clang_parseTranslationUnit index fp args flags
    kont unit
  where
    flags :: CXTranslationUnit_Flags
    flags = bitfieldEnum [
          CXTranslationUnit_SkipFunctionBodies
        ]

getTranslationUnitTargetTriple :: CXTranslationUnit -> IO Strict.ByteString
getTranslationUnitTargetTriple unit =
    bracket
        (clang_getTranslationUnitTargetInfo unit)
        clang_TargetInfo_dispose
        clang_TargetInfo_getTriple

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseHeaderWith ::
     ClangArgs
  -> FilePath
  -> (CXTranslationUnit -> Fold a)
  -> IO [a]
parseHeaderWith args fp fold = withTranslationUnit args fp $ \unit -> do
    cursor <- clang_getTranslationUnitCursor unit
    clang_fold cursor $ fold unit

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

foldDecls ::
     HasCallStack
  => Tracer IO ParseMsg -> CXTranslationUnit -> Fold C.Decl
foldDecls tracer unit current = do
    cursorType <- clang_getCursorType current
    case fromSimpleEnum $ cxtKind cursorType of
      Right CXType_Record -> do
        mkStruct <- parseStruct unit current
        let mkDecl :: [C.StructField] -> IO (Maybe C.Decl)
            mkDecl = return . Just . C.DeclStruct . mkStruct
        return $ Recurse (foldStructFields tracer) mkDecl
      Right CXType_Enum -> do
        mkEnum <- parseEnum unit current
        let mkDecl :: [C.EnumValue] -> IO (Maybe C.Decl)
            mkDecl = return . Just . C.DeclEnum . mkEnum
        return $ Recurse (foldEnumValues tracer) mkDecl
      Right CXType_Typedef -> do
        mkTypedef <- parseTypedef current
        let mkDecl :: [C.Typ] -> IO (Maybe C.Decl)
            mkDecl [typ] = return $ Just (C.DeclTypedef $ mkTypedef typ)
            mkDecl types = error $ "mkTypedef: unexpected " ++ show types
        return $ Recurse (foldTyp tracer unit) mkDecl
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
parseStruct :: CXTranslationUnit -> CXCursor -> IO ([C.StructField] -> C.Struct)
parseStruct unit current = do
    cursorType      <- clang_getCursorType current
    structTag       <- fmap decodeString . getUserProvided <$>
                         getUserProvidedName unit current
    structSizeof    <- fromIntegral <$> clang_Type_getSizeOf  cursorType
    structAlignment <- fromIntegral <$> clang_Type_getAlignOf cursorType

    return $ \structFields -> C.Struct{
        structTag
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
  Enums
-------------------------------------------------------------------------------}

parseEnum :: CXTranslationUnit -> CXCursor -> IO ([C.EnumValue] -> C.Enu)
parseEnum unit current = do
    cursorType    <- clang_getCursorType current
    enumTag       <- fmap decodeString . getUserProvided <$>
                         getUserProvidedName unit current
    enumSizeof    <- fromIntegral <$> clang_Type_getSizeOf  cursorType
    enumAlignment <- fromIntegral <$> clang_Type_getAlignOf cursorType

    return $ \enumValues -> C.Enu{
        enumTag
      , enumSizeof
      , enumAlignment
      , enumValues
      }

foldEnumValues :: HasCallStack => Tracer IO ParseMsg -> Fold C.EnumValue
foldEnumValues tracer current = do
    cursorType <- clang_getCursorType current
    case primType $ cxtKind cursorType of
      Just _fieldType -> do
        valueName <- decodeString <$> clang_getCursorDisplayName current
        let field = C.EnumValue{valueName}
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

foldTyp ::
     HasCallStack
  => Tracer IO ParseMsg -> CXTranslationUnit -> Fold C.Typ
foldTyp tracer unit current = do
    cursorType <- clang_getCursorType current
    case fromSimpleEnum $ cxtKind cursorType of
      Right CXType_Record -> do
        mkStruct <- parseStruct unit current
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
      elementName         :: !(UserProvided Strict.ByteString)
    , elementTypeKind     :: !Strict.ByteString
    , elementRawComment   :: !Strict.ByteString
    , elementIsAnonymous  :: !Bool
    , elementIsDefinition :: !Bool
    }
  deriving stock (Show)

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
foldClangAST :: CXTranslationUnit -> Fold (Tree Element)
foldClangAST unit = go
  where
    go :: Fold (Tree Element)
    go current = do
        elementName         <- getUserProvidedName unit       current
        elementTypeKind     <- clang_getTypeKindSpelling . cxtKind =<<
                                          clang_getCursorType current
        elementRawComment   <- clang_Cursor_getRawCommentText current
        elementIsAnonymous  <- clang_Cursor_isAnonymous       current
        elementIsDefinition <- clang_isCursorDefinition       current

        let element :: Element
            element = Element {
                elementName
              , elementTypeKind
              , elementRawComment
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
