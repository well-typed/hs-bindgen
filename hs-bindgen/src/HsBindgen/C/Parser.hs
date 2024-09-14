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
  , foldComments
  , getTranslationUnitTargetTriple
    -- * Logging
  , ParseMsg(..)
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Data.List (partition)
import Data.Tree
import Foreign.C
import GHC.Stack

import HsBindgen.C.AST qualified as C
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Clang.Args
import HsBindgen.Clang.Core
import HsBindgen.Clang.Doxygen
import HsBindgen.Clang.Util.Classification
import HsBindgen.Clang.Util.Diagnostics (Diagnostic(..))
import HsBindgen.Clang.Util.Diagnostics qualified as Diagnostics
import HsBindgen.Clang.Util.Fold
import HsBindgen.Clang.Util.SourceLoc (SourceLoc)
import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
import HsBindgen.Clang.Util.Tokens qualified as Tokens
import HsBindgen.Patterns
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  General setup
-------------------------------------------------------------------------------}

-- TODO: <https://github.com/well-typed/hs-bindgen/issues/174>
-- We should have a pretty renderer for diagnostics. For now we rely on
-- 'diagnosticFormatted'.
data CErrors = CErrors [ByteString]
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Parse C file
--
-- Throws 'CErrors' if @libclang@ reported any errors in the C file.
withTranslationUnit ::
     ClangArgs
  -> FilePath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withTranslationUnit args fp k = do
    index  <- clang_createIndex DontDisplayDiagnostics
    unit   <- clang_parseTranslationUnit index fp args flags
    diags  <- Diagnostics.getDiagnostics unit Nothing

    let errors, warnings :: [Diagnostic]
        (errors, warnings) = partition Diagnostics.isError diags

    case errors of
      [] -> do
        -- TODO: <https://github.com/well-typed/hs-bindgen/issues/175>
        -- We should print warnings only optionally.
        print warnings
        k unit
      errs ->
        throwIO $ CErrors $ map diagnosticFormatted errs
  where
    flags :: BitfieldEnum CXTranslationUnit_Flags
    flags = bitfieldEnum [
          CXTranslationUnit_SkipFunctionBodies
        , CXTranslationUnit_DetailedPreprocessingRecord
        , CXTranslationUnit_IncludeAttributedTypes
        , CXTranslationUnit_VisitImplicitAttributes
        ]

getTranslationUnitTargetTriple :: CXTranslationUnit -> IO ByteString
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
  => Tracer IO ParseMsg
  -> Predicate
  -> CXTranslationUnit -> Fold C.Decl
foldDecls tracer p unit = checkPredicate tracer p $ \_parent current -> do
    cursorKind <- clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_StructDecl -> do
        mkStruct <- parseStruct unit current
        let mkDecl :: [C.StructField] -> IO (Maybe C.Decl)
            mkDecl = return . Just . C.DeclStruct . mkStruct
        return $ Recurse (foldStructFields tracer) mkDecl
      Right CXCursor_EnumDecl -> do
        mkEnum <- parseEnum unit current
        let mkDecl :: [C.EnumValue] -> IO (Maybe C.Decl)
            mkDecl = return . Just . C.DeclEnum . mkEnum
        return $ Recurse (foldEnumValues tracer) mkDecl
      Right CXCursor_TypedefDecl -> do
        mkTypedef <- parseTypedef current
        let mkDecl :: [C.Typ] -> IO (Maybe C.Decl)
            mkDecl [typ] = return $ Just (C.DeclTypedef $ mkTypedef typ)
            mkDecl types = error $ "mkTypedef: unexpected " ++ show types
        return $ Recurse (foldTyp tracer unit) mkDecl
      Right CXCursor_MacroDefinition -> do
        range  <- clang_getCursorExtent current
        tokens <- Tokens.clang_tokenize unit range
        let decl :: C.Decl
            decl = C.DeclMacro $ C.Macro $ map C.Token tokens
        return $ Continue $ Just decl
      _otherwise -> do
        traceWith tracer Warning $ unrecognizedCursor cursorKind
        return $ Continue Nothing

checkPredicate :: Tracer IO ParseMsg -> Predicate -> Fold a -> Fold a
checkPredicate tracer p k parent current = do
    isMatch <- Predicate.match parent current p
    case isMatch of
      Right ()     -> k parent current
      Left  reason -> do
        name <- clang_getCursorSpelling current
        loc  <- SourceLoc.clang_getCursorLocation current
        traceWith tracer Info $ Skipped name loc reason
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
foldStructFields tracer _parent current = do
    ty  <- clang_getCursorType current
    ty' <- parseType tracer ty
    fieldOffset <- fromIntegral <$> clang_Cursor_getOffsetOfField current
    fieldName   <- decodeString <$> clang_getCursorDisplayName current
    case ty' of
      Nothing        -> return $ Continue Nothing
      Just fieldType -> do
        let field = C.StructField{fieldName, fieldOffset, fieldType}
        return $ Continue (Just field)

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
foldEnumValues tracer _parent current = do
    typeKind <- cxtKind <$> clang_getCursorType current
    case primType (fromSimpleEnum typeKind) of
      Just _fieldType -> do
        valueName  <- decodeString <$> clang_getCursorDisplayName     current
        valueValue <- toInteger    <$> clang_getEnumConstantDeclValue current
        let field = C.EnumValue{valueName, valueValue}
        return $ Continue (Just field)
      _otherwise -> do
        traceWith tracer Warning $ unrecognizedType typeKind
        return $ Continue Nothing

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Parse type
--
-- If we encounter an unrecognized type, we return 'Nothing' and issue a
-- warning. The warning is ussed here, rather than at the call site, because
-- 'parseType' is recursive.
parseType :: HasCallStack => Tracer IO ParseMsg -> CXType -> IO (Maybe C.Typ)
parseType tracer = go
  where
    go :: CXType -> IO (Maybe C.Typ)
    go ty =
        case fromSimpleEnum $ cxtKind ty of
          kind | Just prim <- primType kind ->
            return $ Just (C.TypPrim prim)

          Right CXType_Pointer -> do
            ty' <- clang_getPointeeType ty
            fmap C.TypPointer <$> go ty'

          _otherwise -> do
            traceWith tracer Warning $ unrecognizedType (cxtKind ty)
            return Nothing


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
foldTyp tracer unit _parent current = do
    cursorKind <- clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_StructDecl -> do
        mkStruct <- parseStruct unit current
        let mkDecl :: [C.StructField] -> IO (Maybe C.Typ)
            mkDecl = return . Just . C.TypStruct . mkStruct
        return $ Recurse (foldStructFields tracer) mkDecl
      _otherwise -> do
        traceWith tracer Warning $ unrecognizedCursor cursorKind
        return $ Continue Nothing

primType :: Either CInt CXTypeKind -> Maybe C.PrimType
primType (Left _)     = Nothing
primType (Right kind) =
    case kind of
      CXType_Char_S     -> Just $ C.PrimChar     Nothing
      CXType_SChar      -> Just $ C.PrimChar     (Just C.Signed)
      CXType_UChar      -> Just $ C.PrimChar     (Just C.Unsigned)
      CXType_Short      -> Just $ C.PrimShortInt C.Signed
      CXType_UShort     -> Just $ C.PrimShortInt C.Unsigned
      CXType_Int        -> Just $ C.PrimInt      C.Signed
      CXType_UInt       -> Just $ C.PrimInt      C.Unsigned
      CXType_Long       -> Just $ C.PrimLong     C.Signed
      CXType_ULong      -> Just $ C.PrimLong     C.Unsigned
      CXType_LongLong   -> Just $ C.PrimLongLong C.Signed
      CXType_ULongLong  -> Just $ C.PrimLongLong C.Unsigned
      CXType_Float      -> Just $ C.PrimFloat
      CXType_Double     -> Just $ C.PrimDouble
      CXType_LongDouble -> Just $ C.PrimLongDouble
      _otherwise        -> Nothing

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | An element in the @libclang@ AST
data Element = Element {
      elementName         :: !(UserProvided ByteString)
    , elementKind         :: !ByteString
    , elementTypeKind     :: !ByteString
    , elementRawComment   :: !ByteString
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
foldClangAST :: Predicate -> CXTranslationUnit -> Fold (Tree Element)
foldClangAST p unit = checkPredicate nullTracer p go
  where
    go :: Fold (Tree Element)
    go _parent current = do
        elementName         <- getUserProvidedName unit       current
        elementKind         <- clang_getCursorKindSpelling =<<
                                          clang_getCursorKind current
        elementTypeKind     <- clang_getTypeKindSpelling . cxtKind =<<
                                          clang_getCursorType current
        elementRawComment   <- clang_Cursor_getRawCommentText current
        elementIsAnonymous  <- clang_Cursor_isAnonymous       current
        elementIsDefinition <- clang_isCursorDefinition       current

        let element :: Element
            element = Element {
                elementName
              , elementKind
              , elementTypeKind
              , elementRawComment
              , elementIsAnonymous
              , elementIsDefinition
              }

        return $ Recurse go (return . Just . Node element)

-- | Extract Doxygen comments as HTML for all top-level declarations
--
-- For each node in the Clang AST, we report the location, name and comment.
foldComments ::
     Predicate
  -> CXTranslationUnit
  -> Fold (Tree (SourceLoc, ByteString, Maybe ByteString))
foldComments p _unit = checkPredicate nullTracer p go
  where
    go :: Fold (Tree (SourceLoc, ByteString, Maybe ByteString))
    go _parent current = do
        sourceLoc   <- SourceLoc.clang_getCursorLocation current
        name        <- clang_getCursorSpelling current
        comment     <- clang_Cursor_getParsedComment current
        commentKind <- clang_Comment_getKind comment
        mAsHTML     <- case fromSimpleEnum commentKind of
                         Right CXComment_FullComment -> do
                           Just <$> clang_FullComment_getAsHTML comment
                         _otherwise ->
                           return Nothing

        return $ Recurse go (return . Just . Node (sourceLoc, name, mAsHTML))

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
decodeString :: ByteString -> String
decodeString = BS.Strict.Char8.unpack

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data ParseMsg =
    -- | Skipped filtered-out element
    --
    -- We record the name and location of the element, as well as the reason we
    -- skipped it.
    Skipped ByteString SourceLoc String

    -- | Skipped unrecognized cursor
  | UnrecognizedCursor CallStack (SimpleEnum CXCursorKind)

    -- | Skip unrecognized type
  | UnrecognizedType CallStack (SimpleEnum CXTypeKind)

unrecognizedCursor :: HasCallStack => SimpleEnum CXCursorKind -> ParseMsg
unrecognizedCursor = UnrecognizedCursor callStack

unrecognizedType :: HasCallStack => SimpleEnum CXTypeKind -> ParseMsg
unrecognizedType = UnrecognizedType callStack

instance PrettyLogMsg ParseMsg where
  prettyLogMsg (Skipped name loc reason) = mconcat [
        "Skipped element "
      , show name
      , " at "
      , show loc
      , ": "
      , reason
      ]
  prettyLogMsg (UnrecognizedCursor cs kind) = mconcat [
        "Unrecognized element "
      , show kind
      , " at "
      , prettyCallStack cs
      ]
  prettyLogMsg (UnrecognizedType cs kind) = mconcat [
        "Unrecognized type "
      , show kind
      , " at "
      , prettyCallStack cs
      ]
