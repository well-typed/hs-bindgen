-- | Types
--
-- TODO: We should make the distinction between (C) type declarations and type
-- uses clear at the (Haskell) type level. Once we do, we should cull the
-- export list of this module.
module HsBindgen.C.Fold.Type (
    foldTypeDecl
  , mkTypeUse
    -- * TODO: The below exports should be removed
  , mkStructHeader
  , mkStructField
  , mkEnumHeader
  , mkEnumValue
  , TypedefHeader(..)
  , mkTypedefHeader
  ) where

import Control.Monad.State
import Foreign.C
import GHC.Stack

import HsBindgen.C.AST
import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.DeclState
import HsBindgen.C.Reparse
import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.Classification
import HsBindgen.Clang.Util.Fold
import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
import HsBindgen.Clang.Util.Tokens qualified as Tokens
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Fold type /declaration/
foldTypeDecl :: HasCallStack => CXTranslationUnit -> Fold (State DeclState) Typ
foldTypeDecl unit current = do
    cursorKind <- liftIO $ clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_StructDecl -> do
        mkStruct <- mkStructHeader current
        let mkDecl :: [StructField] -> Maybe Typ
            mkDecl = Just . TypStruct . mkStruct
        return $ Recurse (continue $ mkStructField unit) mkDecl
      _otherwise ->
        unrecognizedCursor current

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Parse type /use site/
mkTypeUse :: CXType -> IO Typ
mkTypeUse = go
  where
    go :: CXType -> IO Typ
    go ty =
        case fromSimpleEnum $ cxtKind ty of
          kind | Just prim <- primType kind ->
            return $ TypPrim prim

          Right CXType_Pointer -> do
            ty' <- clang_getPointeeType ty
            TypPointer <$> go ty'

          Right CXType_Elaborated -> do
            name <- CName <$> clang_getTypeSpelling ty
            return $ TypElaborated name

          _otherwise ->
            unrecognizedType ty

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

mkStructHeader :: MonadIO m => CXCursor -> m ([StructField] -> Struct)
mkStructHeader current = liftIO $ do
    cursorType      <- clang_getCursorType current
    structTag       <- fmap CName . getUserProvided <$>
                         getUserProvidedName current
    structSizeof    <- fromIntegral <$> clang_Type_getSizeOf  cursorType
    structAlignment <- fromIntegral <$> clang_Type_getAlignOf cursorType

    return $ \structFields -> Struct{
        structTag
      , structSizeof
      , structAlignment
      , structFields
      }

mkStructField ::
     CXTranslationUnit
  -> CXCursor
  -> FoldM (State DeclState) StructField
mkStructField unit current = do
    extent   <- liftIO $ SourceLoc.clang_getCursorExtent current
    hasMacro <- gets $ containsMacroExpansion extent

    if hasMacro then liftIO $ do

      tokens <- Tokens.clang_tokenize unit (multiLocExpansion <$> extent)
      case reparseWith reparseFieldDecl tokens of
        Left err ->
          error $ "mkStructField: " ++ show err
        Right (fieldType, fieldName) -> do
          fieldOffset <- fromIntegral <$> clang_Cursor_getOffsetOfField current
          return StructField{fieldName, fieldOffset, fieldType}

    else liftIO $ do

      ty          <- clang_getCursorType current
      fieldType   <- mkTypeUse ty
      fieldOffset <- fromIntegral <$> clang_Cursor_getOffsetOfField current
      fieldName   <- CName <$> clang_getCursorDisplayName current

      return StructField{fieldName, fieldOffset, fieldType}

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

mkEnumHeader :: MonadIO m => CXCursor -> m ([EnumValue] -> Enu)
mkEnumHeader current = liftIO $ do
    cursorType    <- clang_getCursorType current
    enumTag       <- fmap CName . getUserProvided <$>
                       getUserProvidedName current
    enumSizeof    <- fromIntegral <$> clang_Type_getSizeOf  cursorType
    enumAlignment <- fromIntegral <$> clang_Type_getAlignOf cursorType

    return $ \enumValues -> Enu{
        enumTag
      , enumSizeof
      , enumAlignment
      , enumValues
      }

mkEnumValue :: MonadIO m => CXCursor -> m (Maybe EnumValue)
mkEnumValue current = liftIO $ do
    cursorKind <- liftIO $ clang_getCursorKind current

    case fromSimpleEnum cursorKind of
      Right CXCursor_EnumConstantDecl -> do
        valueName  <- CName <$> clang_getCursorDisplayName current
        valueValue <- toInteger <$> clang_getEnumConstantDeclValue current
        return $ Just EnumValue{valueName, valueValue}
      Right CXCursor_PackedAttr ->
        -- TODO: __attribute__(packed))
        return Nothing
      _otherwise ->
        -- there could be attributes, e.g. packed
        unrecognizedCursor current

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

data TypedefHeader =
    -- | Typedef around a primitive type. No further parsing required
    TypedefPrim Typedef

    -- | Typedef around an elaborated type such as a struct.
  | TypedefElaborated (Typ -> Typedef)

mkTypedefHeader :: MonadIO m => CXCursor -> m TypedefHeader
mkTypedefHeader current = liftIO $ do
    typedefName <- CName <$> clang_getCursorDisplayName current
    let mkTypedef typedefType = Typedef{typedefName, typedefType}

    underlyingType <- clang_getTypedefDeclUnderlyingType current
    case fromSimpleEnum (cxtKind underlyingType) of
      typ | Just prim <- primType typ ->
        return $ TypedefPrim $ mkTypedef (TypPrim prim)
      Right CXType_Elaborated -> do
        return $ TypedefElaborated mkTypedef
      typ ->
        error $ "mkTypedefHeader: unexpected " ++ show typ

{-------------------------------------------------------------------------------
  Primitive types
-------------------------------------------------------------------------------}

primType :: Either CInt CXTypeKind -> Maybe PrimType
primType (Left _)     = Nothing
primType (Right kind) =
    case kind of
      CXType_Char_S     -> Just $ PrimChar     Nothing
      CXType_SChar      -> Just $ PrimChar     (Just Signed)
      CXType_UChar      -> Just $ PrimChar     (Just Unsigned)
      CXType_Short      -> Just $ PrimShort    Signed
      CXType_UShort     -> Just $ PrimShort    Unsigned
      CXType_Int        -> Just $ PrimInt      Signed
      CXType_UInt       -> Just $ PrimInt      Unsigned
      CXType_Long       -> Just $ PrimLong     Signed
      CXType_ULong      -> Just $ PrimLong     Unsigned
      CXType_LongLong   -> Just $ PrimLongLong Signed
      CXType_ULongLong  -> Just $ PrimLongLong Unsigned
      CXType_Float      -> Just $ PrimFloat
      CXType_Double     -> Just $ PrimDouble
      CXType_LongDouble -> Just $ PrimLongDouble
      _otherwise        -> Nothing


