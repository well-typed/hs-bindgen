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
  , mkTypedef
  ) where

import Control.Monad
import Control.Monad.State
import Foreign.C
import GHC.Stack

import HsBindgen.C.AST
import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.DeclState
import HsBindgen.C.Reparse
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Eff
import HsBindgen.Imports
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Fold type /declaration/
foldTypeDecl :: HasCallStack => CXTranslationUnit -> Fold (FoldM (State DeclState)) Typ
foldTypeDecl unit current = do
    cursorKind <- liftIO $ clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_StructDecl -> do
        mkStruct <- mkStructHeader current
        let mkDecl :: [Maybe StructField] -> Maybe Typ
            mkDecl = Just . TypStruct . mkStruct . catMaybes
        return $ Recurse (continue $ mkStructField unit) mkDecl
      _otherwise ->
        unrecognizedCursor current

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Parse type /use site/
mkTypeUse :: HasCallStack => CXType -> IO Typ
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

          Right CXType_ConstantArray -> do
            n   <- clang_getArraySize ty
            ty' <- clang_getArrayElementType ty
            TypConstArray (fromIntegral n) <$> go ty'

          Right CXType_Elaborated -> do
            name <- CName <$> clang_getTypeSpelling ty
            return $ TypElaborated name

          -- Older versions of libclang (e.g. clang-14) report 'CXType_Typedef'
          -- instead of 'CXType_Elaborated'.
          Right CXType_Typedef -> do
            name <- CName <$> clang_getTypeSpelling ty
            return $ TypElaborated name

          Right CXType_Void -> do
            return $ TypPrim PrimVoid

          Right CXType_FunctionProto -> do
            -- TODO: for now we represent function types as Void
            return $ TypPrim PrimVoid

          _otherwise ->
            unrecognizedType ty

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

mkStructHeader :: MonadIO m => CXCursor -> m ([StructField] -> Struct)
mkStructHeader current = liftIO $ do
    cursorType      <- clang_getCursorType current
    structTag       <- fmap CName . getUserProvided <$>
                         HighLevel.clang_getCursorSpelling current
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
  -> FoldM (State DeclState) (Maybe StructField)
mkStructField unit current = do
    cursorKind <- liftIO $ clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_UnexposedAttr ->
        return Nothing

      Right CXCursor_FieldDecl -> do
        extent   <- liftIO $ HighLevel.clang_getCursorExtent current
        hasMacro <- gets $ containsMacroExpansion extent

        if hasMacro then liftIO $ do

          tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> extent)
          case reparseWith reparseFieldDecl tokens of
            Left err ->
              error $ "mkStructField: " ++ show err
            Right (fieldType, fieldName) -> do
              fieldOffset <- fromIntegral <$> clang_Cursor_getOffsetOfField current
              unless (fieldOffset `mod` 8 == 0) $
                error "bit-fields not supported yet"
              return $ Just StructField{fieldName, fieldOffset, fieldType}

        else liftIO $ do
          fieldName   <- CName <$> clang_getCursorDisplayName current
          ty          <- clang_getCursorType current
          fieldType   <- mkTypeUse ty
          fieldOffset <- fromIntegral <$> clang_Cursor_getOffsetOfField current

          unless (fieldOffset `mod` 8 == 0) $ error "bit-fields not supported yet"

          return $ Just StructField{fieldName, fieldOffset, fieldType}

      _other ->
        unrecognizedCursor current

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

mkEnumHeader :: MonadIO m => CXCursor -> m ([EnumValue] -> Enu)
mkEnumHeader current = liftIO $ do
    cursorType    <- clang_getCursorType current
    enumTag       <- fmap CName . getUserProvided <$>
                       HighLevel.clang_getCursorSpelling current
    enumType      <- mkTypeUse =<< clang_getEnumDeclIntegerType current
    enumSizeof    <- fromIntegral <$> clang_Type_getSizeOf  cursorType
    enumAlignment <- fromIntegral <$> clang_Type_getAlignOf cursorType

    return $ \enumValues -> Enu{
        enumTag
      , enumType
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

mkTypedef :: MonadIO m => CXCursor -> m Typedef
mkTypedef current = liftIO $ do
    typedefName <- CName <$> clang_getCursorDisplayName current

    underlyingType <- clang_getTypedefDeclUnderlyingType current
    typedefType <- mkTypeUse underlyingType
    return Typedef {typedefName,typedefType}

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
      CXType_Short      -> Just $ PrimIntegral $ PrimShort    Signed
      CXType_UShort     -> Just $ PrimIntegral $ PrimShort    Unsigned
      CXType_Int        -> Just $ PrimIntegral $ PrimInt      Signed
      CXType_UInt       -> Just $ PrimIntegral $ PrimInt      Unsigned
      CXType_Long       -> Just $ PrimIntegral $ PrimLong     Signed
      CXType_ULong      -> Just $ PrimIntegral $ PrimLong     Unsigned
      CXType_LongLong   -> Just $ PrimIntegral $ PrimLongLong Signed
      CXType_ULongLong  -> Just $ PrimIntegral $ PrimLongLong Unsigned
      CXType_Float      -> Just $ PrimFloating $ PrimFloat
      CXType_Double     -> Just $ PrimFloating $ PrimDouble
      CXType_LongDouble -> Just $ PrimFloating $ PrimLongDouble
      _otherwise        -> Nothing

