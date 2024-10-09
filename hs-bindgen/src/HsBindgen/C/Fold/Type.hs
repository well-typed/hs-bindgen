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
  , mkTypedefHeader
  ) where

import Control.Monad.State
import Foreign.C
import GHC.Stack

import HsBindgen.C.AST
import HsBindgen.C.Fold.Common
import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.Classification
import HsBindgen.Clang.Util.Fold
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Fold type /declaration/
foldTypeDecl :: HasCallStack => Fold s Typ
foldTypeDecl current = do
    cursorKind <- liftIO $ clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_StructDecl -> do
        mkStruct <- mkStructHeader current
        let mkDecl :: [StructField] -> Maybe Typ
            mkDecl = Just . TypStruct . mkStruct
        return $ Recurse (continue mkStructField) mkDecl
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

mkStructField :: MonadIO m => CXCursor -> m StructField
mkStructField current = liftIO $ do
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

mkTypedefHeader :: MonadIO m => CXCursor -> m (Typ -> Typedef)
mkTypedefHeader current = liftIO $ do
    typedefName <- CName <$> clang_getCursorDisplayName current
    return $ \typedefType -> Typedef{
          typedefName
        , typedefType
        }

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


