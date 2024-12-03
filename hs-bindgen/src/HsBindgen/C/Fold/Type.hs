-- | Types
--
-- TODO: We should make the distinction between (C) type declarations and type
-- uses clear at the (Haskell) type level. Once we do, we should cull the
-- export list of this module.
module HsBindgen.C.Fold.Type (
    processTypeDecl,
) where

import Control.Monad.State (State, get, put, gets)
import Foreign.C
import Data.Map.Ordered.Strict qualified as OMap
import Data.Text qualified as T

import HsBindgen.C.AST

import HsBindgen.Imports
import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.DeclState
import HsBindgen.C.Reparse
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Eff
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Process top-level (type) declration
processTypeDecl :: CXTranslationUnit -> CXType -> Eff (State DeclState) ()
processTypeDecl unit ty = do
    -- name <- CName <$> liftIO (clang_getTypeSpelling ty)
    -- liftIO $ putStrLn $ "processTypeDecl " ++ show (name, ty)

    s <- get

    case OMap.lookup ty (typeDeclarations s) of
        Nothing                      -> void $ processTypeDecl' PathTop unit ty
        Just (TypeDecl _ _)          -> return ()
        Just (TypeDeclAlias _)       -> return ()
        Just (TypeDeclProcessing t') -> liftIO $ fail $ "Incomplete type declaration: " ++ show t'

data Path
    = PathTop
    | PathStruct (Maybe CName) Path
    | PathField CName Path
    -- TODO: | PathPtr Path
    -- TODO: | PathConstArray Natural Path
  deriving Show

isPathTop :: Path -> Bool
isPathTop PathTop = True
isPathTop _       = False

-- | Make definition name for anonymous structures from their path
mkDefnName :: Path -> DefnName
mkDefnName = DefnName . CName . go where
    -- TODO: temporary, expose name structure as is in DefnName
    go :: Path -> Text
    go PathTop = "ANONYMOUS" -- shouldn't happen
    go (PathField n p) = go p <> getCName n
    go (PathStruct Nothing p) = go p
    go (PathStruct (Just n) _) = getCName n

processTypeDeclRec :: Path -> CXTranslationUnit -> CXType -> Eff (State DeclState) Type
processTypeDeclRec path unit ty = do
    -- name <- CName <$> liftIO (clang_getTypeSpelling ty)
    -- liftIO $ putStrLn $ "processTypeDeclRec " ++ show (name, ty)

    s <- get
    case OMap.lookup ty (typeDeclarations s) of
        Nothing                     -> processTypeDecl' path unit ty
        Just (TypeDecl t _)         -> return t
        Just (TypeDeclAlias t)      -> return t
        Just (TypeDeclProcessing t) -> return t

-- Process types.
--
-- Note: it is a bit tricky to process typedefs.
-- Consider following shape of code
--
--   typedef struct foo { ... } bar;
--
-- libclang represents that single declration as two in its asT:
--
--   struct foo {...};
--   typedef struct foo bar;
--
-- As far as I can tell, it's virtually impossible to distingiush these two reliably.
--
-- That is also true if the struct is unnamed, i.e. there is no name `foo`.
--
--   struct { ... };
--   typedef struct (unnamed at ...) bar;
--
-- LLVM-16 has also changed clang_getCursorSpelling results for such anonymous
-- structures.
--
-- Thus it seems that the most robust approach is to use clang_getTypeSpelling
-- and strip possible "struct " prefix already here.
-- (i.e. work closer to C++ rules, where struct foo { ... } kind of includes type typedef).
--
-- In practice, this means that we cannot mangle names differently in cases where
-- libclang already blurs the differences before we are able to see them.
-- In particular a contrived corner cases like
--
--   struct foo { int x; int y; };
--   typedef double foo;
--
-- will result in invalid code generated.
--
-- https://github.com/well-typed/hs-bindgen/issues/306
-- https://github.com/well-typed/hs-bindgen/issues/314
--
processTypeDecl' :: Path -> CXTranslationUnit -> CXType -> Eff (State DeclState) Type
processTypeDecl' path unit ty = case fromSimpleEnum $ cxtKind ty of
    kind | Just prim <- primType kind -> do
        return $ TypePrim prim

    -- elaborated types, we follow the definition.
    Right CXType_Elaborated -> do
        ty' <- liftIO $ clang_Type_getNamedType ty
        processTypeDeclRec path unit ty'

    -- typedefs
    Right CXType_Typedef -> do
        -- getTypedefName returns the same string as clang_getTypeSpellingg
        name <- CName <$> liftIO (clang_getTypedefName ty)
        let ctype = TypeTypedef name
        addTypeDeclProcessing ty ctype

        decl <- liftIO (clang_getTypeDeclaration ty)
        tag <- CName <$> liftIO (clang_getCursorSpelling decl)
        ty' <- liftIO (clang_getTypedefDeclUnderlyingType decl)

        use <- processTypeDeclRec  PathTop unit ty'

        -- we could check whether typedef has a transparent tag,
        -- like in case of `typedef struct foo { ..} foo;`
        -- but we don't use that for anything.
        --
        -- transparent <- liftIO (clang_Type_isTransparentTagTypedef ty)

        case use of
            -- If names match, skip.
            -- Note: this is not the same as clang_Type_isTransparentTagTypedef,
            -- in typedef struct { ... } foo; the typedef does not have transparent tag.
            --
            TypeStruct (DefnName n) | n == tag ->
                addAlias ty use

            TypeEnum n | n == tag ->
                addAlias ty use

            _ -> do
                --
                -- record name-path properly in underlying struct. (something like Path)
                --
                -- TODO: handle typedef struct|enum {..} ty; // pattern

                addDecl ty $ DeclTypedef $ Typedef tag use

    -- structs
    Right CXType_Record -> do
        decl <- liftIO (clang_getTypeDeclaration ty)
        -- TODO: don't use getCursorSpelling.
        tag <- liftIO (clang_getCursorSpelling decl)
        name <- liftIO (clang_getTypeSpelling ty)
        anon <- liftIO (clang_Cursor_isAnonymous decl)

        if anon && isPathTop path
        then do
            -- anonymous declration, nothing to do
            -- warn, we shouldn't reach that in "good" code.
            return $ TypePrim PrimVoid

        else do
            let cname :: Maybe CName
                defnName :: DefnName

                (cname, defnName)
                    | anon
                    = (Nothing, mkDefnName path)

                    | let n = CName (either id id $ structSpelling name)
                    , otherwise = (Just n, DefnName n)

            addTypeDeclProcessing ty $ TypeStruct defnName

            liftIO (HighLevel.classifyDeclaration decl) >>= \case
                DeclarationOpaque ->
                    addDecl ty (DeclOpaqueStruct (CName tag)) -- TODO: use defnname

                DeclarationForward _defn -> do
                    liftIO $ fail "should not happen"

                DeclarationRegular -> do
                    sizeof    <- liftIO (clang_Type_getSizeOf  ty)
                    alignment <- liftIO (clang_Type_getAlignOf ty)

                    fields <- HighLevel.clang_visitChildren decl $ \cursor -> do
                        mfield <- mkStructField unit (PathStruct cname path) cursor
                        return $ Continue mfield

                    addDecl ty $ DeclStruct Struct
                        { structTag       = defnName
                        , structSizeof    = fromIntegral sizeof
                        , structAlignment = fromIntegral alignment
                        , structFields    = fields
                        }

    -- enum
    Right CXType_Enum -> do
        decl <- liftIO (clang_getTypeDeclaration ty)
        name <- liftIO (clang_getTypeSpelling ty)
        anon <- liftIO (clang_Cursor_isAnonymous decl)

        if anon
        then do
            -- anonymous declration, nothing to do

            -- TODO: check with struct foo { struct { ... } field; };
            return $ TypePrim PrimVoid

        else do
            let defnName :: CName
                defnName = case enumSpelling name of
                    Left n -> CName n
                    Right n -> CName n

            addTypeDeclProcessing ty $ TypeEnum defnName

            liftIO (HighLevel.classifyDeclaration decl) >>= \case
                    DeclarationOpaque -> do
                        addDecl ty (DeclOpaqueEnum defnName)

                    DeclarationForward _defn -> do
                        liftIO $ fail "should not happen"

                    DeclarationRegular -> do
                        sizeof    <- liftIO (clang_Type_getSizeOf  ty)
                        alignment <- liftIO (clang_Type_getAlignOf ty)
                        ety       <- liftIO (clang_getEnumDeclIntegerType decl) >>= processTypeDeclRec PathTop unit

                        values <- HighLevel.clang_visitChildren decl $ \cursor -> do
                            mvalue <- mkEnumValue cursor
                            return $ Continue mvalue

                        addDecl ty $ DeclEnum $ Enu
                            { enumTag       = defnName
                            , enumType      = ety
                            , enumSizeof    = fromIntegral sizeof
                            , enumAlignment = fromIntegral alignment
                            , enumValues    = values
                            }

    Right CXType_Pointer -> do
        pointee <- liftIO $ clang_getPointeeType ty
        -- TOOD: think about what path should be
        pointee' <- processTypeDeclRec path unit pointee
        return (TypePointer pointee')

    Right CXType_ConstantArray -> do
        n <- liftIO $ clang_getArraySize ty
        e <- liftIO $ clang_getArrayElementType ty
        e' <- processTypeDeclRec path unit e
        return (TypeConstArray (fromIntegral n) e')

    Right CXType_Void -> do
        return $ TypePrim PrimVoid

    Right CXType_Bool -> do
        return $ TypePrim PrimBool

    Right CXType_FunctionProto -> do
        -- TODO: for now we represent function types as Void
        return $ TypePrim PrimVoid

    _ -> do
      name <- CName <$> liftIO (clang_getTypeSpelling ty)
      liftIO $ print name
      unrecognizedType ty

structSpelling :: Text -> Either Text Text
structSpelling n
    | Just sfx <- T.stripPrefix "struct " n = Left sfx
    | otherwise                             = Right n

enumSpelling :: Text -> Either Text Text
enumSpelling n
    | Just sfx <- T.stripPrefix "enum " n = Left sfx
    | otherwise                           = Right n

addAlias :: CXType -> Type -> Eff (State DeclState) Type
addAlias ty t = do
    s <- get
    let ds = typeDeclarations s
    case OMap.lookup ty ds of
        Nothing -> liftIO $ fail "type not being processed"
        Just (TypeDeclProcessing _t) -> do
            put s { typeDeclarations = omapInsertBack ty (TypeDeclAlias t) ds }
            return t
        Just (TypeDeclAlias _) -> liftIO $ fail "type already processed"
        Just (TypeDecl _ _) -> liftIO $ fail "type already processed"

addTypeDeclProcessing :: CXType -> Type -> Eff (State DeclState) ()
addTypeDeclProcessing ty t = do
    s <- get
    let ds = typeDeclarations s
    case OMap.lookup ty ds of
        Nothing -> put s { typeDeclarations = omapInsertBack ty (TypeDeclProcessing t) ds }
        Just (TypeDeclProcessing t') -> liftIO $ fail $ "type already processed (1)" ++ show (t, t')
        Just (TypeDecl t' _) -> liftIO $ fail $ "type already processed (2)" ++ show (t, t')
        Just (TypeDeclAlias t') -> liftIO $ fail $ "type already processed (3)" ++ show (t, t')

addDecl :: CXType -> Decl -> Eff (State DeclState) Type
addDecl ty d = do
    s <- get
    let ds = typeDeclarations s
    case OMap.lookup ty ds of
        Nothing -> liftIO $ fail "type not being processed"
        Just (TypeDeclProcessing t) -> do
            put s { typeDeclarations = omapInsertBack ty (TypeDecl t d) ds }
            return t
        Just (TypeDecl _ _)    -> liftIO $ fail "type already processed"
        Just (TypeDeclAlias _) -> liftIO $ fail "type already processed"

-- https://github.com/dmwit/ordered-containers/issues/29
omapInsertBack :: Ord k => k -> v -> OMap.OMap k v -> OMap.OMap k v
omapInsertBack k v m = m OMap.>| (k, v)

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

mkStructField ::
     CXTranslationUnit
  -> Path
  -> CXCursor
  -> Eff (State DeclState) (Maybe StructField)
mkStructField unit path current = do
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

        else do
          fieldName   <- CName <$> liftIO (clang_getCursorDisplayName current)
          ty          <- liftIO (clang_getCursorType current)
          -- TODO: correct path
          fieldType   <- processTypeDeclRec (PathField fieldName path) unit ty
          fieldOffset <- fromIntegral <$> liftIO (clang_Cursor_getOffsetOfField current)

          unless (fieldOffset `mod` 8 == 0) $ error "bit-fields not supported yet"

          return $ Just StructField{fieldName, fieldOffset, fieldType}

      -- inner structs, there are two approaches:
      -- * process eagerly
      -- * process when encountered in a field
      --
      -- For now we chose the latter.
      Right CXCursor_StructDecl ->
        return Nothing

      _other ->
        unrecognizedCursor current

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

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
