-- | Types
--
-- TODO: We should make the distinction between (C) type declarations and type
-- uses clear at the (Haskell) type level. Once we do, we should cull the
-- export list of this module.
module HsBindgen.C.Fold.Type (
    processTypeDecl,
) where

import Control.Monad.State (State, get, put, gets)
import Data.Map.Ordered.Strict qualified as OMap
import Data.Text qualified as T
import Foreign.C

import HsBindgen.C.AST

import HsBindgen.Imports
import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.DeclState
import HsBindgen.C.Reparse
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Eff
import HsBindgen.Runtime.Enum.Simple

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Process top-level (type) declration
processTypeDecl ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> CXTranslationUnit
  -> CXType
  -> Eff (State DeclState) Type
processTypeDecl relPath unit ty = do
    -- dtraceIO "processTypeDecl" ty

    s <- get

    case OMap.lookup ty (typeDeclarations s) of
        Nothing                      -> processTypeDecl' relPath DeclPathTop unit ty
        Just (TypeDecl t _)          -> return t
        Just (TypeDeclAlias t)       -> return t
        Just (TypeDeclProcessing t') -> liftIO $ fail $ "Incomplete type declaration: " ++ show t'

processTypeDeclRec ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> DeclPath
  -> CXTranslationUnit
  -> CXType
  -> Eff (State DeclState) Type
processTypeDeclRec relPath path unit ty = do
    -- dtraceIO "processTypeDeclRec" ty

    s <- get
    case OMap.lookup ty (typeDeclarations s) of
        Nothing                     -> processTypeDecl' relPath path unit ty
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
processTypeDecl' ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> DeclPath
  -> CXTranslationUnit
  -> CXType
  -> Eff (State DeclState) Type
processTypeDecl' relPath path unit ty = case fromSimpleEnum $ cxtKind ty of
    kind | Just prim <- primType kind -> do
        return $ TypePrim prim

    -- elaborated types, we follow the definition.
    Right CXType_Elaborated -> do
        ty' <- liftIO $ clang_Type_getNamedType ty
        processTypeDeclRec relPath path unit ty'

    -- typedefs
    Right CXType_Typedef -> do
        -- getTypedefName returns the same string as clang_getTypeSpellingg
        name <- CName <$> liftIO (clang_getTypedefName ty)
        let ctype = TypeTypedef name
        addTypeDeclProcessing ty ctype

        decl <- liftIO (clang_getTypeDeclaration ty)
        tag <- CName <$> liftIO (clang_getCursorSpelling decl)
        ty' <- liftIO (clang_getTypedefDeclUnderlyingType decl)

        sloc <- liftIO $
          HighLevel.clang_getExpansionLocation relPath
            =<< clang_getCursorLocation decl

        use <- processTypeDeclRec relPath DeclPathTop unit ty'

        -- we could check whether typedef has a transparent tag,
        -- like in case of `typedef struct foo { ..} foo;`
        -- but we don't use that for anything.
        --
        -- transparent <- liftIO (clang_Type_isTransparentTagTypedef ty)

        case use of
            -- If names match, skip.
            -- Note: this is not the same as clang_Type_isTransparentTagTypedef,
            -- in typedef struct { ... } foo; the typedef does not have transparent tag.
            TypeStruct (DeclPathStruct declName _declPath)
              | declName == DeclNameTag tag -> addAlias ty use
              | declName == DeclNameTypedef tag -> addAlias ty use

            TypeEnum n | n == tag ->
                addAlias ty use

            _ -> do
                --
                -- record name-path properly in underlying struct. (something like Path)
                --
                -- TODO: handle typedef struct|enum {..} ty; // pattern

                addDecl ty $ DeclTypedef Typedef {
                    typedefName      = tag
                  , typedefType      = use
                  , typedefSourceLoc = sloc
                  }

    -- structs
    Right CXType_Record -> do
        decl <- liftIO (clang_getTypeDeclaration ty)
        -- TODO: don't use getCursorSpelling.
        tag <- liftIO (clang_getCursorSpelling decl)
        name <- liftIO (clang_getTypeSpelling ty)
        anon <- liftIO (clang_Cursor_isAnonymous decl)

        -- dtraceIO "record" (decl, tag, name, anon)

        sloc <- liftIO $
          HighLevel.clang_getExpansionLocation relPath
            =<< clang_getCursorLocation decl

        let declPath
              | anon      = DeclPathStruct DeclNameNone path
              | otherwise = case T.stripPrefix "struct " name of
                  Just n  -> DeclPathStruct (DeclNameTag (CName n))        path
                  Nothing -> DeclPathStruct (DeclNameTypedef (CName name)) path

        if declPath == DeclPathStruct DeclNameNone DeclPathTop
        then do
            -- Anonymous top-level declaration: nothing to do but warn, as there
            -- shouldn't be one in "good" code.
            return TypeVoid

        else do
            addTypeDeclProcessing ty $ TypeStruct declPath

            liftIO (HighLevel.classifyDeclaration decl) >>= \case
                DeclarationOpaque ->
                    -- TODO: use defnname
                    addDecl ty $ DeclOpaqueStruct OpaqueStruct {
                        opaqueStructTag       = CName tag
                      , opaqueStructSourceLoc = sloc
                      }

                DeclarationForward _defn -> do
                    liftIO $ fail "should not happen"

                DeclarationRegular -> do
                    sizeof    <- liftIO (clang_Type_getSizeOf  ty)
                    alignment <- liftIO (clang_Type_getAlignOf ty)

                    fields' <- HighLevel.clang_visitChildren decl $ \cursor -> do
                        mfield <- mkStructField relPath unit declPath cursor
                        return $ Continue mfield

                    (fields, flam) <- partitionFields fields'

                    addDecl ty $ DeclStruct Struct
                        { structDeclPath  = declPath
                        , structSizeof    = fromIntegral sizeof
                        , structAlignment = fromIntegral alignment
                        , structFields    = fields
                        , structFlam      = flam
                        , structSourceLoc = sloc
                        }

    -- enum
    Right CXType_Enum -> do
        decl <- liftIO (clang_getTypeDeclaration ty)
        sloc <- liftIO $
          HighLevel.clang_getExpansionLocation relPath
            =<< clang_getCursorLocation decl
        name <- liftIO (clang_getTypeSpelling ty)
        anon <- liftIO (clang_Cursor_isAnonymous decl)

        if anon
        then do
            -- anonymous declration, nothing to do

            -- TODO: check with struct foo { struct { ... } field; };
            return TypeVoid

        else do
            let defnName :: CName
                defnName = case enumSpelling name of
                    Left n -> CName n
                    Right n -> CName n

            addTypeDeclProcessing ty $ TypeEnum defnName

            liftIO (HighLevel.classifyDeclaration decl) >>= \case
                    DeclarationOpaque -> do
                        addDecl ty $ DeclOpaqueEnum OpaqueEnum {
                            opaqueEnumTag       = defnName
                          , opaqueEnumSourceLoc = sloc
                          }

                    DeclarationForward _defn -> do
                        liftIO $ fail "should not happen"

                    DeclarationRegular -> do
                        sizeof    <- liftIO (clang_Type_getSizeOf  ty)
                        alignment <- liftIO (clang_Type_getAlignOf ty)
                        ety       <- liftIO (clang_getEnumDeclIntegerType decl)
                          >>= processTypeDeclRec relPath DeclPathTop unit

                        values <- HighLevel.clang_visitChildren decl $ \cursor -> do
                            mvalue <- mkEnumValue relPath cursor
                            return $ Continue mvalue

                        addDecl ty $ DeclEnum $ Enu
                            { enumTag       = defnName
                            , enumType      = ety
                            , enumSizeof    = fromIntegral sizeof
                            , enumAlignment = fromIntegral alignment
                            , enumValues    = values
                            , enumSourceLoc = sloc
                            }

    Right CXType_Pointer -> do
        pointee <- liftIO $ clang_getPointeeType ty
        -- TOOD: think about what path should be
        pointee' <- processTypeDeclRec relPath path unit pointee
        return (TypePointer pointee')

    Right CXType_ConstantArray -> do
        n <- liftIO $ clang_getArraySize ty
        e <- liftIO $ clang_getArrayElementType ty
        e' <- processTypeDeclRec relPath path unit e
        return (TypeConstArray (fromIntegral n) e')

    Right CXType_Void -> do
        return TypeVoid

    Right CXType_Bool -> do
        return $ TypePrim PrimBool

    Right CXType_FunctionProto -> do
        -- TODO: fail on variadic types, these don't have great FFI support anyway. clang_isFunctionTypeVariadic
        -- TODO: we could record calling convention clang_getFunctionTypeCallingConv, but for CApiFFI it's irrelevant as it creates C wrappers with known convention
        res <- liftIO $ clang_getResultType ty
        res' <- processTypeDeclRec relPath path unit res

        nargs <- liftIO $ clang_getNumArgTypes ty
        args' <- forM [0 .. nargs - 1] $ \i -> do
            arg <- liftIO $ clang_getArgType ty (fromIntegral i)
            processTypeDeclRec relPath path unit arg

        return $ TypeFun args' res'

    Right CXType_IncompleteArray -> do
        e <- liftIO $ clang_getArrayElementType ty
        e' <- processTypeDeclRec relPath path unit e
        return (TypeIncompleteArray e')

    _ -> do
      name <- CName <$> liftIO (clang_getTypeSpelling ty)
      liftIO $ print name
      unrecognizedType ty

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

data Field
    = Normal !StructField
    | IncompleteArray !StructField
  deriving Show

type DList a = [a] -> [a]

partitionFields :: [Field] -> Eff m ([StructField],  Maybe StructField)
partitionFields = go id where
    go :: DList StructField -> [Field] -> Eff m ([StructField], Maybe StructField)
    go !fs []                       = return (fs [], Nothing)
    go !fs (IncompleteArray f : []) = return (fs [], Just f)
    go !_  (IncompleteArray _ : _)  = fail "incomplete array is not a last field"
    go !fs (Normal f : xs)          = go (fs . (f :)) xs

mkStructField ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> CXTranslationUnit
  -> DeclPath
  -> CXCursor
  -> Eff (State DeclState) (Maybe Field) -- ^ Left values are flexible array members.
mkStructField relPath unit path current = do
    fieldSourceLoc <- liftIO $
      HighLevel.clang_getExpansionLocation relPath
        =<< clang_getCursorLocation current
    cursorKind <- liftIO $ clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_UnexposedAttr ->
        return Nothing

      Right CXCursor_FieldDecl -> do
        extent   <- liftIO $ HighLevel.clang_getCursorExtent relPath current
        hasMacro <- gets $ containsMacroExpansion extent

        (fieldName, fieldType, isIncompleteArray) <- if hasMacro
          then liftIO $ do
            tokens <- HighLevel.clang_tokenize relPath unit (multiLocExpansion <$> extent)
            case reparseWith reparseFieldDecl tokens of
              Left err ->
                fail $ "mkStructField: " ++ show err
              Right (fieldType, fieldName) -> do
                -- Note: macro definitions don't work with incomplete arrays
                -- This is fine as reparseWith doesn't recognise array types atm.
                return (fieldName, fieldType, False)

          else do
            fieldName   <- CName <$> liftIO (clang_getCursorDisplayName current)
            ty          <- liftIO (clang_getCursorType current)
            case fromSimpleEnum $ cxtKind ty of
              Right CXType_IncompleteArray -> do
                e <- liftIO $ clang_getArrayElementType ty
                fieldType <- processTypeDeclRec relPath (DeclPathField fieldName path) unit e
                return (fieldName, fieldType, True)

              _ -> do
                fieldType <- processTypeDeclRec relPath (DeclPathField fieldName path) unit ty
                return (fieldName, fieldType, False)

        fieldOffset <- fromIntegral <$> liftIO (clang_Cursor_getOffsetOfField current)

        if isIncompleteArray
        then do
          assertEff (fieldOffset `mod` 8 == 0) "offset should be divisible by 8"
          return $ Just $ IncompleteArray StructField{fieldName, fieldOffset, fieldType, fieldSourceLoc, fieldWidth = Nothing}
        else do
          isBitField <- liftIO $ clang_Cursor_isBitField current
          if isBitField
          then do
            width <- liftIO $ clang_getFieldDeclBitWidth current
            return $ Just $ Normal StructField{fieldName, fieldOffset, fieldType, fieldSourceLoc, fieldWidth = Just (fromIntegral width)}
          else do
            assertEff (fieldOffset `mod` 8 == 0) "offset should be divisible by 8"
            return $ Just $ Normal StructField{fieldName, fieldOffset, fieldType, fieldSourceLoc, fieldWidth = Nothing}

      -- inner structs, there are two approaches:
      -- * process eagerly
      -- * process when encountered in a field
      --
      -- For now we chose the latter.
      Right CXCursor_StructDecl ->
        return Nothing

      _other ->
        unrecognizedCursor relPath current

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

mkEnumValue ::
     MonadIO m
  => Maybe FilePath -- ^ Directory to make paths relative to
  -> CXCursor
  -> m (Maybe EnumValue)
mkEnumValue relPath current = liftIO $ do
    valueSourceLoc <- liftIO $
      HighLevel.clang_getExpansionLocation relPath
        =<< clang_getCursorLocation current
    cursorKind <- liftIO $ clang_getCursorKind current

    case fromSimpleEnum cursorKind of
      Right CXCursor_EnumConstantDecl -> do
        valueName  <- CName <$> clang_getCursorDisplayName current
        valueValue <- toInteger <$> clang_getEnumConstantDeclValue current
        return $ Just EnumValue{valueName, valueValue, valueSourceLoc}
      Right CXCursor_PackedAttr ->
        -- TODO: __attribute__(packed))
        return Nothing
      _otherwise ->
        -- there could be attributes, e.g. packed
        unrecognizedCursor relPath current

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
      CXType_Short      -> Just $ PrimIntegral PrimShort    Signed
      CXType_UShort     -> Just $ PrimIntegral PrimShort    Unsigned
      CXType_Int        -> Just $ PrimIntegral PrimInt      Signed
      CXType_UInt       -> Just $ PrimIntegral PrimInt      Unsigned
      CXType_Long       -> Just $ PrimIntegral PrimLong     Signed
      CXType_ULong      -> Just $ PrimIntegral PrimLong     Unsigned
      CXType_LongLong   -> Just $ PrimIntegral PrimLongLong Signed
      CXType_ULongLong  -> Just $ PrimIntegral PrimLongLong Unsigned
      CXType_Float      -> Just $ PrimFloating $ PrimFloat
      CXType_Double     -> Just $ PrimFloating $ PrimDouble
      CXType_LongDouble -> Just $ PrimFloating $ PrimLongDouble
      _otherwise        -> Nothing
