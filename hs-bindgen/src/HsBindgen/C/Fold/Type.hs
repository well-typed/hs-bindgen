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

import Data.DynGraph qualified as DynGraph
import HsBindgen.Imports
import HsBindgen.Errors
import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.DeclState
import HsBindgen.C.Reparse
import HsBindgen.Clang.CNameSpelling
import HsBindgen.Clang.HighLevel qualified as HighLevel
import HsBindgen.Clang.HighLevel.Types
import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Eff
import HsBindgen.ExtBindings
import HsBindgen.Runtime.Enum.Simple
import HsBindgen.Util.Tracer (prettyLogMsg)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Process top-level (type) declration
processTypeDecl ::
     ExtBindings
  -> CXTranslationUnit
  -> Maybe CXCursor
  -> CXType
  -> Eff (State DeclState) Type
processTypeDecl extBindings unit declCursor ty = do
    -- dtraceIO "processTypeDecl" ty
    s <- get
    case OMap.lookup ty (typeDeclarations s) of
        Nothing                        -> processTypeDecl' DeclPathCtxtTop extBindings unit declCursor ty
        Just (TypeDecl t _)            -> return t
        Just (TypeDeclAlias t)         -> return t
        Just (TypeDeclProcessing t' _) -> liftIO $ panicIO $ "Incomplete type declaration: " ++ show t'

processTypeDeclRec ::
     DeclPathCtxt
  -> ExtBindings
  -> CXTranslationUnit
  -> Maybe CXCursor
  -> CXType
  -> Eff (State DeclState) Type
processTypeDeclRec ctxt extBindings unit curr ty = do
    s <- get
    case OMap.lookup ty (typeDeclarations s) of
        Nothing                       -> processTypeDecl' ctxt extBindings unit curr ty
        Just (TypeDecl t _)           -> return t
        Just (TypeDeclAlias t)        -> return t
        Just (TypeDeclProcessing t _) -> return t

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
     DeclPathCtxt
  -> ExtBindings
  -> CXTranslationUnit
  -> Maybe CXCursor
      -- ^ cursor of the type declaration; only used for reparsing
      -- function declarations containing macros
  -> CXType
  -> Eff (State DeclState) Type
processTypeDecl' ctxt extBindings unit declCursor ty = case fromSimpleEnum $ cxtKind ty of
    kind | Just prim <- primType kind -> do
        return $ TypePrim prim

    -- elaborated types, we follow the definition.
    Right CXType_Elaborated -> do
        ty' <- liftIO $ clang_Type_getNamedType ty
        processTypeDeclRec ctxt extBindings unit Nothing ty'

    -- typedefs
    Right CXType_Typedef -> do
        -- getTypedefName returns the same string as clang_getTypeSpelling
        name <- liftIO (clang_getTypedefName ty)
        let ctype = TypeTypedef $ CName name
        addTypeDeclProcessing ty ctype

        decl <- liftIO (clang_getTypeDeclaration ty)
        sloc <- liftIO $
            HighLevel.clang_getExpansionLocation =<< clang_getCursorLocation decl

        mExtId <- lookupExtBinding (CNameSpelling name) sloc extBindings
        case mExtId of
            Just extId -> addAlias ty $ TypeExtBinding extId
            Nothing -> do
                tag <- CName <$> liftIO (clang_getCursorSpelling decl)
                ty' <- liftIO $ getElaborated =<< clang_getTypedefDeclUnderlyingType decl
                use <- processTypeDeclRec (DeclPathCtxtTypedef tag) extBindings unit Nothing ty'

                -- we could check whether typedef has a transparent tag,
                -- like in case of `typedef struct foo { ..} foo;`
                -- but we don't use that for anything.
                --
                -- transparent <- liftIO (clang_Type_isTransparentTagTypedef ty)

                case use of
                    -- If names match, skip.
                    -- Note: this is not the same as clang_Type_isTransparentTagTypedef,
                    -- in typedef struct { ... } foo; the typedef does not have transparent tag.
                    TypeStruct (DeclPathName declName _ctxt) | declName == tag -> do
                            updateDeclAddAlias ty' tag
                            addAlias ty use
                    TypeStruct (DeclPathAnon (DeclPathCtxtTypedef typedefName)) | typedefName == tag ->
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

    -- structs and unions
    Right CXType_Record -> do
        decl <- liftIO (clang_getTypeDeclaration ty)
        ki <- liftIO $ fromSimpleEnum <$> clang_getCursorKind decl
        case ki of
            Right CXCursor_StructDecl -> do
                name <- liftIO (clang_getTypeSpelling ty)
                anon <- liftIO (clang_Cursor_isAnonymous decl)

                -- dtraceIO "record" (decl, name, anon)

                -- Something like
                --
                -- > typedef struct {
                -- >     char a;
                -- > } S3_t;
                --
                -- is not considered an anonymous struct by @clang@, but we /do/
                -- treat it as one.

                let declPath
                      | anon      = DeclPathAnon ctxt
                      | otherwise = case T.stripPrefix "struct " name of
                          Just n  -> DeclPathName (CName n) ctxt
                          Nothing -> DeclPathAnon (DeclPathCtxtTypedef (CName name))

                -- name for opaque types.
                --
                -- TODO: Could we use 'Nothing' for @anon@?
                let name'
                      | anon      = ""
                      | otherwise =  case T.stripPrefix "struct " name of
                          Just n  -> n
                          Nothing -> name

                if declPath == DeclPathAnon DeclPathCtxtTop
                then do
                    -- Anonymous top-level declaration: nothing to do but warn, as there
                    -- shouldn't be one in "good" code.
                    return TypeVoid

                else do
                    addTypeDeclProcessing ty $ TypeStruct declPath
                    sloc <- liftIO $
                        HighLevel.clang_getExpansionLocation
                            =<< clang_getCursorLocation decl

                    mExtId <- case declPath of
                        DeclPathName{}->
                            lookupExtBinding (CNameSpelling name) sloc extBindings
                        DeclPathAnon{} ->
                            return Nothing
                    case mExtId of
                        Just extId -> addAlias ty $ TypeExtBinding extId
                        Nothing -> liftIO (HighLevel.classifyDeclaration decl) >>= \case
                            DeclarationOpaque ->
                                addDecl ty $ DeclOpaqueStruct OpaqueStruct {
                                      opaqueStructTag       = CName name'
                                    , opaqueStructAliases   = []
                                    , opaqueStructSourceLoc = sloc
                                    }

                            DeclarationForward _defn -> do
                                liftIO $ panicIO "should not happen"

                            DeclarationRegular -> do
                                sizeof    <- liftIO (clang_Type_getSizeOf  ty)
                                alignment <- liftIO (clang_Type_getAlignOf ty)

                                fields' <- HighLevel.clang_visitChildren decl $ \cursor -> do
                                    mfield <- mkStructField extBindings unit (if anon then Nothing else Just $ CName name') ctxt cursor
                                    return $ Continue mfield

                                (fields, flam) <- partitionFields fields'

                                addDecl ty $ DeclStruct Struct
                                    { structDeclPath  = declPath
                                    , structAliases   = []
                                    , structSizeof    = fromIntegral sizeof
                                    , structAlignment = fromIntegral alignment
                                    , structFields    = fields
                                    , structFlam      = flam
                                    , structSourceLoc = sloc
                                    }

            Right CXCursor_UnionDecl -> do
                name <- liftIO (clang_getTypeSpelling ty)
                anon <- liftIO (clang_Cursor_isAnonymous decl)

                -- dtraceIO "union" (decl, name, anon)

                let declPath
                      | anon      = DeclPathAnon ctxt
                      | otherwise = case T.stripPrefix "union " name of
                          Just n  -> DeclPathName (CName n) ctxt
                          Nothing -> DeclPathAnon (DeclPathCtxtTypedef (CName name))

                -- name for opaque types.
                let name'
                      | anon      = ""
                      | otherwise =  case T.stripPrefix "union " name of
                          Just n  -> n
                          Nothing -> name

                if declPath == DeclPathAnon DeclPathCtxtTop
                then do
                    -- Anonymous top-level declaration: nothing to do but warn, as there
                    -- shouldn't be one in "good" code.
                    return TypeVoid

                else do
                    addTypeDeclProcessing ty $ TypeUnion declPath
                    sloc <- liftIO $
                        HighLevel.clang_getExpansionLocation
                            =<< clang_getCursorLocation decl

                    -- TODO: ExtBindings?
                    liftIO (HighLevel.classifyDeclaration decl) >>= \case
                            DeclarationOpaque ->
                                -- opaque struct and opaque union look the same.
                                addDecl ty $ DeclOpaqueStruct OpaqueStruct {
                                      opaqueStructTag       = CName name'
                                    , opaqueStructAliases   = []
                                    , opaqueStructSourceLoc = sloc
                                    }

                            DeclarationForward _defn -> do
                                liftIO $ panicIO "should not happen"

                            DeclarationRegular -> do
                                -- the below is TODO:
                                sizeof    <- liftIO (clang_Type_getSizeOf  ty)
                                alignment <- liftIO (clang_Type_getAlignOf ty)

                                fields <- HighLevel.clang_visitChildren decl $ \cursor -> do
                                    mfield <- mkUnionField extBindings unit (if anon then Nothing else Just $ CName name') ctxt cursor
                                    return $ Continue mfield

                                addDecl ty $ DeclUnion Union
                                    { unionDeclPath  = declPath
                                    , unionAliases   = []
                                    , unionSizeof    = fromIntegral sizeof
                                    , unionAlignment = fromIntegral alignment
                                    , unionFields    = fields
                                    , unionSourceLoc = sloc
                                    }

            _ -> panicIO $ show ki
    -- enum
    Right CXType_Enum -> do
        decl <- liftIO (clang_getTypeDeclaration ty)
        name <- liftIO (clang_getTypeSpelling ty)
        anon <- liftIO (clang_Cursor_isAnonymous decl)

        if anon
        then do
            -- anonymous declaration, nothing to do
            -- TODO: This is wrong, they can be nested.
            return TypeVoid
        else do
            let declPath
                  | anon      = DeclPathAnon ctxt
                  | otherwise = case T.stripPrefix "enum " name of
                      Just n  -> DeclPathName (CName n) ctxt
                      Nothing -> DeclPathAnon (DeclPathCtxtTypedef (CName name))

            -- name for opaque types.
            let name'
                  | anon      = ""
                  | otherwise =  case T.stripPrefix "enum " name of
                      Just n  -> n
                      Nothing -> name

            addTypeDeclProcessing ty $ TypeEnum declPath
            sloc <- liftIO $
                HighLevel.clang_getExpansionLocation
                    =<< clang_getCursorLocation decl

            mExtId <- lookupExtBinding (CNameSpelling name) sloc extBindings
            case mExtId of
                Just extId -> addAlias ty $ TypeExtBinding extId
                Nothing -> liftIO (HighLevel.classifyDeclaration decl) >>= \case
                    DeclarationOpaque -> do
                        addDecl ty $ DeclOpaqueEnum OpaqueEnum {
                            opaqueEnumTag       = CName name'
                          , opaqueEnumAliases   = []
                          , opaqueEnumSourceLoc = sloc
                          }

                    DeclarationForward _defn -> do
                        liftIO $ panicIO "should not happen"

                    DeclarationRegular -> do
                        sizeof    <- liftIO (clang_Type_getSizeOf  ty)
                        alignment <- liftIO (clang_Type_getAlignOf ty)
                        ety       <- liftIO (clang_getEnumDeclIntegerType decl)
                          >>= processTypeDeclRec DeclPathCtxtTop extBindings unit Nothing

                        values <- HighLevel.clang_visitChildren decl $ \cursor -> do
                            mvalue <- mkEnumValue cursor
                            return $ Continue mvalue

                        addDecl ty $ DeclEnum $ Enu
                            { enumDeclPath  = declPath
                            , enumAliases   = []
                            , enumType      = ety
                            , enumSizeof    = fromIntegral sizeof
                            , enumAlignment = fromIntegral alignment
                            , enumValues    = values
                            , enumSourceLoc = sloc
                            }

    Right CXType_Pointer -> do
        pointee <- liftIO $ clang_getPointeeType ty
        pointee' <- processTypeDeclRec (DeclPathCtxtPtr ctxt) extBindings unit Nothing pointee
        return (TypePointer pointee')

    Right CXType_ConstantArray -> do
        n <- liftIO $ clang_getArraySize ty
        e <- liftIO $ clang_getArrayElementType ty
        -- TODO: This context should use 'DeclPathCtxtConstArray'
        e' <- processTypeDeclRec ctxt extBindings unit Nothing e
        return (TypeConstArray (fromIntegral n) e')

    Right CXType_Void -> do
        return TypeVoid

    Right CXType_Bool -> do
        return $ TypePrim PrimBool

    Right CXType_FunctionProto -> do
        mbProtoWithMacros <-
          case declCursor of
            Nothing ->
              -- This should only happen when we are recurring into a
              -- function which takes function (pointers) as parameters.
              --
              -- We only do such manual recursion if the entire function doesn't
              -- have macros, so we are OK to report no macros here.
              return Nothing
            Just cx -> do
              protoExtent <- liftIO $ HighLevel.clang_getCursorExtent cx
              hasMacro <- gets $ containsMacroExpansion protoExtent
              return $
                if hasMacro
                then Just protoExtent
                else Nothing

        mbTyWithMacros <-
          case mbProtoWithMacros of
            Nothing -> return Nothing
            Just protoExtent -> do
              -- TODO: if macro expansion is confined to the function parameters,
              -- we don't need to reparse the whole function as we do here.
              tokens <- liftIO $ HighLevel.clang_tokenize unit (multiLocExpansion <$> protoExtent)
              macroTyEnv <- macroTypes <$> get
              case reparseWith (reparseFunDecl macroTyEnv) tokens of
                  Left err -> do
                    -- TODO: improve mechanism for reporting warnings
                    liftIO $ putStrLn $ unlines
                      [ "\nWarning: failed to re-parse function declaration containing macro expansion."
                      , "Proceeding with macros expanded."
                      , ""
                      , "Parse error:"
                      , prettyLogMsg err
                      , ""
                      ]
                    return Nothing
                  Right ((args, res), _fnName) ->
                    -- For function pointers, the number of pointers will get added
                    -- on by the CXType_Pointer case of processTypeDecl'.
                    return $ Just $ TypeFun args res
        case mbTyWithMacros of
          Just funTy -> return funTy
          Nothing -> do
            -- TODO: fail on variadic types, these don't have great FFI support anyway
            -- (clang_isFunctionTypeVariadic)
            -- TODO: we could record calling convention (clang_getFunctionTypeCallingConv),
            -- but for CApiFFI it's irrelevant as it creates C wrappers with known convention

            res <- liftIO $ clang_getResultType ty
            res' <- processTypeDeclRec ctxt extBindings unit Nothing res
            nargs <- liftIO $ clang_getNumArgTypes ty
            args' <- forM [0 .. nargs - 1] $ \i -> do
              arg <- liftIO $ clang_getArgType ty (fromIntegral i)
              processTypeDeclRec ctxt extBindings unit Nothing arg

            -- There are no macros in the function, hence no macros in the
            -- function argument or return types either. This is why it's OK
            -- to pass 'Nothing' as a CXCursor above (it is only used for re-parsing).

            return $ TypeFun args' res'

    Right CXType_IncompleteArray -> do
        e <- liftIO $ clang_getArrayElementType ty
        -- TODO: Should this also use 'DeclPathCtxtConstArray'?
        e' <- processTypeDeclRec ctxt extBindings unit Nothing e
        return (TypeIncompleteArray e')

    _ -> do
      name <- CName <$> liftIO (clang_getTypeSpelling ty)
      liftIO $ print name
      unrecognizedType ty

getElaborated :: CXType -> IO CXType
getElaborated ty = case fromSimpleEnum (cxtKind ty) of
    Right CXType_Elaborated -> getElaborated =<< clang_Type_getNamedType ty
    _otherwise              -> return ty

lookupExtBinding ::
     CNameSpelling
  -> SingleLoc
  -> ExtBindings
  -> Eff (State DeclState) (Maybe ExtIdentifier)
lookupExtBinding cname sloc extBindings =
    case lookupExtBindingsType cname extBindings of
      Nothing -> return Nothing
      Just ps -> do
        graph <- gets cIncludePathGraph
        let path = singleLocPath sloc
        return $ lookupExtIdentifier (graph `DynGraph.reaches` path) ps

addAlias :: CXType -> Type -> Eff (State DeclState) Type
addAlias ty t = do
    s <- get
    let ds = typeDeclarations s
    case OMap.lookup ty ds of
        Nothing -> liftIO $ panicIO "type not being processed"
        Just (TypeDeclProcessing _t _as) -> do
            put s { typeDeclarations = omapInsertBack ty (TypeDeclAlias t) ds }
            return t
        Just (TypeDeclAlias _) -> liftIO $ panicIO "type already processed"
        Just (TypeDecl _ _) -> liftIO $ panicIO "type already processed"

addTypeDeclProcessing :: CXType -> Type -> Eff (State DeclState) ()
addTypeDeclProcessing ty t = do
    s <- get
    let ds = typeDeclarations s
    case OMap.lookup ty ds of
        Nothing -> put s { typeDeclarations = omapInsertBack ty (TypeDeclProcessing t []) ds }
        Just (TypeDeclProcessing t' _as) -> liftIO $ panicIO $ "type already processed (1)" ++ show (t, t')
        Just (TypeDecl t' _) -> liftIO $ panicIO $ "type already processed (2)" ++ show (t, t')
        Just (TypeDeclAlias t') -> liftIO $ panicIO $ "type already processed (3)" ++ show (t, t')

addDecl :: CXType -> Decl -> Eff (State DeclState) Type
addDecl ty d = do
    s <- get
    let ds = typeDeclarations s
    case OMap.lookup ty ds of
        Nothing -> liftIO $ panicIO "type not being processed"
        Just (TypeDeclProcessing t aliases) -> do
            let err = "updateDeclAddAliases not implemented for type: " ++ show t
            d' <- maybe (liftIO (panicIO err)) return $
                    if null aliases
                        then Just d
                        else updateDeclAddAliases aliases d
            put s { typeDeclarations = omapInsertBack ty (TypeDecl t d') ds }
            return t
        Just (TypeDecl _ _)    -> liftIO $ panicIO "type already processed"
        Just (TypeDeclAlias _) -> liftIO $ panicIO "type already processed"

updateDeclAddAlias :: CXType -> CName -> Eff (State DeclState) ()
updateDeclAddAlias ty alias = do
    s <- get
    let ds = typeDeclarations s
    case OMap.lookup ty ds of
        Nothing -> liftIO $ panicIO "type not found"
        Just (TypeDeclProcessing typ aliases) ->
            let d = TypeDeclProcessing typ (alias : aliases)
            in  put s { typeDeclarations = (ty, d) OMap.<| ds }
        Just (TypeDecl typ decl) -> case updateDeclAddAliases [alias] decl of
            Just decl' ->
                put s { typeDeclarations = (ty, TypeDecl typ decl') OMap.<| ds }
            Nothing -> liftIO $ panicIO $
              "updateDeclAddAliases not implemented for type: " ++ show typ
        Just (TypeDeclAlias typ) -> liftIO $ panicIO $
          "cannot add alias to an alias: " ++ show typ

updateDeclAddAliases :: [CName] -> Decl -> Maybe Decl
updateDeclAddAliases aliases = \case
    DeclStruct struct -> Just $
        DeclStruct struct{ structAliases = aliases ++ structAliases struct }
    DeclOpaqueStruct ostruct -> Just $
        DeclOpaqueStruct ostruct{
                opaqueStructAliases = aliases ++ opaqueStructAliases ostruct
            }
    DeclUnion union -> Just $
        DeclUnion union{ unionAliases = aliases ++ unionAliases union }
    DeclEnum enu -> Just $
        DeclEnum enu{ enumAliases = aliases ++ enumAliases enu }
    DeclOpaqueEnum oenum -> Just $
        DeclOpaqueEnum oenum{
                opaqueEnumAliases = aliases ++ opaqueEnumAliases oenum
            }
    _otherwise -> Nothing

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
    go !_  (IncompleteArray _ : _)  = panicIO "incomplete array is not a last field"
    go !fs (Normal f : xs)          = go (fs . (f :)) xs

mkStructField ::
     ExtBindings
  -> CXTranslationUnit
  -> Maybe CName         -- ^ Name of the struct (unless anonymous)
  -> DeclPathCtxt
  -> CXCursor
  -> Eff (State DeclState) (Maybe Field) -- ^ Left values are flexible array members.
mkStructField extBindings unit mStructName ctxt current = do
    fieldSourceLoc <- liftIO $
      HighLevel.clang_getExpansionLocation =<< clang_getCursorLocation current
    cursorKind <- liftIO $ clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_UnexposedAttr ->
        return Nothing

      Right CXCursor_FieldDecl -> do
        extent   <- liftIO $ HighLevel.clang_getCursorExtent current
        hasMacro <- gets $ containsMacroExpansion extent

        mbNameTypeWithMacros <-
          if hasMacro
          then do
            tokens <- liftIO $ HighLevel.clang_tokenize unit (multiLocExpansion <$> extent)
            macroTyEnv <- macroTypes <$> get
            case reparseWith (reparseFieldDecl macroTyEnv) tokens of
              Left err -> do
                -- TODO: improve mechanism for reporting warnings
                liftIO $ putStrLn $ unlines
                  [ "\nWarning: failed to re-parse struct field containing macro expansion."
                  , "Proceeding with macros expanded."
                  , ""
                  , "Parse error:"
                  , prettyLogMsg err
                  , ""
                  ]
                return Nothing
              Right (fieldType, fieldName) ->
                return $ Just (fieldName, fieldType, isIncompleteArrayType fieldType)
          else
            return Nothing
        (fieldName, fieldType, isIncompleteArray) <-
          case mbNameTypeWithMacros of
            Just declNameAndTy -> return declNameAndTy
            Nothing -> do
              fieldName   <- CName <$> liftIO (clang_getCursorDisplayName current)
              ty          <- liftIO (clang_getCursorType current)
              case fromSimpleEnum $ cxtKind ty of
                Right CXType_IncompleteArray -> do
                  e <- liftIO $ clang_getArrayElementType ty
                  fieldType <- processTypeDeclRec (DeclPathCtxtField mStructName fieldName ctxt) extBindings unit Nothing e
                  return (fieldName, fieldType, True)
                _ -> do
                  fieldType <- processTypeDeclRec (DeclPathCtxtField mStructName fieldName ctxt) extBindings unit Nothing ty
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
        unrecognizedCursor current

isIncompleteArrayType :: Type -> Bool
isIncompleteArrayType (TypeIncompleteArray {}) = True
isIncompleteArrayType _ = False

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

mkUnionField
    :: ExtBindings
    -> CXTranslationUnit
    -> Maybe CName         -- ^ Name of the union (unless anonymous)
    -> DeclPathCtxt
    -> CXCursor
    -> Eff (State DeclState) (Maybe UnionField)
mkUnionField extBindings unit mUnionName ctxt current = do
    ufieldSourceLoc <- liftIO $
      HighLevel.clang_getExpansionLocation =<< clang_getCursorLocation current
    cursorKind <- liftIO $ clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_UnexposedAttr ->
        return Nothing

      Right CXCursor_FieldDecl -> do
        extent   <- liftIO $ HighLevel.clang_getCursorExtent current
        hasMacro <- gets $ containsMacroExpansion extent

        -- TODO: the macro code is untested.
        mbNameTypeWithMacros <-
          if hasMacro
          then do
            tokens <- liftIO $ HighLevel.clang_tokenize unit (multiLocExpansion <$> extent)
            macroTyEnv <- macroTypes <$> get
            case reparseWith (reparseFieldDecl macroTyEnv) tokens of
              Left err -> do
                -- TODO: improve mechanism for reporting warnings
                liftIO $ putStrLn $ unlines
                  [ "\nWarning: failed to re-parse struct field containing macro expansion."
                  , "Proceeding with macros expanded."
                  , ""
                  , "Parse error:"
                  , prettyLogMsg err
                  , ""
                  ]
                return Nothing
              Right (fieldType, fieldName) ->
                return $ Just (fieldName, fieldType)
          else
            return Nothing

        (ufieldName, ufieldType) <-
          case mbNameTypeWithMacros of
            Just declNameAndTy -> return declNameAndTy
            Nothing -> do
              fieldName   <- CName <$> liftIO (clang_getCursorDisplayName current)
              ty          <- liftIO (clang_getCursorType current)
              fieldType <- processTypeDeclRec (DeclPathCtxtField mUnionName fieldName ctxt) extBindings unit Nothing ty
              return (fieldName, fieldType)

        return $ Just $ UnionField{ufieldName, ufieldType, ufieldSourceLoc}

      -- TODO: inner definitions
      -- Right CXCursor_StructDecl -> return Nothing

      _other ->
        unrecognizedCursor current

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

mkEnumValue ::
     MonadIO m
  => CXCursor
  -> m (Maybe EnumValue)
mkEnumValue current = liftIO $ do
    valueSourceLoc <- liftIO $
      HighLevel.clang_getExpansionLocation =<< clang_getCursorLocation current
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
