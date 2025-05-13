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

import Clang.CNameSpelling
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import Data.DynGraph qualified as DynGraph
import HsBindgen.BindingSpecs
import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.DeclState
import HsBindgen.C.Reparse
import HsBindgen.Eff
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer (prettyLogMsg)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Process top-level (type) declration
processTypeDecl ::
     IBindingSpecs SourcePath -- ^ Binding specs for configuration
  -> IBindingSpecs SourcePath -- ^ External binding specs
  -> CXTranslationUnit
  -> DeclLoc
  -> Maybe CXCursor
  -> CXType
  -> Eff (State DeclState) Type
processTypeDecl specs extSpecs unit declLoc declCursor ty = do
    -- dtraceIO "processTypeDecl" ty
    s <- get
    case OMap.lookup ty (typeDeclarations s) of
        Nothing                        -> processTypeDecl' DeclPathCtxtTop specs extSpecs unit declLoc declCursor ty
        Just (TypeDecl t _)            -> return t
        Just (TypeDeclAlias t)         -> return t
        Just (TypeDeclProcessing t' _) -> panicIO $ "Incomplete type declaration: " ++ show t'
        Just (TypeDeclOmitted cname)   -> liftIO $ throwIO (OmittedTypeUse cname)

processTypeDeclRec ::
     DeclPathCtxt
  -> IBindingSpecs SourcePath -- ^ Binding specs for configuration
  -> IBindingSpecs SourcePath -- ^ External binding specs
  -> CXTranslationUnit
  -> DeclLoc
  -> Maybe CXCursor
  -> CXType
  -> Eff (State DeclState) Type
processTypeDeclRec ctxt specs extSpecs unit declLoc declCursor ty = do
    s <- get
    case OMap.lookup ty (typeDeclarations s) of
        Nothing                       -> processTypeDecl' ctxt specs extSpecs unit declLoc declCursor ty
        Just (TypeDecl t _)           -> return t
        Just (TypeDeclAlias t)        -> return t
        Just (TypeDeclProcessing t _) -> return t
        Just (TypeDeclOmitted cname)  -> liftIO $ throwIO (OmittedTypeUse cname)

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
  -> IBindingSpecs SourcePath -- ^ Binding specs for configuration
  -> IBindingSpecs SourcePath -- ^ External binding specs
  -> CXTranslationUnit
  -> DeclLoc
     -- ^ Location (for error messages)
  -> Maybe CXCursor
      -- ^ cursor of the type declaration; only used for reparsing
      -- function declarations containing macros
  -> CXType
  -> Eff (State DeclState) Type
processTypeDecl' ctxt specs extSpecs unit declLoc declCursor ty = case fromSimpleEnum $ cxtKind ty of
    kind | Just prim <- primType kind -> do
        return $ TypePrim prim

    -- elaborated types, we follow the definition.
    Right CXType_Elaborated -> do
        ty' <- clang_Type_getNamedType ty
        processTypeDeclRec ctxt specs extSpecs unit (RelatedTo declLoc Named) Nothing ty'

    -- typedefs
    Right CXType_Typedef -> do
        -- getTypedefName returns the same string as clang_getTypeSpelling
        name <- clang_getTypedefName ty
        let ctype = TypeTypedef $ CName name
        addTypeDeclProcessing ty ctype

        decl <- clang_getTypeDeclaration ty
        sloc <- HighLevel.clang_getExpansionLocation =<< clang_getCursorLocation decl
        extent   <- HighLevel.clang_getCursorExtent decl
        hasMacro <- gets $ containsMacroExpansion extent

        let cnameSpelling = CNameSpelling name
        (mExtType, mTypeSpec) <-
          lookupExtTypeAndTypeSpec cnameSpelling sloc specs extSpecs

        case mExtType of
            Just extType -> addAlias ty $ TypeExtBinding extType ctype
            Nothing -> case fmap typeSpecHaskell mTypeSpec of
              Just (Just Omit) -> addOmitted ty cnameSpelling
              _otherwise -> do
                tag <- CName <$> clang_getCursorSpelling decl
                mbTy <- if not hasMacro
                        then return Nothing
                        else do
                          tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> extent)
                          macroTyEnv <- macroTypeEnv <$> get
                          case reparseWith (reparseTypedef macroTyEnv) tokens of
                            Left err -> do
                              -- TODO: improve mechanism for reporting warnings
                              liftIO $ putStrLn $ unlines
                                [ "\nWarning: failed to re-parse typedef containing macro expansion."
                                , "Proceeding with macros expanded."
                                , ""
                                , "Parse error:"
                                , prettyLogMsg err
                                , ""
                                ]
                              return Nothing
                            Right ty1 ->
                              return $ Just ty1
                ty' <- getElaborated =<< clang_getTypedefDeclUnderlyingType decl
                use <-
                  case mbTy of
                    Just ty1 -> return ty1
                    Nothing  ->
                      processTypeDeclRec
                        (DeclPathCtxtTypedef tag)
                        specs
                        extSpecs
                        unit
                        (RelatedTo declLoc TypedefUnderlying)
                        Nothing
                        ty'

                -- we could check whether typedef has a transparent tag,
                -- like in case of `typedef struct foo {..} foo;`
                -- but we don't use that for anything.
                --
                -- transparent <- liftIO (clang_Type_isTransparentTagTypedef ty)

                case use of
                    -- If names match, skip.
                    -- Note: this is not the same as clang_Type_isTransparentTagTypedef,
                    -- in typedef struct { ... } foo; the typedef does not have transparent tag.
                    TypeStruct (DeclPathName declName) | declName == tag -> do
                            updateDeclAddAlias ty' tag
                            addAlias ty use
                    TypeStruct (DeclPathAnon (DeclPathCtxtTypedef typedefName)) | typedefName == tag ->
                            addAlias ty use

                    TypeEnum (DeclPathName declName) | declName == tag -> do
                            updateDeclAddAlias ty' tag
                            addAlias ty use
                    TypeEnum (DeclPathAnon (DeclPathCtxtTypedef typedefName)) | typedefName == tag ->
                            addAlias ty use

                    TypeUnion (DeclPathName declName) | declName == tag -> do
                            updateDeclAddAlias ty' tag
                            addAlias ty use
                    TypeUnion (DeclPathAnon (DeclPathCtxtTypedef typedefName)) | typedefName == tag ->
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
                            , typedefTypeSpec  = mTypeSpec
                            }

    -- structs and unions
    Right CXType_Record -> do
        decl <- clang_getTypeDeclaration ty
        ki   <- fromSimpleEnum <$> clang_getCursorKind decl
        sloc <- HighLevel.clang_getExpansionLocation =<< clang_getCursorLocation decl

        case ki of
            Right CXCursor_StructDecl -> do
                mFlavour <- classifyTypeDecl ctxt specs extSpecs (ty, decl, ki, sloc)
                case mFlavour of
                  Left (AnonTopLevel replacement) ->
                    return replacement
                  Right (declPath, flavour) -> do
                    let ctype = TypeStruct declPath
                    addTypeDeclProcessing ty ctype
                    case flavour of
                      TypeDeclOmitted' cname ->
                        addOmitted ty cname
                      TypeDeclExternal extType ->
                        addAlias ty $ TypeExtBinding extType ctype
                      TypeDeclOpaque name mTypeSpec -> do
                        addDecl ty $ DeclOpaqueStruct OpaqueStruct {
                            opaqueStructTag       = name
                          , opaqueStructAliases   = []
                          , opaqueStructSourceLoc = sloc
                          , opaqueStructTypeSpec  = mTypeSpec
                          }
                      TypeDeclRegular mTypeSpec -> do
                        sizeof    <- clang_Type_getSizeOf  ty
                        alignment <- clang_Type_getAlignOf ty
                        fields'   <- HighLevel.clang_visitChildren decl $ \cursor -> do
                            let mkCtxt fieldName = DeclPathCtxtField (declPathName declPath) fieldName ctxt
                            mfield <- mkStructField specs extSpecs unit mkCtxt cursor
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
                            , structTypeSpec  = mTypeSpec
                            }

            Right CXCursor_UnionDecl -> do
                mFlavour <- classifyTypeDecl ctxt specs extSpecs (ty, decl, ki, sloc)

                case mFlavour of
                  Left (AnonTopLevel replacement) ->
                    return replacement
                  Right (declPath, flavour) -> do
                    addTypeDeclProcessing ty $ TypeUnion declPath
                    case flavour of
                      TypeDeclOmitted' cname ->
                        addOmitted ty cname
                      TypeDeclExternal _extType ->
                        panicIO "external bindings for unions not implemented #537"
                      TypeDeclOpaque name mTypeSpec ->
                        -- opaque struct and opaque union look the same.
                        addDecl ty $ DeclOpaqueStruct OpaqueStruct {
                              opaqueStructTag       = name
                            , opaqueStructAliases   = []
                            , opaqueStructSourceLoc = sloc
                            , opaqueStructTypeSpec  = mTypeSpec
                            }
                      TypeDeclRegular mTypeSpec -> do
                        -- the below is TODO:
                        sizeof    <- clang_Type_getSizeOf  ty
                        alignment <- clang_Type_getAlignOf ty
                        fields    <- HighLevel.clang_visitChildren decl $ \cursor -> do
                            let mkCtxt fieldName = DeclPathCtxtField (declPathName declPath) fieldName ctxt
                            mfield <- mkUnionField specs extSpecs unit mkCtxt cursor
                            return $ Continue mfield

                        addDecl ty $ DeclUnion Union
                            { unionDeclPath  = declPath
                            , unionAliases   = []
                            , unionSizeof    = fromIntegral sizeof
                            , unionAlignment = fromIntegral alignment
                            , unionFields    = fields
                            , unionSourceLoc = sloc
                            , unionTypeSpec  = mTypeSpec
                            }

            _ -> panicIO $ show ki

    -- enum
    Right CXType_Enum -> do
        decl <- clang_getTypeDeclaration ty
        ki   <- fromSimpleEnum <$> clang_getCursorKind decl
        sloc <- HighLevel.clang_getExpansionLocation =<< clang_getCursorLocation decl

        case ki of
            Right CXCursor_EnumDecl -> do
                mFlavour <- classifyTypeDecl ctxt specs extSpecs (ty, decl, ki, sloc)
                case mFlavour of
                  Left (AnonTopLevel replacement) ->
                    return replacement
                  Right (declPath, flavour) -> do
                    let ctype = TypeEnum declPath
                    addTypeDeclProcessing ty ctype
                    case flavour of
                      TypeDeclOmitted' cname ->
                        addOmitted ty cname
                      TypeDeclExternal extType ->
                        addAlias ty $ TypeExtBinding extType ctype
                      TypeDeclOpaque name mTypeSpec ->
                        addDecl ty $ DeclOpaqueEnum OpaqueEnum {
                            opaqueEnumTag       = name
                          , opaqueEnumAliases   = []
                          , opaqueEnumSourceLoc = sloc
                          , opaqueEnumTypeSpec  = mTypeSpec
                          }
                      TypeDeclRegular mTypeSpec -> do
                        sizeof    <- clang_Type_getSizeOf  ty
                        alignment <- clang_Type_getAlignOf ty
                        ety       <- clang_getEnumDeclIntegerType decl
                          >>= processTypeDeclRec DeclPathCtxtTop specs extSpecs unit (RelatedTo declLoc EnumInteger) Nothing

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
                            , enumTypeSpec  = mTypeSpec
                            }

            _ -> panicIO $ show ki

    Right CXType_Pointer -> do
        pointee <- clang_getPointeeType ty
        pointee' <- processTypeDeclRec (DeclPathCtxtPtr ctxt) specs extSpecs unit (RelatedTo declLoc Pointee) Nothing pointee
        return (TypePointer pointee')

    Right CXType_ConstantArray -> do
        n <- fromIntegral <$> clang_getArraySize ty
        e <- clang_getArrayElementType ty
        -- TODO: This context should use 'DeclPathCtxtConstArray'
        e' <- processTypeDeclRec ctxt specs extSpecs unit (RelatedTo declLoc ArrayElement) Nothing e
        return (TypeConstArray (Size n $ litSizeExpression n) e')

    Right CXType_Void -> do
        return TypeVoid

    Right CXType_Bool -> do
        return $ TypePrim PrimBool

    Right CXType_FunctionProto -> processFun

    -- For a function declarator without a parameter type list: the effect is as
    -- if it were declared with a parameter type list consisting of the keyword
    -- void.
    Right CXType_FunctionNoProto -> processFun

    Right CXType_IncompleteArray -> do
        e <- clang_getArrayElementType ty
        -- TODO: Should this also use 'DeclPathCtxtConstArray'?
        e' <- processTypeDeclRec ctxt specs extSpecs unit (RelatedTo declLoc ArrayElement) Nothing e
        return (TypeIncompleteArray e')

    Right CXType_Attributed -> do
        ty' <- clang_Type_getModifiedType ty
        processTypeDeclRec ctxt specs extSpecs unit (RelatedTo declLoc Modified) Nothing ty'

    _otherwise ->
      unrecognizedType ty declLoc

  where
    processFun :: Eff (State DeclState) Type
    processFun = do
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
              protoExtent <- HighLevel.clang_getCursorExtent cx
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
              tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> protoExtent)
              macroTyEnv <- macroTypeEnv <$> get
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

            res <- clang_getResultType ty
            res' <- processTypeDeclRec ctxt specs extSpecs unit (RelatedTo declLoc Result) Nothing res
            nargs <- clang_getNumArgTypes ty
            args' <- forM [0 .. nargs - 1] $ \i -> do
              arg <- clang_getArgType ty (fromIntegral i)
              processTypeDeclRec ctxt specs extSpecs unit (RelatedTo declLoc Arg) Nothing arg

            -- There are no macros in the function, hence no macros in the
            -- function argument or return types either. This is why it's OK
            -- to pass 'Nothing' as a CXCursor above (it is only used for re-parsing).

            return $ TypeFun args' res'

getElaborated :: MonadIO m => CXType -> m CXType
getElaborated ty = case fromSimpleEnum (cxtKind ty) of
    Right CXType_Elaborated -> getElaborated =<< clang_Type_getNamedType ty
    _otherwise              -> return ty

lookupExtTypeAndTypeSpec ::
     CNameSpelling
  -> SingleLoc
  -> IBindingSpecs SourcePath -- ^ Binding specs for configuration
  -> IBindingSpecs SourcePath -- ^ External binding specs
  -> Eff (State DeclState) (Maybe ExtType, Maybe TypeSpec)
lookupExtTypeAndTypeSpec cname sloc specs extSpecs
    | isNothing mSpecPs && isNothing mExtSpecPs = return (Nothing, Nothing)
    | otherwise = do
        let path = singleLocPath sloc
        paths <- gets $ (`DynGraph.reaches` path) . cIncludePathGraph
        let mTypeSpec = lookupTypeSpec paths =<< mSpecPs
        fmap (, mTypeSpec) $ case lookupTypeSpec paths =<< mExtSpecPs of
          Nothing -> return Nothing
          Just typeSpec ->
            either (liftIO . throwIO . HsBindgenException) (return . Just) $
              getExtType cname typeSpec
  where
    mSpecPs, mExtSpecPs :: Maybe [(Set SourcePath, TypeSpec)]
    mSpecPs    = lookupBindingSpecsType cname specs
    mExtSpecPs = lookupBindingSpecsType cname extSpecs

addAlias :: CXType -> Type -> Eff (State DeclState) Type
addAlias ty t = do
    s <- get
    let ds = typeDeclarations s
    case OMap.lookup ty ds of
        Nothing -> panicIO "type not being processed"
        Just (TypeDeclProcessing _t _as) -> do
            put s { typeDeclarations = omapInsertBack ty (TypeDeclAlias t) ds }
            return t
        Just (TypeDeclAlias _) -> panicIO "type already processed"
        Just (TypeDecl _ _) -> panicIO "type already processed"
        Just (TypeDeclOmitted _) -> panicIO "type already processed"

addTypeDeclProcessing :: CXType -> Type -> Eff (State DeclState) ()
addTypeDeclProcessing ty t = do
    s <- get
    let ds = typeDeclarations s
    case OMap.lookup ty ds of
        Nothing -> put s { typeDeclarations = omapInsertBack ty (TypeDeclProcessing t []) ds }
        Just (TypeDeclProcessing t' _as) -> panicIO $ "type already processed (1)" ++ show (t, t')
        Just (TypeDecl t' _) -> panicIO $ "type already processed (2)" ++ show (t, t')
        Just (TypeDeclAlias t') -> panicIO $ "type already processed (3)" ++ show (t, t')
        Just (TypeDeclOmitted cname) -> panicIO $ "type already processed (4)" ++ show (t, cname)

addDecl :: CXType -> Decl -> Eff (State DeclState) Type
addDecl ty d = do
    s <- get
    let ds = typeDeclarations s
    case OMap.lookup ty ds of
        Nothing -> panicIO "type not being processed"
        Just (TypeDeclProcessing t aliases) -> do
            let err = "updateDeclAddAliases not implemented for type: " ++ show t
            d' <- maybe (panicIO err) return $
                    if null aliases
                        then Just d
                        else updateDeclAddAliases aliases d
            put s { typeDeclarations = omapInsertBack ty (TypeDecl t d') ds }
            return t
        Just (TypeDecl _ _) -> panicIO "type already processed"
        Just (TypeDeclAlias _) -> panicIO "type already processed"
        Just (TypeDeclOmitted _) -> panicIO "type already processed"

updateDeclAddAlias :: CXType -> CName -> Eff (State DeclState) ()
updateDeclAddAlias ty alias = do
    s <- get
    let ds = typeDeclarations s
    case OMap.lookup ty ds of
        Nothing -> panicIO "type not found"
        Just (TypeDeclProcessing typ aliases) ->
            let d = TypeDeclProcessing typ (alias : aliases)
            in  put s { typeDeclarations = (ty, d) OMap.<| ds }
        Just (TypeDecl typ decl) -> case updateDeclAddAliases [alias] decl of
            Just decl' ->
                put s { typeDeclarations = (ty, TypeDecl typ decl') OMap.<| ds }
            Nothing -> panicIO $
              "updateDeclAddAliases not implemented for type: " ++ show typ
        Just (TypeDeclAlias typ) -> panicIO $
          "cannot add alias to an alias: " ++ show typ
        Just (TypeDeclOmitted cname) -> liftIO $ throwIO (OmittedTypeUse cname)

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

addOmitted :: CXType -> CNameSpelling -> Eff (State DeclState) Type
addOmitted ty cname = do
    s <- get
    let ds = typeDeclarations s
    case OMap.lookup ty ds of
      Nothing -> panicIO "type not being processed"
      Just (TypeDeclProcessing t aliases) -> do
        unless (null aliases) $ liftIO $ throwIO (OmittedTypeUse cname)
        put s {
            typeDeclarations = omapInsertBack ty (TypeDeclOmitted cname) ds
          }
        return t
      Just (TypeDecl _ _) -> panicIO "type already processed"
      Just (TypeDeclAlias _) -> panicIO "type already processed"
      Just (TypeDeclOmitted _) -> panicIO "type already processed"

-- https://github.com/dmwit/ordered-containers/issues/29
omapInsertBack :: Ord k => k -> v -> OMap.OMap k v -> OMap.OMap k v
omapInsertBack k v m = m OMap.>| (k, v)

{-------------------------------------------------------------------------------
  Internal auxiliary: classify type declarations
-------------------------------------------------------------------------------}

data TypeDeclFlavour =
    -- | Type declaration that is omitted by binding specs
    TypeDeclOmitted' CNameSpelling

    -- | Type declaration for which we have external bindings
  | TypeDeclExternal ExtType

    -- | Opaque type declaration
  | TypeDeclOpaque CName (Maybe TypeSpec)

    -- | All other cases
  | TypeDeclRegular (Maybe TypeSpec)

-- | Anonymous top-level declaration
--
-- Since (even in C) these are unusable, we replace them by essentially an
-- arbitrary type.
--
-- TODO: Should we warn when we see these?
data AnonTopLevel = AnonTopLevel Type

-- | Classify type declaration
classifyTypeDecl ::
     DeclPathCtxt
  -> IBindingSpecs SourcePath -- ^ Binding specs for configuration
  -> IBindingSpecs SourcePath -- ^ External binding specs
  -> (CXType, CXCursor, Either CInt CXCursorKind, SingleLoc)
  -> Eff (State DeclState) (Either AnonTopLevel (DeclPath, TypeDeclFlavour))
classifyTypeDecl ctxt specs extSpecs (ty, decl, ki, sloc) = do
    anon <- clang_Cursor_isAnonymous decl

    if anon then
      case ctxt of
        DeclPathCtxtTop ->
          return $ Left $ AnonTopLevel TypeVoid
        _otherwise ->
          -- We assume anonymous type declarations are never opaque, and
          -- cannot have external bindings.
          --
          -- TODO <https://github.com/well-typed/hs-bindgen/issues/536>
          -- We might want to support external bindings for anonymous types.
          return $ Right (DeclPathAnon ctxt, TypeDeclRegular Nothing)
    else do
      spelling <- clang_getTypeSpelling ty
      let cnameSpelling = CNameSpelling spelling
      (mExtType, mTypeSpec) <-
        lookupExtTypeAndTypeSpec cnameSpelling sloc specs extSpecs

      let mTag     = CName <$> T.stripPrefix expectedPrefix spelling
          declPath = mkDeclPath spelling mTag

      fmap (Right . (declPath,)) $ case mExtType of
        Just extType -> return $ TypeDeclExternal extType
        Nothing -> case fmap typeSpecHaskell mTypeSpec of
          Just (Just Omit) -> return $ TypeDeclOmitted' cnameSpelling
          _otherwise -> do
            classified <- HighLevel.classifyDeclaration decl
            case classified of
              DeclarationOpaque ->
                case mTag of
                  Just tag -> return $ TypeDeclOpaque tag mTypeSpec
                  Nothing  -> panicIO "opaque definition without tag"
              DeclarationForward _defn ->
                panicIO "should not happen"
              DeclarationRegular ->
                return $ TypeDeclRegular mTypeSpec
  where
    expectedPrefix :: Text
    expectedPrefix =
        case ki of
          Right CXCursor_StructDecl -> "struct "
          Right CXCursor_UnionDecl  -> "union "
          Right CXCursor_EnumDecl   -> "enum "
          kind -> panicPure $ "classifyTypeDecl called on " ++ show kind

    mkDeclPath :: Text -> Maybe CName -> DeclPath
    mkDeclPath spelling mTag =
        case mTag of
          Just tag -> DeclPathName tag
          Nothing  -> DeclPathAnon (DeclPathCtxtTypedef (CName spelling))

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
     IBindingSpecs SourcePath -- ^ Binding specs for configuration
  -> IBindingSpecs SourcePath -- ^ External binding specs
  -> CXTranslationUnit
  -> (CName -> DeclPathCtxt) -- ^ Construct context given field name
  -> CXCursor
  -> Eff (State DeclState) (Maybe Field) -- ^ Left values are flexible array members.
mkStructField specs extSpecs unit mkCtxt current = do
    fieldSourceLoc <-
      HighLevel.clang_getExpansionLocation =<< clang_getCursorLocation current
    cursorKind <- clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_UnexposedAttr ->
        return Nothing

      Right CXCursor_FieldDecl -> do
        extent   <- HighLevel.clang_getCursorExtent current
        hasMacro <- gets $ containsMacroExpansion extent

        mbNameTypeWithMacros <-
          if hasMacro
          then do
            tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> extent)
            macroTyEnv <- macroTypeEnv <$> get
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
              fieldName <- CName <$> clang_getCursorDisplayName current
              ty        <- clang_getCursorType current
              case fromSimpleEnum $ cxtKind ty of
                Right CXType_IncompleteArray -> do
                  e <- clang_getArrayElementType ty
                  fieldType <- processTypeDeclRec (mkCtxt fieldName) specs extSpecs unit (RelatedTo (Precise fieldSourceLoc) ArrayElement) Nothing e
                  return (fieldName, fieldType, True)
                _ -> do
                  fieldType <- processTypeDeclRec (mkCtxt fieldName) specs extSpecs unit (Precise fieldSourceLoc) Nothing ty
                  return (fieldName, fieldType, False)

        fieldOffset <- fromIntegral <$> clang_Cursor_getOffsetOfField current

        if isIncompleteArray
        then do
          assertEff (fieldOffset `mod` 8 == 0) "offset should be divisible by 8"
          return $ Just $ IncompleteArray StructField{fieldName, fieldOffset, fieldType, fieldSourceLoc, fieldWidth = Nothing}
        else do
          isBitField <- clang_Cursor_isBitField current
          if isBitField
          then do
            width <- clang_getFieldDeclBitWidth current
            return $ Just $ Normal StructField{fieldName, fieldOffset, fieldType, fieldSourceLoc, fieldWidth = Just (fromIntegral width)}
          else do
            assertEff (fieldOffset `mod` 8 == 0) "offset should be divisible by 8"
            return $ Just $ Normal StructField{fieldName, fieldOffset, fieldType, fieldSourceLoc, fieldWidth = Nothing}

      -- nested type declarations, there are two approaches:
      -- * process eagerly
      -- * process when encountered in a field
      --
      -- For now we chose the latter.
      Right CXCursor_StructDecl -> return Nothing
      Right CXCursor_EnumDecl   -> return Nothing
      Right CXCursor_UnionDecl  -> return Nothing

      _other ->
        unrecognizedCursor current

isIncompleteArrayType :: Type -> Bool
isIncompleteArrayType (TypeIncompleteArray {}) = True
isIncompleteArrayType _ = False

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

mkUnionField ::
     IBindingSpecs SourcePath -- ^ Binding specs for configuration
  -> IBindingSpecs SourcePath -- ^ External binding specs
  -> CXTranslationUnit
  -> (CName -> DeclPathCtxt) -- ^ Construct context given field name
  -> CXCursor
  -> Eff (State DeclState) (Maybe UnionField)
mkUnionField specs extSpecs unit mkCtxt current = do
    ufieldSourceLoc <-
      HighLevel.clang_getExpansionLocation =<< clang_getCursorLocation current
    cursorKind <- clang_getCursorKind current
    case fromSimpleEnum cursorKind of
      Right CXCursor_UnexposedAttr ->
        return Nothing

      Right CXCursor_FieldDecl -> do
        extent   <- HighLevel.clang_getCursorExtent current
        hasMacro <- gets $ containsMacroExpansion extent

        -- TODO: the macro code is untested.
        mbNameTypeWithMacros <-
          if hasMacro
          then do
            tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> extent)
            macroTyEnv <- macroTypeEnv <$> get
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
              fieldName <- CName <$> clang_getCursorDisplayName current
              ty        <- clang_getCursorType current
              fieldType <- processTypeDeclRec (mkCtxt fieldName) specs extSpecs unit (Precise ufieldSourceLoc) Nothing ty
              return (fieldName, fieldType)

        return $ Just $ UnionField{ufieldName, ufieldType, ufieldSourceLoc}

      -- inner structs: skip, processed as part of fields if needed.
      Right CXCursor_StructDecl -> return Nothing

      _other ->
        unrecognizedCursor current

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

mkEnumValue ::
     MonadIO m
  => CXCursor
  -> m (Maybe EnumValue)
mkEnumValue current = do
    valueSourceLoc <-
      HighLevel.clang_getExpansionLocation =<< clang_getCursorLocation current
    cursorKind <- clang_getCursorKind current

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
      CXType_Char_S     -> Just $ PrimChar $ PrimSignImplicit (Just Signed)
      CXType_Char_U     -> Just $ PrimChar $ PrimSignImplicit (Just Unsigned)
      CXType_SChar      -> Just $ PrimChar $ PrimSignExplicit Signed
      CXType_UChar      -> Just $ PrimChar $ PrimSignExplicit Unsigned
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
