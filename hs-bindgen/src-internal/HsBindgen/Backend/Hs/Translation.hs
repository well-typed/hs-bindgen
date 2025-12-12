-- | Low-level translation of the C header to a Haskell module
module HsBindgen.Backend.Hs.Translation (
    generateDeclarations
  ) where

import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Type.Nat (SNatI)
import Data.Vec.Lazy qualified as Vec

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Config (HaddockConfig)
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Config
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as HsFI
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.Newtype qualified as Hs
import HsBindgen.Backend.Hs.Translation.State (TranslationState)
import HsBindgen.Backend.Hs.Translation.State qualified as State
import HsBindgen.Backend.Hs.Translation.ToFromFunPtr qualified as ToFromFunPtr
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Config.Internal
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.PrettyC qualified as PC

import DeBruijn (Add (..), Env (..), Idx (..), pattern I1, pattern I2, sizeEnv,
                 tabulateEnv, weaken, zipWithEnv)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

generateDeclarations ::
     TranslationConfig
  -> HaddockConfig
  -> BaseModuleName
  -> DeclIndex
  -> [C.Decl]
  -> ByCategory [Hs.Decl]
generateDeclarations opts config name declIndex =
    ByCategory . Map.map reverse .
      foldl' partitionBindingCategories  Map.empty .
      generateDeclarations' opts config name declIndex
  where
    partitionBindingCategories ::
      Map BindingCategory [a] -> WithCategory a  -> Map BindingCategory [a]
    partitionBindingCategories m (WithCategory cat decl) =
      Map.alter (addDecl decl) cat m

    addDecl :: a -> Maybe [a] -> Maybe [a]
    addDecl decl Nothing      = Just [decl]
    addDecl decl (Just decls) = Just $ decl : decls

-- | Internal. Top-level declaration with foreign import category.
data WithCategory a = WithCategory {
    _withCategoryCategory :: BindingCategory
  , _withCategoryDecl     :: a
  } deriving (Show)

generateDeclarations' ::
     TranslationConfig
  -> HaddockConfig
  -> BaseModuleName
  -> DeclIndex
  -> [C.Decl]
  -> [WithCategory Hs.Decl]
generateDeclarations' opts haddockConfig moduleName declIndex decs =
    flip State.evalState State.emptyTranslationState $ do
      let scannedFunctionPointerTypes = scanAllFunctionPointerTypes decs
          -- Generate ToFunPtr/FromFunPtr instances for nested callback types
          -- These go in the main module to avoid orphan instances
          --WithCategory c
          fFIStubsAndFunPtrInstances =
                   [ WithCategory BType d
                   | C.TypePointer (C.TypeFun args res) <- Set.toList scannedFunctionPointerTypes
                   , not (any hasUnsupportedType (res:args))
                   , any (isDefinedInCurrentModule declIndex) (res:args)
                   , d <- ToFromFunPtr.forFunction (args, res)
                   ]
      hsDecls <- concat <$> mapM (generateDecs opts haddockConfig moduleName) decs
      pure $ hsDecls ++ fFIStubsAndFunPtrInstances

-- | This function takes a list of all declarations and collects all function
-- pointer callback types, i.e. all function types that exist as either
-- arguments for other functions (nth-order functions), fields of structs,
-- unions and enums.
--
-- This recursively traverses all types to find deeply nested function pointers.
scanAllFunctionPointerTypes :: [C.Decl] -> Set C.Type
scanAllFunctionPointerTypes =
  foldMap (\(C.Decl _ kind _) ->
            case kind of
              C.DeclStruct C.Struct {..} ->
                foldMap (scanTypeForFunctionPointers . C.structFieldType) structFields
              C.DeclUnion C.Union {..}   ->
                foldMap (scanTypeForFunctionPointers . C.unionFieldType) unionFields
              C.DeclFunction fn ->
                foldMap scanTypeForFunctionPointers ((C.functionRes fn) : (snd <$> C.functionArgs fn))
              _ -> Set.empty
         )
  where
    -- | Recursively scan a type for all function pointers, including nested ones
    scanTypeForFunctionPointers :: C.Type -> Set C.Type
    scanTypeForFunctionPointers ty = case ty of
      fp@(C.TypePointer (C.TypeFun args res)) ->
           Set.singleton fp
        <> foldMap scanTypeForFunctionPointers (res : args)
      C.TypePointer t                  -> scanTypeForFunctionPointers t
      C.TypeIncompleteArray  t         -> scanTypeForFunctionPointers t
      C.TypeConstArray _ t             -> scanTypeForFunctionPointers t
      C.TypeBlock t                    -> scanTypeForFunctionPointers t
      C.TypeQualified _ t              -> scanTypeForFunctionPointers t
      C.TypeTypedef (C.TypedefRef _ t) -> scanTypeForFunctionPointers t
      _                                -> Set.empty

-- | Check if a type is defined in the current module
isDefinedInCurrentModule :: DeclIndex -> C.Type -> Bool
isDefinedInCurrentModule declIndex =
    any isInDeclIndex . C.typeDeclIds
  where
    isInDeclIndex :: C.DeclId MangleNames -> Bool
    isInDeclIndex declId =
        case declId.origDeclId of
          C.OrigDeclId orig ->
            isJust $ DeclIndex.lookup orig declIndex
          C.AuxForDecl _parent ->
            False

{-------------------------------------------------------------------------------
  Declarations
------------------------------------------------------------------------------}

-- TODO: Take DeclSpec into account
generateDecs ::
     State.MonadState TranslationState m
  => TranslationConfig
  -> HaddockConfig
  -> BaseModuleName
  -> C.Decl
  -> m [WithCategory Hs.Decl]
generateDecs opts haddockConfig moduleName (C.Decl info kind spec) =
    case kind of
      C.DeclStruct struct -> withCategoryM BType $
        reifyStructFields struct $ structDecs opts haddockConfig info struct spec
      C.DeclUnion union -> withCategoryM BType $
        unionDecs haddockConfig info union spec
      C.DeclEnum e -> withCategoryM BType $
        enumDecs opts haddockConfig info e spec
      C.DeclTypedef d -> withCategoryM BType $
        typedefDecs opts haddockConfig info d spec
      C.DeclOpaque cNameKind -> withCategoryM BType $
        opaqueDecs cNameKind haddockConfig info spec
      C.DeclFunction f ->
        let funDeclsWith safety =
              functionDecs safety opts haddockConfig moduleName info f spec
            funType  = (C.TypeFun (snd <$> C.functionArgs f) (C.functionRes f))
            -- Declare a function pointer. We can pass this 'FunPtr' to C
            -- functions that take a function pointer of the appropriate type.
            funPtrDecls = fst $
              addressStubDecs opts haddockConfig moduleName info funType spec
        in  pure $ withCategory BSafe   (funDeclsWith SHs.Safe)
                ++ withCategory BUnsafe (funDeclsWith SHs.Unsafe)
                ++ withCategory BFunPtr  funPtrDecls
      C.DeclMacro macro -> withCategoryM BType $
        macroDecs opts haddockConfig info macro spec
      C.DeclGlobal ty -> do
        transState <- State.get
        pure $ withCategory BGlobal $
          global opts haddockConfig moduleName transState info ty spec
    where
      withCategory :: BindingCategory -> [a] -> [WithCategory a]
      withCategory c = map (WithCategory c)

      withCategoryM :: Functor m => BindingCategory -> m [a] -> m [WithCategory a]
      withCategoryM c = fmap (withCategory c)


{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

reifyStructFields ::
     C.Struct
  -> (forall n. SNatI n => Vec n C.StructField -> a)
  -> a
reifyStructFields struct k = Vec.reifyList (C.structFields struct) k

-- | Generate declarations for given C struct
structDecs :: forall n m.
     (SNatI n, State.MonadState TranslationState m)
  => TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.Struct
  -> C.DeclSpec
  -> Vec n C.StructField
  -> m [Hs.Decl]
structDecs opts haddockConfig info struct spec fields = do
    (insts, decls) <- aux <$> State.gets State.instanceMap
    State.modifyInstanceMap' $ Map.insert structName insts
    pure decls
  where
    structName :: Hs.Name Hs.NsTypeConstr
    structName = C.unsafeDeclIdHaskellName info.declId

    structFields :: Vec n Hs.Field
    structFields = flip Vec.map fields $ \f -> Hs.Field {
        fieldName    = C.nameHs (C.fieldName (C.structFieldInfo f))
      , fieldType    = Type.topLevel (C.structFieldType f)
      , fieldOrigin  = Origin.StructField f
      , fieldComment = generateHaddocksWithFieldInfo haddockConfig info (C.structFieldInfo f)
      }

    candidateInsts :: Set Hs.TypeClass
    candidateInsts = Set.union (Set.singleton Hs.Storable) $
      Set.fromList (snd <$> translationDeriveStruct opts)

    -- everything in aux is state-dependent
    aux :: Hs.InstanceMap -> (Set Hs.TypeClass, [Hs.Decl])
    aux instanceMap = (insts,) $
        structDecl : storableDecl ++ optDecls ++ hasFlamDecl ++
        concatMap (structFieldDecls structName) (C.structFields struct)
        -- TODO: generate zero-copy bindings for the FLAM field. See issue
        -- #1286.
      where
        insts :: Set Hs.TypeClass
        insts = Hs.getInstances instanceMap (Just structName) candidateInsts $
          Hs.fieldType <$> Vec.toList structFields

        hsStruct :: Hs.Struct n
        hsStruct = Hs.Struct {
            structName      = structName
          , structConstr    = C.recordConstr (C.structNames struct)
          , structFields    = structFields
          , structInstances = insts
          , structOrigin    = Just Origin.Decl{
                declInfo = info
              , declKind = Origin.Struct struct
              , declSpec = spec
              }
          , structComment = generateHaddocksWithInfo haddockConfig info
          }

        structDecl :: Hs.Decl
        structDecl = Hs.DeclData hsStruct

        storableDecl :: [Hs.Decl]
        storableDecl
          | Hs.Storable `Set.notMember` insts = []
          | otherwise = singleton
                      $ Hs.DeclDefineInstance
                          Hs.DefineInstance {
                            defineInstanceComment      = Nothing
                          , defineInstanceDeclarations =
                              Hs.InstanceStorable
                                  hsStruct
                                  Hs.StorableInstance {
                                    Hs.storableSizeOf    = C.structSizeof struct
                                  , Hs.storableAlignment = C.structAlignment struct
                                  , Hs.storablePeek = Hs.Lambda "ptr" $
                                      Hs.Ap (Hs.StructCon hsStruct) $
                                        map (peekStructField IZ) (C.structFields struct)
                                  , Hs.storablePoke      = Hs.Lambda "ptr" $ Hs.Lambda "s" $
                                      Hs.makeElimStruct IZ hsStruct $ \wk xs -> Hs.Seq $ toList $
                                        Vec.zipWith (pokeStructField (weaken wk I1)) fields xs
                                  }
                          }

        optDecls :: [Hs.Decl]
        optDecls = [
            Hs.DeclDeriveInstance
              Hs.DeriveInstance {
                deriveInstanceStrategy = strat
              , deriveInstanceClass    = clss
              , deriveInstanceName     = structName
              , deriveInstanceComment  = Nothing
              }
          | (strat, clss) <- translationDeriveStruct opts
          , clss `Set.member` insts
          ]

        hasFlamDecl :: [Hs.Decl]
        hasFlamDecl = case C.structFlam struct of
          Nothing   -> []
          Just flam -> singleton
                     $ Hs.DeclDefineInstance
                        Hs.DefineInstance {
                          defineInstanceComment      = Nothing
                        , defineInstanceDeclarations =
                            Hs.InstanceHasFLAM hsStruct
                                               (Type.topLevel (C.structFieldType flam))
                                               (C.structFieldOffset flam `div` 8)
                        }

-- | 'HasCField', 'HasCBitfield', and 'HasField' instances for a field of a
-- struct declaration
--
-- Given a struct:
--
-- > struct myStruct { int x; char y };
--
-- We generate roughly this datatype:
--
-- > newtype MyStruct = MyStruct { myStruct_x :: CInt, myStruct_y :: CChar }
--
-- Then, 'structFieldDecls' will generate roughly the following class instances
-- for the fields @x@ and @y@ respectively:
--
-- > instance HasCField "myStruct_x" MyStruct where
-- >   type CFieldType "myStruct_x" MyStruct = CInt
-- > instance HasField "myStruct_x" (Ptr MyStruct) (Ptr CInt)
--
-- > instance HasCField "myStruct_y" MyStruct where
-- >   type CFieldType "myStruct_y" MyStruct = CChar
-- > instance HasField "myStruct_y" (Ptr MyStruct) (Ptr CChar)
--
-- This works similarly for bit-fields, but those get a 'HasCBitfield' instance
-- instead of a 'HasCField' instance.
structFieldDecls :: Hs.Name Hs.NsTypeConstr -> C.StructField -> [Hs.Decl]
structFieldDecls structName f = [
      Hs.DeclDefineInstance $
        Hs.DefineInstance {
            defineInstanceComment      = Nothing
          , defineInstanceDeclarations =
              case C.structFieldWidth f of
                Nothing -> Hs.InstanceHasCField $ hasCFieldDecl
                Just w  -> Hs.InstanceHasCBitfield $ hasCBitfieldDecl w
          }
    , Hs.DeclDefineInstance $
        Hs.DefineInstance {
            defineInstanceComment      = Nothing
          , defineInstanceDeclarations = Hs.InstanceHasField hasFieldDecl
          }
    ]
  where
    parentType :: HsType
    parentType = Hs.HsTypRef structName

    fieldName :: Hs.Name Hs.NsVar
    fieldName = C.nameHs (C.fieldName (C.structFieldInfo f))

    fieldType :: HsType
    fieldType = Type.topLevel (C.structFieldType f)

    hasFieldDecl :: Hs.HasFieldInstance
    hasFieldDecl = Hs.HasFieldInstance {
          hasFieldInstanceParentType = parentType
        , hasFieldInstanceFieldName = fieldName
        , hasFieldInstanceFieldType = fieldType
        , hasFieldInstanceVia = case C.structFieldWidth f of
            Nothing -> Hs.ViaHasCField
            Just _  -> Hs.ViaHasCBitfield
        }

    hasCFieldDecl :: Hs.HasCFieldInstance
    hasCFieldDecl = Hs.HasCFieldInstance {
          hasCFieldInstanceParentType = parentType
        , hasCFieldInstanceFieldName  = fieldName
        , hasCFieldInstanceCFieldType = fieldType
        , hasCFieldInstanceFieldOffset = C.structFieldOffset f `div` 8
        }

    hasCBitfieldDecl :: Int -> Hs.HasCBitfieldInstance
    hasCBitfieldDecl w = Hs.HasCBitfieldInstance {
          hasCBitfieldInstanceParentType = parentType
        , hasCBitfieldInstanceFieldName  = fieldName
        , hasCBitfieldInstanceCBitfieldType = fieldType
        , hasCBitfieldInstanceBitOffset = C.structFieldOffset f
        , hasCBitfieldInstanceBitWidth = w
        }

peekStructField :: Idx ctx -> C.StructField -> Hs.PeekCField ctx
peekStructField ptr f = case C.structFieldWidth f of
    Nothing -> Hs.PeekCField (HsStrLit name) ptr
    Just _w -> Hs.PeekCBitfield (HsStrLit name) ptr
  where
    name = T.unpack $ Hs.getName $ C.nameHs (C.fieldName (C.structFieldInfo f))

pokeStructField :: Idx ctx -> C.StructField -> Idx ctx -> Hs.PokeCField ctx
pokeStructField ptr f x = case C.structFieldWidth f of
    Nothing -> Hs.PokeCField (HsStrLit name) ptr x
    Just _w  -> Hs.PokeCBitfield (HsStrLit name) ptr x
  where
    name = T.unpack $ Hs.getName $ C.nameHs (C.fieldName (C.structFieldInfo f))

{-------------------------------------------------------------------------------
  Opaque struct and opaque enum
-------------------------------------------------------------------------------}

opaqueDecs ::
     State.MonadState TranslationState m
  => C.NameKind
  -> HaddockConfig
  -> C.DeclInfo
  -> C.DeclSpec
  -> m [Hs.Decl]
opaqueDecs cNameKind haddockConfig info spec = do
    State.modifyInstanceMap' $ Map.insert name Set.empty
    return [decl]
  where
    name :: Hs.Name Hs.NsTypeConstr
    name = C.unsafeDeclIdHaskellName info.declId

    decl :: Hs.Decl
    decl = Hs.DeclEmpty Hs.EmptyData {
        emptyDataName   = name
      , emptyDataOrigin = Origin.Decl{
            declInfo = info
          , declKind = Origin.Opaque cNameKind
          , declSpec = spec
          }
      , emptyDataComment = generateHaddocksWithInfo haddockConfig info
      }

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

unionDecs ::
     forall m. State.MonadState TranslationState m
  => HaddockConfig
  -> C.DeclInfo
  -> C.Union
  -> C.DeclSpec
  -> m [Hs.Decl]
unionDecs haddockConfig info union spec = do
    nt <- newtypeDec
    flip aux nt <$> State.get
  where
    newtypeDec :: m Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = C.unsafeDeclIdHaskellName info.declId

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = C.newtypeConstr (C.unionNames union)

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
              fieldName    = C.newtypeField (C.unionNames union)
            , fieldType    = Hs.HsByteArray
            , fieldOrigin  = Origin.GeneratedField
            , fieldComment = Nothing
            }

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin =  Origin.Decl {
              declInfo = info
            , declKind = Origin.Union union
            , declSpec = spec
            }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment = generateHaddocksWithInfo haddockConfig info

        candidateInsts :: Set Hs.TypeClass
        candidateInsts = Set.empty

        knownInsts :: Set Hs.TypeClass
        knownInsts = Set.singleton Hs.Storable

    -- everything in aux is state-dependent
    aux :: TranslationState -> Hs.Newtype -> [Hs.Decl]
    aux transState nt =
        Hs.DeclNewtype nt : storableDecl : accessorDecls ++
        concatMap (unionFieldDecls nt.newtypeName) (C.unionFields union)
      where
        storableDecl :: Hs.Decl
        storableDecl =
          Hs.DeclDeriveInstance
            Hs.DeriveInstance {
              deriveInstanceStrategy = Hs.DeriveVia sba
            , deriveInstanceClass    = Hs.Storable
            , deriveInstanceName     = nt.newtypeName
            , deriveInstanceComment  = Nothing
            }

        sba :: Hs.HsType
        sba =
          HsSizedByteArray
            (fromIntegral (C.unionSizeof union))
            (fromIntegral (C.unionAlignment union))

        accessorDecls :: [Hs.Decl]
        accessorDecls = concatMap getAccessorDecls (C.unionFields union)

        -- TODO: Should the name mangler take care of the "get" and "set" prefixes?
        getAccessorDecls :: C.UnionField -> [Hs.Decl]
        getAccessorDecls C.UnionField{..} =
          let hsType = Type.topLevel unionFieldType
              fInsts = Hs.getInstances
                          (State.instanceMap transState) (Just nt.newtypeName)
                          (Set.singleton Hs.Storable) [hsType]
              getterName = "get_" <> C.nameHs (C.fieldName unionFieldInfo)
              setterName = "set_" <> C.nameHs (C.fieldName unionFieldInfo)
              commentRefName name = Just $ HsDoc.paragraph [
                  HsDoc.Bold [HsDoc.TextContent "See:"]
                , HsDoc.Identifier name
                ]
          in  if Hs.Storable `Set.notMember` fInsts
                then []
                else
                  [ Hs.DeclUnionGetter
                      Hs.UnionGetter {
                        unionGetterName    = getterName
                      , unionGetterType    = hsType
                      , unionGetterConstr  = nt.newtypeName
                      , unionGetterComment = generateHaddocksWithFieldInfo haddockConfig info unionFieldInfo
                                          <> commentRefName (Hs.getName setterName)
                      }
                  , Hs.DeclUnionSetter
                      Hs.UnionSetter {
                        unionSetterName    = setterName
                      , unionSetterType    = hsType
                      , unionSetterConstr  = nt.newtypeName
                      , unionSetterComment = commentRefName (Hs.getName getterName)
                      }
                  ]

-- | 'HasCField' and 'HasField' instances for a field of a
-- union declaration
--
-- Given a union:
--
-- > union myUnion { int option1; char option2 };
--
-- We generate roughly this newtype:
--
-- > newtype MyUnion = MyUnion { un_MyUnion :: ByteArray }
--
-- Then, 'unionFieldDecls' will generate roughly the following class instances
-- for the fields @option1@ and @option@ respectively:
--
-- > instance HasCField "myUnion_option1" MyUnion where
-- >   type CFieldType "myUnion_option1" MyUnion = CInt
-- > instance HasField "myUnion_option1" (Ptr MyUnion) (Ptr CInt)
--
-- > instance HasCField "myUnion_option2" MyUnion where
-- >   type CFieldType "myUnion_option2" MyUnion = CChar
-- > instance HasField "myUnion_option2" (Ptr MyUnion) (Ptr CChar)
--
-- This works similarly for bit-fields, but those get a 'HasCBitfield' instance
-- instead of a 'HasCField' instance.
unionFieldDecls :: Hs.Name Hs.NsTypeConstr -> C.UnionField -> [Hs.Decl]
unionFieldDecls unionName f = [
      Hs.DeclDefineInstance $
        Hs.DefineInstance {
            defineInstanceComment      = Nothing
          , defineInstanceDeclarations =
              case unionFieldWidth f of
                Nothing -> Hs.InstanceHasCField $ hasCFieldDecl
                Just w  -> Hs.InstanceHasCBitfield $ hasCBitfieldDecl w
          }
    , Hs.DeclDefineInstance $
        Hs.DefineInstance {
            defineInstanceComment      = Nothing
          , defineInstanceDeclarations = Hs.InstanceHasField hasFieldDecl
          }
    ]
  where
    -- | TODO: should be changed to @C.unionFieldWidth f@ when
    -- bit-fields in unions are supported. See issue #1253.
    unionFieldWidth :: C.UnionField -> Maybe Int
    unionFieldWidth _f = Nothing

    parentType :: HsType
    parentType = Hs.HsTypRef unionName

    fieldName :: Hs.Name Hs.NsVar
    fieldName = C.nameHs (C.fieldName (C.unionFieldInfo f))

    fieldType :: HsType
    fieldType = Type.topLevel (C.unionFieldType f)

    hasFieldDecl :: Hs.HasFieldInstance
    hasFieldDecl = Hs.HasFieldInstance {
          hasFieldInstanceParentType = parentType
        , hasFieldInstanceFieldName = fieldName
        , hasFieldInstanceFieldType = fieldType
        , hasFieldInstanceVia = case unionFieldWidth f of
            Nothing -> Hs.ViaHasCField
            Just _  -> Hs.ViaHasCBitfield
        }

    hasCFieldDecl :: Hs.HasCFieldInstance
    hasCFieldDecl = Hs.HasCFieldInstance {
          hasCFieldInstanceParentType = parentType
        , hasCFieldInstanceFieldName  = fieldName
        , hasCFieldInstanceCFieldType = fieldType
        , hasCFieldInstanceFieldOffset = 0
        }

    hasCBitfieldDecl :: Int -> Hs.HasCBitfieldInstance
    hasCBitfieldDecl w = Hs.HasCBitfieldInstance {
          hasCBitfieldInstanceParentType = parentType
        , hasCBitfieldInstanceFieldName  = fieldName
        , hasCBitfieldInstanceCBitfieldType = fieldType
        , hasCBitfieldInstanceBitOffset = 0
        , hasCBitfieldInstanceBitWidth = w
        }

{-------------------------------------------------------------------------------
  Enum
-------------------------------------------------------------------------------}

enumDecs ::
     forall m. State.MonadState TranslationState m
  => TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.Enum
  -> C.DeclSpec
  -> m [Hs.Decl]
enumDecs opts haddockConfig info e spec = do
    nt <- newtypeDec
    pure $ aux nt
  where
    newtypeDec :: m Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = C.unsafeDeclIdHaskellName info.declId

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = C.newtypeConstr (C.enumNames e)

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
            fieldName    = C.newtypeField (C.enumNames e)
          , fieldType    = Type.topLevel (C.enumType e)
          , fieldOrigin  = Origin.GeneratedField
          , fieldComment = Nothing
          }

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin = Origin.Decl{
              declInfo = info
            , declKind = Origin.Enum e
            , declSpec = spec
            }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment = generateHaddocksWithInfo haddockConfig info

        candidateInsts :: Set Hs.TypeClass
        candidateInsts = Set.empty

        knownInsts :: Set Hs.TypeClass
        knownInsts = Set.union (Set.fromList [Hs.Show, Hs.Read, Hs.Storable, Hs.HasBaseForeignType]) $
          Set.fromList (snd <$> translationDeriveEnum opts)

    -- everything in aux is state-dependent
    aux :: Hs.Newtype -> [Hs.Decl]
    aux nt =
        Hs.DeclNewtype nt : storableDecl : HsFI.hasBaseForeignTypeDecs nt ++
        optDecls ++ cEnumInstanceDecls ++ valueDecls
      where
        hsStruct :: Hs.Struct (S Z)
        hsStruct = Hs.Struct {
            structName      = nt.newtypeName
          , structConstr    = nt.newtypeConstr
          , structFields    = Vec.singleton nt.newtypeField
          , structInstances = nt.newtypeInstances
          , structOrigin    = Nothing
          , structComment   = Nothing
          }

        storableDecl :: Hs.Decl
        storableDecl = Hs.DeclDefineInstance
          Hs.DefineInstance {
            defineInstanceComment      = Nothing
          , defineInstanceDeclarations =
              Hs.InstanceStorable hsStruct Hs.StorableInstance {
                  Hs.storableSizeOf    = C.enumSizeof e
                , Hs.storableAlignment = C.enumAlignment e
                , Hs.storablePeek      = Hs.Lambda "ptr" $
                    Hs.Ap (Hs.StructCon hsStruct) [ Hs.PeekByteOff IZ 0 ]
                , Hs.storablePoke      = Hs.Lambda "ptr" $ Hs.Lambda "s" $
                    Hs.ElimStruct IZ hsStruct (AS AZ) $
                      Hs.Seq [ Hs.PokeByteOff I2 0 IZ ]
                }
          }

        optDecls :: [Hs.Decl]
        optDecls = [
            Hs.DeclDeriveInstance
              Hs.DeriveInstance {
                deriveInstanceName     = nt.newtypeName
              , deriveInstanceClass    = clss
              , deriveInstanceStrategy = strat
              , deriveInstanceComment  = Nothing
              }
          | (strat, clss) <- translationDeriveEnum opts
          ]

        valueDecls :: [Hs.Decl]
        valueDecls =
            [ Hs.DeclPatSyn Hs.PatSyn
              { patSynName    = C.nameHs (C.fieldName enumConstantInfo)
              , patSynType    = nt.newtypeName
              , patSynConstr  = nt.newtypeConstr
              , patSynValue   = enumConstantValue
              , patSynOrigin  = Origin.EnumConstant enumValue
              , patSynComment = generateHaddocksWithFieldInfo haddockConfig info enumConstantInfo
              }
            | enumValue@C.EnumConstant{..} <- C.enumConstants e
            ]

        cEnumInstanceDecls :: [Hs.Decl]
        cEnumInstanceDecls =
          let vNames = Map.fromListWith (flip (<>)) [ -- preserve source order
                  ( Hs.patSynValue pat
                  , NonEmpty.singleton (Hs.patSynName pat)
                  )
                | Hs.DeclPatSyn pat <- valueDecls
                ]
              mSeqBounds = do
                (minV, minNames) <- Map.lookupMin vNames
                (maxV, maxNames) <- Map.lookupMax vNames
                guard $ maxV - minV + 1 == fromIntegral (Map.size vNames)
                return (NonEmpty.head minNames, NonEmpty.head maxNames)
              fTyp = Hs.fieldType nt.newtypeField
              vStrs = fmap (T.unpack . Hs.getName) <$> vNames
              cEnumDecl = Hs.DeclDefineInstance
                Hs.DefineInstance {
                  defineInstanceComment      = Nothing
                , defineInstanceDeclarations = Hs.InstanceCEnum hsStruct fTyp vStrs (isJust mSeqBounds)
                }
              cEnumShowDecl = Hs.DeclDefineInstance
                Hs.DefineInstance {
                  defineInstanceComment      = Nothing
                , defineInstanceDeclarations = Hs.InstanceCEnumShow hsStruct
                }
              cEnumReadDecl = Hs.DeclDefineInstance
                Hs.DefineInstance {
                  defineInstanceComment      = Nothing
                , defineInstanceDeclarations = Hs.InstanceCEnumRead hsStruct
                }
              sequentialCEnumDecl = case mSeqBounds of
                Just (nameMin, nameMax) -> List.singleton
                                        . Hs.DeclDefineInstance
                                        $ Hs.DefineInstance {
                                            defineInstanceComment      = Nothing
                                          , defineInstanceDeclarations =
                                              Hs.InstanceSequentialCEnum hsStruct nameMin nameMax
                                          }
                Nothing -> []
          in  cEnumDecl : sequentialCEnumDecl ++ [cEnumShowDecl, cEnumReadDecl]

{-------------------------------------------------------------------------------
  Typedef
-------------------------------------------------------------------------------}

typedefDecs ::
     forall m. State.MonadState TranslationState m
  => TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.Typedef
  -> C.DeclSpec
  -> m [Hs.Decl]
typedefDecs opts haddockConfig info typedef spec = do
    nt <- newtypeDec
    pure $ aux nt
  where
    newtypeDec :: m Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = C.unsafeDeclIdHaskellName info.declId

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = C.newtypeConstr (C.typedefNames typedef)

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
            fieldName    = C.newtypeField (C.typedefNames typedef)
          , fieldType    = Type.topLevel (C.typedefType typedef)
          , fieldOrigin  = Origin.GeneratedField
          , fieldComment = Nothing
          }

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin =  Origin.Decl{
            declInfo = info
          , declKind = Origin.Typedef typedef
          , declSpec = spec
          }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment =  generateHaddocksWithInfo haddockConfig info

        candidateInsts :: Set Hs.TypeClass
        candidateInsts = Set.unions
                      [ Set.singleton Hs.Storable
                      , Set.singleton Hs.HasBaseForeignType
                      , Set.fromList (snd <$> translationDeriveTypedef opts)
                      ]

        knownInsts :: Set Hs.TypeClass
        knownInsts = Set.empty

    -- everything in aux is state-dependent
    aux :: Hs.Newtype -> [Hs.Decl]
    aux nt =
        Hs.DeclNewtype nt : newtypeWrapper ++ storableDecl ++ optDecls ++
        typedefFieldDecls nt ++
        HsFI.hasBaseForeignTypeDecs nt
      where
        insts = nt.newtypeInstances

        storableDecl :: [Hs.Decl]
        storableDecl
          | Hs.Storable `Set.notMember` insts = []
          | otherwise = singleton $
              Hs.DeclDeriveInstance
                Hs.DeriveInstance {
                  deriveInstanceStrategy = Hs.DeriveNewtype
                , deriveInstanceClass    = Hs.Storable
                , deriveInstanceName     = nt.newtypeName
                , deriveInstanceComment  = Nothing
                }

        optDecls :: [Hs.Decl]
        optDecls = [
            Hs.DeclDeriveInstance
              Hs.DeriveInstance {
                deriveInstanceStrategy = strat
              , deriveInstanceClass    = clss
              , deriveInstanceName     = nt.newtypeName
              , deriveInstanceComment  = Nothing
              }
          | (strat, clss) <- translationDeriveTypedef opts
          , clss `Set.member` insts
          ]

        newtypeWrapper :: [Hs.Decl]
        newtypeWrapper  =
          case C.typedefType typedef of
            -- We need to be careful and not generate any wrappers for function
            -- types that receive data types not supported by Haskell's FFI
            -- (i.e. structs, unions by value).
            --
            -- Note that we don't want to explicitly see all the way through
            -- typedefs here. See the following example
            --
            -- @
            -- typedef void (f)(int);
            -- typedef f g;
            -- @
            --
            -- If we see all the way through the typedef this case will not be
            -- handled correctly.
            --
            C.TypeFun args res | not (any hasUnsupportedType (res:args)) ->
              ToFromFunPtr.forNewtype nt.newtypeName (args, res)
            _ -> []

-- | 'HasCField', 'HasCBitfield', and 'HasField' instances for a typedef
-- declaration.
--
-- Given a typedef:
--
-- > typedef int myInt;
--
-- We generate roughly this newtype:
--
-- > newtype MyInt = MyInt { un_MyInt :: CInt }
--
-- Then, 'typedefFieldDecls' will generate roughly the following class
-- instances.
--
-- > instance HasCField "un_MyInt" MyInt where
-- >   type CFieldType "un_MyInt" MyInt = CInt
-- > instance HasField "un_MyInt" (Ptr MyInt) (Ptr CInt)
--
-- These instance help eliminating newtypes from 'Ptr' types. Naturally,
-- newtypes can also be introduced in 'Ptr' types, but this should be done using
-- 'castPtr' or some similar function.
typedefFieldDecls :: Hs.Newtype -> [Hs.Decl]
typedefFieldDecls hsNewType = [
    -- * Eliminate newtypes

      Hs.DeclDefineInstance $
        Hs.DefineInstance {
            defineInstanceComment      = Nothing
          , defineInstanceDeclarations = Hs.InstanceHasField elimHasFieldDecl
          }
    , Hs.DeclDefineInstance $
        Hs.DefineInstance {
            defineInstanceComment      = Nothing
          , defineInstanceDeclarations = Hs.InstanceHasCField $ elimHasCFieldDecl
          }
    ]
  where
    field :: Hs.Field
    field = Hs.newtypeField hsNewType

    parentType :: HsType
    parentType = Hs.HsTypRef (Hs.newtypeName hsNewType)

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.fieldName field

    fieldType :: HsType
    fieldType = Hs.fieldType field

    elimHasFieldDecl :: Hs.HasFieldInstance
    elimHasFieldDecl = Hs.HasFieldInstance {
          hasFieldInstanceParentType = parentType
        , hasFieldInstanceFieldName = fieldName
        , hasFieldInstanceFieldType = fieldType
        , hasFieldInstanceVia = Hs.ViaHasCField
        }

    elimHasCFieldDecl :: Hs.HasCFieldInstance
    elimHasCFieldDecl = Hs.HasCFieldInstance {
          hasCFieldInstanceParentType = parentType
        , hasCFieldInstanceFieldName  = fieldName
        , hasCFieldInstanceCFieldType = fieldType
        , hasCFieldInstanceFieldOffset = 0
        }

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

macroDecs ::
     State.MonadState TranslationState m
  => TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.CheckedMacro
  -> C.DeclSpec
  -> m [Hs.Decl]
macroDecs opts haddockConfig info checkedMacro spec =
    case checkedMacro of
      C.MacroType ty   -> macroDecsTypedef opts haddockConfig info ty spec
      C.MacroExpr expr -> pure $ macroVarDecs haddockConfig info expr

macroDecsTypedef ::
     forall m. State.MonadState TranslationState m
  => TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.CheckedMacroType
  -> C.DeclSpec
  -> m [Hs.Decl]
macroDecsTypedef opts haddockConfig info macroType spec = do
    nt <- newtypeDec
    pure $ aux nt
  where
    newtypeDec :: m Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = C.unsafeDeclIdHaskellName info.declId

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = C.newtypeConstr (C.macroTypeNames macroType)

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
              fieldName    = C.newtypeField (C.macroTypeNames macroType)
            , fieldType    = Type.topLevel (C.macroType macroType)
            , fieldOrigin  = Origin.GeneratedField
            , fieldComment = Nothing
            }

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin = Origin.Decl {
              declInfo = info
            , declKind = Origin.Macro macroType
            , declSpec = spec
            }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment = generateHaddocksWithInfo haddockConfig info

        candidateInsts :: Set Hs.TypeClass
        candidateInsts = Set.unions [
            Set.singleton Hs.Storable
          , Set.singleton Hs.HasBaseForeignType
          , Set.fromList (snd <$> translationDeriveTypedef opts)
          ]

        knownInsts :: Set Hs.TypeClass
        knownInsts = Set.empty

    -- everything in aux is state-dependent
    aux :: Hs.Newtype -> [Hs.Decl]
    aux nt =
        Hs.DeclNewtype nt : storableDecl ++ HsFI.hasBaseForeignTypeDecs nt ++ optDecls
      where
        insts :: Set Hs.TypeClass
        insts = nt.newtypeInstances

        storableDecl :: [Hs.Decl]
        storableDecl
          | Hs.Storable `Set.notMember` insts = []
          | otherwise = singleton $
              Hs.DeclDeriveInstance
                Hs.DeriveInstance {
                  deriveInstanceStrategy = Hs.DeriveNewtype
                , deriveInstanceClass    = Hs.Storable
                , deriveInstanceName     = nt.newtypeName
                , deriveInstanceComment  = Nothing
                }

        optDecls :: [Hs.Decl]
        optDecls = [
            Hs.DeclDeriveInstance
              Hs.DeriveInstance {
                deriveInstanceStrategy = strat
              , deriveInstanceClass    = clss
              , deriveInstanceName     = nt.newtypeName
              , deriveInstanceComment  = Nothing
              }
          | (strat, clss) <- translationDeriveTypedef opts
          , clss `Set.member` insts
          ]

{-------------------------------------------------------------------------------
  Function
-------------------------------------------------------------------------------}

data WrappedType
    = WrapType C.Type -- ^ ordinary, "primitive" types which can be handled by Haskell FFI directly
    | HeapType C.Type -- ^ types passed on heap
      -- | An array of known size, with const-qualified array elements
      --
      -- Only if the array elements are const qualified do we know for sure that
      -- the array is read-only. In such cases, we generate a high-level
      -- wrapper.
    | CAType C.Type Natural C.Type
      -- | An array of unknown size, with const-qualified array elements
      --
      -- Only if the array elements are const qualified do we know for sure that
      -- the array is read-only. In such cases, we generate a high-level
      -- wrapper.
    | AType C.Type C.Type
  deriving Show

-- | Checks if a type is unsupported by Haskell's FFI
--
hasUnsupportedType :: C.GetCanonicalType t => t -> Bool
hasUnsupportedType = aux . C.getCanonicalType
  where
    aux :: C.CanonicalType -> Bool
    aux (C.TypeRef declId)       = auxRef declId
    aux C.TypeComplex {}         = True
    aux C.TypeConstArray {}      = True
    aux C.TypeIncompleteArray {} = True
    aux C.TypePrim {}            = False
    aux C.TypePointer {}         = False
    aux C.TypeFun {}             = False
    aux C.TypeVoid               = False
    aux C.TypeBlock {}           = False
    aux C.TypeExtBinding {}      = False

    auxRef :: C.FinalDeclId -> Bool
    auxRef declId =
        case declId.name.kind of
          C.NameKindOrdinary               -> False
          C.NameKindTagged C.TagKindStruct -> True
          C.NameKindTagged C.TagKindUnion  -> True
          C.NameKindTagged C.TagKindEnum   -> False

-- | Fancy types are heap types or constant arrays. We create high-level
-- wrapper for fancy types.
--
anyFancy :: [WrappedType] -> Bool
anyFancy types = any p types where
    p WrapType {} = False
    p HeapType {} = True
    p CAType {}   = True
    p AType {}    = True

-- | Types that we cannot directly pass via C FFI.
wrapType ::  C.Type -> WrappedType
wrapType ty
  -- Heap types
  | C.isCanonicalTypeStruct ty ||
    C.isCanonicalTypeUnion ty ||
    C.isCanonicalTypeComplex ty
  = HeapType ty

  -- Array types
  | Just aTy <- C.isCanonicalTypeArray ty
  = if C.isErasedTypeConstQualified ty then
      case aTy of
        C.ConstantArrayClassification n eTy -> CAType ty n eTy
        C.IncompleteArrayClassification eTy -> AType ty eTy
    else
      WrapType $ C.TypePointer (C.getArrayElementType aTy)

  -- Other types
  | otherwise
  = WrapType ty

-- | Type in low-level Haskell wrapper
unwrapType :: WrappedType -> C.Type
unwrapType = \case
    WrapType ty -> ty
    HeapType ty -> C.TypePointer ty
    CAType aTy _ eTy -> firstElemPtr aTy eTy
    AType aTy eTy -> firstElemPtr aTy eTy
  where
    -- NOTE: if an array type is const-qualified, then its array element type is
    -- also const-qualified, and vice versa.
    firstElemPtr :: C.Type -> C.Type -> C.Type
    firstElemPtr aTy eTy
      -- The array element type has a const qualifier.
      | C.isErasedTypeConstQualified eTy
      = C.TypePointer eTy
      -- The array type has a const qualifier, but the array element type does
      -- not.
      | C.isErasedTypeConstQualified aTy
      = C.TypePointer $ C.TypeQualified C.TypeQualifierConst eTy
      -- No const qualifiers on either the array type or the array element type.
      | otherwise
      = C.TypePointer eTy


-- | Type in high-level Haskell wrapper
unwrapOrigType :: WrappedType -> C.Type
unwrapOrigType (WrapType ty)    = ty
unwrapOrigType (HeapType ty)    = ty
unwrapOrigType (CAType oty _ _) = oty
unwrapOrigType (AType oty _)    = oty

isVoidW :: WrappedType -> Bool
isVoidW = C.isVoid . unwrapType

-- | Whether wrapped type is HeapType.
isWrappedHeap :: WrappedType -> Bool
isWrappedHeap WrapType {} = False
isWrappedHeap HeapType {} = True
isWrappedHeap CAType {}   = False
isWrappedHeap AType {}    = False

-- | userland-api C wrapper.
wrapperDecl
    :: String         -- ^ true C name
    -> String         -- ^ wrapper name
    -> WrappedType    -- ^ result type
    -> [WrappedType]  -- ^ arguments
    -> PC.Decl
wrapperDecl innerName wrapperName res args
    | isVoidW res
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid C.ImpureFunction (unwrapType <$> args')
          [PC.Expr $ PC.Call innerName (callArgs args' (PC.argsToIdx args'))]

    | isWrappedHeap res
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName C.TypeVoid C.ImpureFunction (unwrapType <$> (args' :> res))
          [PC.Assign (PC.LDeRef (PC.LVar IZ)) $ PC.Call innerName (callArgs args' (IS <$> PC.argsToIdx args'))]

    | otherwise
    = PC.withArgs args $ \args' ->
        PC.FunDefn wrapperName (unwrapType res) C.ImpureFunction (unwrapType <$> args')
          [PC.Return $ PC.Call innerName (callArgs args' (PC.argsToIdx args'))]
  where
    callArgs :: Env ctx' WrappedType -> Env ctx' (Idx ctx) -> [PC.Expr ctx]
    callArgs tys ids = toList (zipWithEnv f tys ids) where f ty idx = if isWrappedHeap ty then PC.DeRef (PC.Var idx) else PC.Var idx

-- | Generate a 'DeclFunction' for a high-level wrapper function
--
hsWrapperDeclFunction
    :: Hs.Name Hs.NsVar       -- ^ high-level name
    -> Hs.Name Hs.NsVar       -- ^ low-level import name
    -> WrappedType            -- ^ result type
    -> [WrappedType]          -- ^ arguments
    -> [Hs.FunctionParameter] -- ^ function parameter with comments
    -> C.Function             -- ^ original C function
    -> Maybe HsDoc.Comment    -- ^ function comment
    -> Hs.Decl
hsWrapperDeclFunction hiName loName res wrappedArgs wrapperParams cFunc mbComment =
  let resType = Type.inContext Type.FunRes $ unwrapOrigType res
   in case res of
        HeapType {} ->
          Hs.DeclFunction $ Hs.FunctionDecl
            { functionDeclName       = hiName
            , functionDeclParameters = wrapperParams
            , functionDeclResultType = HsIO resType
            , functionDeclBody       = goA EmptyEnv wrappedArgs
            , functionDeclOrigin     = Origin.Function cFunc
            , functionDeclComment    = mbComment
            }

        WrapType {} ->
          Hs.DeclFunction $ Hs.FunctionDecl
            { functionDeclName       = hiName
            , functionDeclParameters = wrapperParams
            , functionDeclResultType = HsIO resType
            , functionDeclBody       = goB EmptyEnv wrappedArgs
            , functionDeclOrigin     = Origin.Function cFunc
            , functionDeclComment    = mbComment
            }

        CAType {} ->
          panicPure "ConstantArray cannot occur as a result type"

        AType {} ->
          panicPure "Array cannot occur as a result type"
  where
    -- wrapper for fancy result
    goA :: Env ctx WrappedType -> [WrappedType] -> SHs.SExpr ctx
    goA env []     = goA' env (tabulateEnv (sizeEnv env) id) []
    goA env (x:xs) = SHs.ELam "x" $ goA (env :> x) xs

    goA' :: Env ctx' WrappedType -> Env ctx' (Idx ctx) -> [(Bool, Idx ctx)] -> SHs.SExpr ctx
    goA' EmptyEnv    EmptyEnv  zs
        = shsApps (SHs.EGlobal SHs.CAPI_allocaAndPeek)
          [ SHs.ELam "z" $ shsApps (SHs.EFree loName)
              (map
                (\(useConstPtr, x) -> constPtr useConstPtr $ SHs.EBound x)
                (fmap (second IS) zs ++ [(False, IZ)]))
          ]
      where
        constPtr :: Bool -> SHs.SExpr ctx -> SHs.SExpr ctx
        constPtr useConstPtr
          | useConstPtr = SHs.EApp (SHs.EGlobal SHs.ConstPtr_constructor)
          | otherwise = id

    goA' (tys :> ty) (xs :> x) zs = case ty of
        HeapType ty' -> shsApps (SHs.EGlobal SHs.CAPI_with) $
            let useConstPtr = C.isErasedTypeConstQualified ty' in
            [ SHs.EBound x
            , SHs.ELam "y" $ goA' tys (IS <$> xs) ((useConstPtr, IZ) : fmap (second IS) zs)
            ]

        CAType aTy _ _ -> shsApps (SHs.EGlobal SHs.ConstantArray_withPtr) $
            let useConstPtr = C.isErasedTypeConstQualified aTy in
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goA' tys (IS <$> xs) ((useConstPtr, IZ) : fmap (second IS) zs)
            ]

        AType aTy _ -> shsApps (SHs.EGlobal SHs.IncompleteArray_withPtr) $
            let useConstPtr = C.isErasedTypeConstQualified aTy in
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goA' tys (IS <$> xs) ((useConstPtr, IZ) : fmap (second IS) zs)
            ]

        WrapType{} ->
            goA' tys xs ((False, x) : zs)

    -- wrapper for non-fancy result.
    goB :: Env ctx WrappedType -> [WrappedType] -> SHs.SExpr ctx
    goB env []     = goB' env (tabulateEnv (sizeEnv env) id) []
    goB env (x:xs) = SHs.ELam "x" $ goB (env :> x) xs

    goB' :: Env ctx' WrappedType -> Env ctx' (Idx ctx) -> [(Bool, Idx ctx)] -> SHs.SExpr ctx
    goB' EmptyEnv    EmptyEnv  zs
        = shsApps (SHs.EFree loName)
            (map
              (\(useConstPtr, x) -> constPtr useConstPtr $ SHs.EBound x)
              zs)
      where
        constPtr :: Bool -> SHs.SExpr ctx -> SHs.SExpr ctx
        constPtr useConstPtr
          | useConstPtr = SHs.EApp (SHs.EGlobal SHs.ConstPtr_constructor)
          | otherwise = id

    goB' (tys :> ty) (xs :> x) zs = case ty of
        HeapType ty' -> shsApps (SHs.EGlobal SHs.CAPI_with) $
          let useConstPtr = C.isErasedTypeConstQualified ty' in
          [ SHs.EBound x
          , SHs.ELam "y" $ goB' tys (IS <$> xs) ((useConstPtr, IZ) : fmap (second IS) zs)
          ]

        CAType aTy _ _ -> shsApps (SHs.EGlobal SHs.ConstantArray_withPtr) $
            let useConstPtr = C.isErasedTypeConstQualified aTy in
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goB' tys (IS <$> xs) ((useConstPtr, IZ) : fmap (second IS) zs)
            ]

        AType aTy _ -> shsApps (SHs.EGlobal SHs.IncompleteArray_withPtr) $
            let useConstPtr = C.isErasedTypeConstQualified aTy in
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goB' tys (IS <$> xs) ((useConstPtr, IZ) : fmap (second IS) zs)
            ]

        WrapType {} ->
            goB' tys xs ((False, x) : zs)

shsApps :: SHs.SExpr ctx -> [SHs.SExpr ctx] -> SHs.SExpr ctx
shsApps = foldl' SHs.EApp

functionDecs ::
     HasCallStack
  => SHs.Safety
  -> TranslationConfig
  -> HaddockConfig
  -> BaseModuleName
  -> C.DeclInfo
  -> C.Function
  -> C.DeclSpec
  -> [Hs.Decl]
functionDecs safety opts haddockConfig moduleName info f _spec = concat [
      funDecls
    , [ hsWrapperDeclFunction highlevelName importName res wrappedArgTypes wrapParsedArgs f mbWrapComment
      | areFancy
      ]
    ]
  where
    areFancy = anyFancy (res : wrappedArgTypes)

    funDecls :: [Hs.Decl]
    funDecls =
        HsFI.foreignImportDecs
          importName
          (snd resType)
          (if areFancy then ffiParams else ffiParsedArgs)
          (uniqueCDeclName wrapperName)
          (CallConvUserlandCAPI userlandCapiWrapper)
          (Origin.Function f)
          (mconcat [
              if areFancy
                then Just nonFancyComment
                else mbFFIComment
            , ioComment
            , Just $ HsDoc.uniqueSymbol wrapperName
            ])
          safety

    userlandCapiWrapper :: UserlandCapiWrapper
    userlandCapiWrapper = UserlandCapiWrapper {
          capiWrapperDefinition =
            PC.prettyDecl (wrapperDecl innerName wrapperName.unique res wrappedArgTypes) ""
        , capiWrapperImport =
            getMainHashIncludeArg info
        }

    highlevelName = C.unsafeDeclIdHaskellName info.declId
    importName
        | areFancy  = highlevelName <> "_wrapper" -- TODO: Add to NameMangler pass
        | otherwise = highlevelName

    res = wrapType $ C.functionRes f

    -- Parameters for FFI import
    ffiParams = [ Hs.FunctionParameter
                   { functionParameterName    = fmap C.nameHs mbName
                   , functionParameterType    = Type.inContext Type.FunArg (unwrapType (wrapType ty))
                   , functionParameterComment = Nothing
                   }
                | (mbName, ty) <- C.functionArgs f
                ] ++ toList (fst resType)

    (mbFFIComment, ffiParsedArgs) =
      generateHaddocksWithInfoParams haddockConfig info ffiParams

    -- Parameters for wrapper decl
    wrapperParams = [ Hs.FunctionParameter
                     { functionParameterName    = fmap C.nameHs mbName
                     , functionParameterType    = Type.inContext Type.FunArg (unwrapOrigType (wrapType ty))
                     , functionParameterComment = Nothing
                     }
                  | (mbName, ty) <- C.functionArgs f
                  ]

    (mbWrapComment, wrapParsedArgs) =
      generateHaddocksWithInfoParams haddockConfig info wrapperParams

    wrappedArgTypes =
      [ wrapType ty
      | (_, ty) <- C.functionArgs f
      ]

    -- | When translating a 'C.Type' there are C types which we
    -- cannot pass directly using C FFI. We need to distinguish these.
    --
    -- Result types can be heap types, which are types we can't return by value
    -- due to Haskell FFI limitation. Or they can be normal types supported by
    -- Haskell FFI. This is also true for function arguments as well, result types
    -- are a special case where unsupported result types become arguments.
    resType :: (Maybe Hs.FunctionParameter, HsType)
    resType =
      case res of
        -- A heap type that is not supported by the Haskell FFI as a function
        -- result. We pass it as a function argument instead.
        HeapType {} -> (Just Hs.FunctionParameter {
            functionParameterName = Nothing
          , functionParameterType = Type.inContext Type.FunArg $ unwrapType res
          , functionParameterComment = Nothing
          }
          , hsIO $ HsPrimType HsPrimUnit
          )

        -- A "normal" result type that is supported by the Haskell FFI.
        WrapType {} ->
          ( Nothing
          , hsIO $ Type.inContext Type.FunRes $ unwrapType res
          )

        CAType {} ->
            panicPure "ConstantArray cannot occur as a result type"

        AType {} ->
            panicPure "Array cannot occur as a result type"

    -- | Decide based on the function attributes whether to include 'IO' in the
    -- result type of the foreign import. See the documentation on
    -- 'C.FunctionPurity'.
    --
    -- An exception to the rules: the foreign import function returns @void@
    -- when @res@ is a heap type, in which case a @const@ or @pure@ attribute
    -- does not make much sense, and so we just return the result in 'IO'.
    hsIO :: Hs.HsType -> Hs.HsType
    ioComment :: Maybe HsDoc.Comment
    (hsIO, ioComment) = case C.functionPurity (C.functionAttrs f) of
        C.HaskellPureFunction -> (id  , Nothing)
        C.CPureFunction       -> (HsIO, Just pureComment)
        C.ImpureFunction      -> (HsIO, Nothing)

    -- Generation of C wrapper for userland-capi.
    innerName :: String
    innerName = T.unpack info.declId.name.text

    wrapperName :: UniqueSymbol
    wrapperName = globallyUnique opts.translationUniqueId moduleName $ concat [
          show safety
        , "_"
        , innerName
        ]

    --
    -- Comments
    --

    -- "Pointer-based API for '<function>'"
    nonFancyComment :: HsDoc.Comment
    nonFancyComment = HsDoc.title [
          HsDoc.TextContent "Pointer-based API for"
        , HsDoc.Identifier (Hs.getName highlevelName)
        ]

    -- "Marked @__attribute((pure))__@"
    --
    -- C-pure functions can be safely encapsulated using 'unsafePerformIO' to
    -- create a Haskell-pure functions. We include a comment in the generated
    -- bindings to this effect.
    pureComment :: HsDoc.Comment
    pureComment = HsDoc.paragraph [
          HsDoc.TextContent "Marked"
        , HsDoc.Monospace
          [ HsDoc.Bold
            [ HsDoc.TextContent "attribute((pure))" ]
          ]
        ]

getMainHashIncludeArg :: HasCallStack => C.DeclInfo -> HashIncludeArg
getMainHashIncludeArg declInfo = case C.declHeaderInfo declInfo of
    Nothing -> panicPure "no main header for builtin"
    Just C.HeaderInfo{headerMainHeaders} -> NonEmpty.head headerMainHeaders

{-------------------------------------------------------------------------------
  Globals
-------------------------------------------------------------------------------}

-- | === Global variables
--
-- For by-reference foreign imports, @capi@ vs @ccall@ makes no difference:
-- @ghc@ does not create a wrapper. For non-extern non-static globals however it
-- is important that the header is imported /somewhere/, otherwise the global
-- variable is not linked in; we therefore add an explicit import. It is
-- important that we don't import such headers more than once, but this is taken
-- care of in 'csources'.
--
-- On Windows, simply generating a foreign import of a global variable's address
-- can lead to errors (see #898). For example, given a global @int
-- simpleGlobal@, the following foreign import might cause an error:
--
-- > foreign import capi safe "&simpleGlobal" simpleGlobal :: Ptr CInt
--
-- So, instead we generate a /stub/ function that simply returns the address of
-- the global variable ...
--
-- > /* get_simpleGlobal_ptr */
-- > __attribute__ ((const)) signed int *abc949ab (void) {
-- >   return &simpleGlobal;
-- > }
--
-- ... and then create a foreign import for the stub. Note that the name of the
-- stub function is mangled, though the original name of the stub function is
-- included in a comment before the stub.
--
-- > foreign import ccall unsafe "abc949ab" abc949ab :: IO (Ptr CInt)
--
-- Note that stub function also has a @const@ function attribute to emphasise
-- that the function always returns the same address throughout the lifetime of
-- the program. This means we could omit the 'IO' from the foreign import to
-- make it a pure foreign import. Instead, we make the foreign import impure and
-- we generate an additional pure Haskell function that safely unsafely runs the
-- 'IO'.
--
-- > {-# NOINLINE simpleGlobal_ptr #-}
-- > global_ptr :: Ptr CInt
-- > global_ptr = unsafePerformIO abc949ab
--
-- === Global /constant/ (i.e., @const@) variables
--
-- We generate bindings for these as we would generate bindings for
-- non-constant global variables.
--
-- However, if the type of the global constant has a 'Storable' instance,
-- we also generate an additional \"getter\" function in Haskell land that
-- returns precisely the value of the constant rather than a /pointer/ to
-- the value.
global ::
     TranslationConfig
  -> HaddockConfig
  -> BaseModuleName
  -> TranslationState
  -> C.DeclInfo
  -> C.Type
  -> C.DeclSpec
  -> [Hs.Decl]
global opts haddockConfig moduleName transState info ty _spec
    -- Generate getter if the type is @const@-qualified. We inspect the /erased/
    -- type because we want to see through newtypes as well.
    | C.isErasedTypeConstQualified ty = stubDecs ++ getConstGetterOfType ty
      -- Otherwise, do not generate a getter
    | otherwise = stubDecs
  where
    -- *** Stub ***
    stubDecs :: [Hs.Decl]
    pureStubName :: Hs.Name Hs.NsVar
    (stubDecs, pureStubName) =
      addressStubDecs opts haddockConfig moduleName info ty _spec

    getConstGetterOfType :: C.Type -> [Hs.Decl]
    getConstGetterOfType t = constGetter (Type.topLevel t) transState info pureStubName

-- | Getter for a constant (i.e., @const@) global variable
--
-- > simpleGlobal :: CInt
-- > simpleGlobal = unsafePerformIO (peek simpleGlobal_ptr)
--
-- We only generate a getter function if the type of the global constant has a
-- 'Storable' instance. In such cases, a user of the generated bindings should
-- use the foreign import of the stub function instead. Most notably, arrays of
-- unknown size do not have a 'Storable' instance.
constGetter ::
     HsType
  -> TranslationState
  -> C.DeclInfo
  -> Hs.Name Hs.NsVar
  -> [Hs.Decl]
constGetter ty transState info pureStubName = concat [
          [ Hs.DeclPragma (SHs.NOINLINE getterName)
          , getterDecl
          ]
        | -- We must have a storable instance available without any constraints.
          --
          -- We are generating a binding for a global variable here. This binding
          -- must be marked NOINLINE, so that it will be evaluated at most once.
          -- /If/ we have a Storable instance, but that storable instance has a
          -- superclass constraint, then we could _in principle_ add that superclass
          -- constraint to as a constraint to the type of the global, but this would
          -- then turn the global into a function instead.
          --
          -- TODO: we don't yet check whether the Storable instance has no
          -- superclass constraints. See issue #993.
          Hs.Storable
            `elem`
              Hs.getInstances (State.instanceMap transState) Nothing (Set.singleton Hs.Storable) [ty]
        ]
  where
    -- *** Getter ***
    --
    -- The "getter" peeks the value from the pointer
    getterDecl :: Hs.Decl
    getterDecl = Hs.DeclVar $ SHs.Var {
          varName    = getterName
        , varType    = getterType
        , varExpr    = getterExpr
        , varComment = Nothing
        }

    getterName = C.unsafeDeclIdHaskellName info.declId
    getterType = SHs.translateType ty
    getterExpr = SHs.EGlobal SHs.IO_unsafePerformIO
                `SHs.EApp` (SHs.EGlobal SHs.Storable_peek
                `SHs.EApp` (SHs.EGlobal SHs.ConstPtr_unConstPtr
                `SHs.EApp` SHs.EFree pureStubName))

-- | Create a stub C function that returns the address of a given declaration,
-- and create a binding to that stub C function.
--
-- See 'global' and 'globalConst' for example uses.
--
-- This function returns a pair @(stubDecs, stubImportName)@:
--
-- * @stubDecs@: a list of declarations for the pure\/impure stub.
--
-- * @pureStubName@: the identifier of the /pure/ stub.
addressStubDecs ::
     TranslationConfig
  -> HaddockConfig
  -> BaseModuleName
  -> C.DeclInfo -- ^ The given declaration
  -> C.Type -- ^ The type of the given declaration
  -> C.DeclSpec
  -> ( [Hs.Decl]
     , Hs.Name 'Hs.NsVar
     )
addressStubDecs opts haddockConfig moduleName info ty _spec =
    (foreignImport ++ runnerDecls, runnerName)
  where
    -- *** Stub (impure) ***

    -- We reuse the mangled stub name here, since the import is supposed to be
    -- internal. Users should use functioned identified by @runnerName@ instead,
    -- which does not include 'IO' in the return type.
    stubImportName :: Hs.Name 'Hs.NsVar
    stubImportName = unsafeUniqueHsName stubName

    stubImportType :: HsType
    stubImportType = HsIO $ Type.topLevel stubType

    stubName :: UniqueSymbol
    stubName =
        globallyUnique opts.translationUniqueId moduleName $
          "get_" ++ varName ++ "_ptr"

    varName :: String
    varName = T.unpack info.declId.name.text

    stubType :: C.Type
    stubType = C.TypePointer ty

    prettyStub :: String
    prettyStub = concat [
          "/* ", stubName.source, " */\n"
        , PC.prettyDecl stubDecl ""
        ]

    stubDecl :: PC.Decl
    stubDecl =
        PC.withArgs [] $ \args' ->
          PC.FunDefn stubName.unique stubType C.HaskellPureFunction args'
            [PC.Return $ PC.Address $ PC.NamedVar varName]

    userlandCapiWrapper :: UserlandCapiWrapper
    userlandCapiWrapper = UserlandCapiWrapper {
          capiWrapperDefinition = prettyStub
        , capiWrapperImport = getMainHashIncludeArg info
        }

    mbComment = generateHaddocksWithInfo haddockConfig info

    foreignImport :: [Hs.Decl]
    foreignImport =
        HsFI.foreignImportDecs
          stubImportName
          stubImportType
          []
          (uniqueCDeclName stubName)
          (CallConvUserlandCAPI userlandCapiWrapper)
          (Origin.Global ty)
          (Just $ HsDoc.uniqueSymbol stubName)
          -- These imports can be unsafe. We're binding to simple address stubs,
          -- so there are no callbacks into Haskell code. Moreover, they are
          -- short running code.
          SHs.Unsafe

    -- *** Stub (pure) ***

    runnerDecls :: [Hs.Decl]
    runnerDecls = [
          Hs.DeclPragma (SHs.NOINLINE runnerName)
        , runnerDecl
        ]

    runnerDecl :: Hs.Decl
    runnerDecl = Hs.DeclVar $ SHs.Var {
          varName    = runnerName
        , varType    = runnerType
        , varExpr    = runnerExpr
        , varComment = mbComment
        }

    runnerName = Hs.Name $ Hs.getIdentifier info.declId.haskellId <> "_ptr"
    runnerType = SHs.translateType (Type.topLevel stubType)
    runnerExpr = SHs.EGlobal SHs.IO_unsafePerformIO
                `SHs.EApp` SHs.EFree stubImportName

{-------------------------------------------------------------------------------
  Macro
-------------------------------------------------------------------------------}

macroVarDecs ::
     HaddockConfig
  -> C.DeclInfo
  -> C.CheckedMacroExpr
  -> [Hs.Decl]
macroVarDecs haddockConfig info macroExpr = [
      Hs.DeclMacroExpr $
        Hs.MacroExpr
          { macroExprName    = hsVarName
          , macroExprBody    = macroExpr
          , macroExprComment = generateHaddocksWithInfo haddockConfig info
          }
    ]
  where
    hsVarName :: Hs.Name Hs.NsVar
    hsVarName = C.unsafeDeclIdHaskellName info.declId
