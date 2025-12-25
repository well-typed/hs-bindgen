{-# LANGUAGE OverloadedLabels #-}

-- | Low-level translation of the C header to a Haskell module
module HsBindgen.Backend.Hs.Translation (
    generateDeclarations
  ) where

import Control.Monad.State qualified as State hiding (MonadState)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Type.Nat (SNatI)
import Data.Vec.Lazy qualified as Vec
import Optics.Core (over)

import Clang.HighLevel.Documentation qualified as Clang

import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Config (HaddockConfig)
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Config
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as HsFI
import HsBindgen.Backend.Hs.Translation.Function
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.Newtype qualified as Hs
import HsBindgen.Backend.Hs.Translation.Prim qualified as HsPrim
import HsBindgen.Backend.Hs.Translation.State (HsM, TranslationState)
import HsBindgen.Backend.Hs.Translation.State qualified as State
import HsBindgen.Backend.Hs.Translation.ToFromFunPtr qualified as ToFromFunPtr
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.PrettyC qualified as PC

import DeBruijn (Add (..), Idx (..), Weaken (..), pattern I1, pattern I2)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

generateDeclarations ::
     TranslationConfig
  -> HaddockConfig
  -> BaseModuleName
  -> DeclIndex
  -> [C.Decl]
  -> ByCategory_ [Hs.Decl]
generateDeclarations opts config name declIndex =
    fmap reverse .
      foldl' partitionBindingCategories mempty .
      generateDeclarations' opts config name declIndex
  where
    partitionBindingCategories ::
      ByCategory_ [a] -> WithCategory a  -> ByCategory_ [a]
    partitionBindingCategories xs (WithCategory cat decl) =
      over (lensForCategory cat) (decl :) xs

-- | Internal. Top-level declaration with foreign import category.
data WithCategory a = WithCategory {
    _withCategoryCategory :: Category
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
    State.runHsM $ do
      let scannedFunctionPointerTypes = scanAllFunctionPointerTypes decs
          -- Generate ToFunPtr/FromFunPtr instances for nested callback types
          -- These go in the main module to avoid orphan instances
          --WithCategory c
          fFIStubsAndFunPtrInstances =
                   [ WithCategory CType d
                   | C.TypePointers _ (C.TypeFun args res) <- Set.toList scannedFunctionPointerTypes
                   , not (any C.hasUnsupportedType (res:args))
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
scanAllFunctionPointerTypes :: [C.Decl] -> Set (C.Type Final)
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
    scanTypeForFunctionPointers :: C.Type Final -> Set (C.Type Final)
    scanTypeForFunctionPointers ty = case ty of
      -- Use TypePointers pattern to safely match N levels of indirection
      fp@(C.TypePointers _n (C.TypeFun args res)) ->
           Set.singleton fp
        <> foldMap scanTypeForFunctionPointers (res : args)
      C.TypePointers _ t               -> scanTypeForFunctionPointers t
      C.TypeIncompleteArray  t         -> scanTypeForFunctionPointers t
      C.TypeConstArray _ t             -> scanTypeForFunctionPointers t
      C.TypeBlock t                    -> scanTypeForFunctionPointers t
      C.TypeQualified _ t              -> scanTypeForFunctionPointers t
      C.TypeTypedef (C.TypedefRef _ t) -> scanTypeForFunctionPointers t
      _                                -> Set.empty

-- | Check if a type is defined in the current module
isDefinedInCurrentModule :: DeclIndex -> C.Type Final -> Bool
isDefinedInCurrentModule declIndex =
    any (isInDeclIndex . snd) . C.depsOfType
  where
    isInDeclIndex :: C.DeclIdPair -> Bool
    isInDeclIndex declId = isJust $ DeclIndex.lookup declId.cName declIndex

{-------------------------------------------------------------------------------
  Declarations
------------------------------------------------------------------------------}

-- TODO: Take DeclSpec into account
generateDecs ::
     TranslationConfig
  -> HaddockConfig
  -> BaseModuleName
  -> C.Decl
  -> HsM [WithCategory Hs.Decl]
generateDecs opts haddockConfig moduleName (C.Decl info kind spec) =
    case kind of
      C.DeclStruct struct -> withCategoryM CType $
        reifyStructFields struct $ structDecs opts haddockConfig info struct spec
      C.DeclUnion union -> withCategoryM CType $
        unionDecs haddockConfig info union spec
      C.DeclEnum e -> withCategoryM CType $
        enumDecs opts haddockConfig info e spec
      C.DeclTypedef d -> withCategoryM CType $
        -- Deal with typedefs around function pointers (#1380)
        case d.typedefType of
          C.TypePointers n (C.TypeFun args res) ->
            typedefFunPtrDecs opts haddockConfig info n (args, res) d.typedefNames spec
          _otherwise ->
            typedefDecs opts haddockConfig info Origin.Typedef d spec
      C.DeclOpaque -> withCategoryM CType $
        opaqueDecs haddockConfig info spec
      C.DeclFunction f ->
        let funDeclsWith safety =
              functionDecs safety opts haddockConfig moduleName info f spec
            funType  = (C.TypeFun (snd <$> C.functionArgs f) (C.functionRes f))
            -- Declare a function pointer. We can pass this 'FunPtr' to C
            -- functions that take a function pointer of the appropriate type.
            funPtrDecls = fst $
              addressStubDecs opts haddockConfig moduleName info funType HaskellId spec
        in  pure $ withCategory (CTerm CSafe)   (funDeclsWith SHs.Safe)
                ++ withCategory (CTerm CUnsafe) (funDeclsWith SHs.Unsafe)
                ++ withCategory (CTerm CFunPtr)  funPtrDecls
      C.DeclMacro macro -> withCategoryM CType $
        macroDecs opts haddockConfig info macro spec
      C.DeclGlobal ty -> do
        transState <- State.get
        pure $ withCategory (CTerm CGlobal) $
          global opts haddockConfig moduleName transState info ty spec
    where
      withCategory :: Category -> [a] -> [WithCategory a]
      withCategory c = map (WithCategory c)

      withCategoryM :: Functor m => Category -> m [a] -> m [WithCategory a]
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
structDecs :: forall n.
     SNatI n
  => TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.Struct
  -> C.DeclSpec
  -> Vec n C.StructField
  -> HsM [Hs.Decl]
structDecs opts haddockConfig info struct spec fields = do
    (insts, decls) <- aux <$> State.gets State.instanceMap
    State.modifyInstanceMap' $ Map.insert structName insts
    pure decls
  where
    structName :: Hs.Name Hs.NsTypeConstr
    structName = Hs.unsafeHsIdHsName info.declId.hsName

    structFields :: Vec n Hs.Field
    structFields = flip Vec.map fields $ \f -> Hs.Field {
        fieldName    = Hs.unsafeHsIdHsName (C.structFieldInfo f).fieldName.hsName
      , fieldType    = Type.topLevel (C.structFieldType f)
      , fieldOrigin  = Origin.StructField f
      , fieldComment = mkHaddocksFieldInfo haddockConfig info (C.structFieldInfo f)
      }

    candidateInsts :: Set Hs.TypeClass
    candidateInsts = Set.union (Set.fromList [Hs.Storable, Hs.Prim]) $
      Set.fromList (snd <$> translationDeriveStruct opts)

    -- everything in aux is state-dependent
    aux :: Hs.InstanceMap -> (Set Hs.TypeClass, [Hs.Decl])
    aux instanceMap = (insts,) $
        structDecl : storableDecl ++ primDecl ++ optDecls ++ hasFlamDecl ++
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
          , structConstr    = MangleNames.recordConstr (C.structNames struct)
          , structFields    = structFields
          , structInstances = insts
          , structOrigin    = Just Origin.Decl{
                declInfo = info
              , declKind = Origin.Struct struct
              , declSpec = spec
              }
          , structComment = mkHaddocks haddockConfig info structName
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

        primDecl :: [Hs.Decl]
        primDecl = HsPrim.mkPrimInstance insts hsStruct struct

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
    fieldName = Hs.unsafeHsIdHsName (C.structFieldInfo f).fieldName.hsName

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
    name = T.unpack (C.structFieldInfo f).fieldName.hsName.text

pokeStructField :: Idx ctx -> C.StructField -> Idx ctx -> Hs.PokeCField ctx
pokeStructField ptr f x = case C.structFieldWidth f of
    Nothing -> Hs.PokeCField (HsStrLit name) ptr x
    Just _w  -> Hs.PokeCBitfield (HsStrLit name) ptr x
  where
    name = T.unpack (C.structFieldInfo f).fieldName.hsName.text

{-------------------------------------------------------------------------------
  Opaque struct and opaque enum
-------------------------------------------------------------------------------}

opaqueDecs ::
     HaddockConfig
  -> C.DeclInfo
  -> C.DeclSpec
  -> HsM [Hs.Decl]
opaqueDecs haddockConfig info spec = do
    State.modifyInstanceMap' $ Map.insert name Set.empty
    return [decl]
  where
    name :: Hs.Name Hs.NsTypeConstr
    name = Hs.unsafeHsIdHsName info.declId.hsName

    -- TODO: Do we still need the @Origin@ at all?
    decl :: Hs.Decl
    decl = Hs.DeclEmpty Hs.EmptyData {
        emptyDataName   = name
      , emptyDataOrigin = Origin.Decl{
            declInfo = info
          , declKind = Origin.Opaque info.declId.cName.name.kind
          , declSpec = spec
          }
      , emptyDataComment = mkHaddocks haddockConfig info name
      }

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

unionDecs ::
     HaddockConfig
  -> C.DeclInfo
  -> C.Union
  -> C.DeclSpec
  -> HsM [Hs.Decl]
unionDecs haddockConfig info union spec = do
    nt <- newtypeDec
    flip aux nt <$> State.get
  where
    newtypeDec :: HsM Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.unsafeHsIdHsName info.declId.hsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = MangleNames.newtypeConstr (C.unionNames union)

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
              fieldName    = MangleNames.newtypeField (C.unionNames union)
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
        newtypeComment = mkHaddocks haddockConfig info newtypeName

        candidateInsts :: Set Hs.TypeClass
        candidateInsts = Set.empty

        knownInsts :: Set Hs.TypeClass
        knownInsts = Set.singleton Hs.Storable

    -- everything in aux is state-dependent
    aux :: TranslationState -> Hs.Newtype -> [Hs.Decl]
    aux transState nt =
        Hs.DeclNewtype nt : storableDecl : primDecl : accessorDecls ++
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

        primDecl :: Hs.Decl
        primDecl =
          Hs.DeclDeriveInstance
            Hs.DeriveInstance {
              deriveInstanceStrategy = Hs.DeriveVia sba
            , deriveInstanceClass    = Hs.Prim
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
              getterName = Hs.unsafeHsIdHsName $ "get_" <> unionFieldInfo.fieldName.hsName
              setterName = Hs.unsafeHsIdHsName $ "set_" <> unionFieldInfo.fieldName.hsName
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
                      , unionGetterComment = mkHaddocksFieldInfo haddockConfig info unionFieldInfo
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
    fieldName = Hs.unsafeHsIdHsName (C.unionFieldInfo f).fieldName.hsName

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
     TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.Enum
  -> C.DeclSpec
  -> HsM [Hs.Decl]
enumDecs opts haddockConfig info e spec = do
    nt <- newtypeDec
    pure $ aux nt
  where
    newtypeDec :: HsM Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.unsafeHsIdHsName info.declId.hsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = MangleNames.newtypeConstr (C.enumNames e)

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
            fieldName    = MangleNames.newtypeField (C.enumNames e)
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
        newtypeComment = mkHaddocks haddockConfig info newtypeName

        candidateInsts :: Set Hs.TypeClass
        candidateInsts = Set.empty

        knownInsts :: Set Hs.TypeClass
        knownInsts = Set.union (Set.fromList [Hs.Show, Hs.Read, Hs.Storable, Hs.HasBaseForeignType]) $
          Set.fromList (snd <$> translationDeriveEnum opts)

    -- everything in aux is state-dependent
    aux :: Hs.Newtype -> [Hs.Decl]
    aux nt =
        Hs.DeclNewtype nt : storableDecl : primDecl : HsFI.hasBaseForeignTypeDecs nt ++
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

        primDecl :: Hs.Decl
        primDecl =
          Hs.DeclDeriveInstance
            Hs.DeriveInstance {
              deriveInstanceStrategy = Hs.DeriveVia nt.newtypeField.fieldType
            , deriveInstanceClass    = Hs.Prim
            , deriveInstanceName     = nt.newtypeName
            , deriveInstanceComment  = Nothing
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
              { patSynName    = Hs.unsafeHsIdHsName enumConstantInfo.fieldName.hsName
              , patSynType    = nt.newtypeName
              , patSynConstr  = nt.newtypeConstr
              , patSynValue   = enumConstantValue
              , patSynOrigin  = Origin.EnumConstant enumValue
              , patSynComment = mkHaddocksFieldInfo haddockConfig info enumConstantInfo
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
     TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> (C.Typedef -> Origin.Newtype)
  -> C.Typedef
  -> C.DeclSpec
  -> HsM [Hs.Decl]
typedefDecs opts haddockConfig info mkNewtypeOrigin typedef spec = do
    nt <- newtypeDec
    pure $ aux nt
  where
    newtypeDec :: HsM Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.unsafeHsIdHsName info.declId.hsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = MangleNames.newtypeConstr (C.typedefNames typedef)

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
            fieldName    = MangleNames.newtypeField (C.typedefNames typedef)
          , fieldType    = Type.topLevel (C.typedefType typedef)
          , fieldOrigin  = Origin.GeneratedField
          , fieldComment = Nothing
          }

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin =  Origin.Decl{
            declInfo = info
          , declKind = mkNewtypeOrigin typedef
          , declSpec = spec
          }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment =  mkHaddocks haddockConfig info newtypeName

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
        Hs.DeclNewtype nt : newtypeWrapper ++ storableDecl ++ primDecl ++ optDecls ++
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

        primDecl :: [Hs.Decl]
        primDecl
          | Hs.Prim `Set.notMember` insts = []
          | otherwise = singleton $
              Hs.DeclDeriveInstance
                Hs.DeriveInstance {
                  deriveInstanceStrategy = Hs.DeriveNewtype
                , deriveInstanceClass    = Hs.Prim
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
            C.TypeFun args res | not (any C.hasUnsupportedType (res:args)) ->
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

-- | Typedef around function pointer
--
-- Given
--
-- > typedef void (*f)(int x, int y);
--
-- We want to generate /two/ types:
--
-- > newtype F_Aux = F_Aux (FC.CInt -> FC.CInt -> IO ())
-- > newtype F     = F (Ptr.FunPtr F_Deref)
--
-- so that @F_Aux@ can be given @ToFunPtr@/@FromFunPtr@ instances.
typedefFunPtrDecs ::
     TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> Int                             -- ^ Number of indirections
  -> ([C.Type Final], C.Type Final)  -- ^ Function arguments and result
  -> MangleNames.NewtypeNames
  -> C.DeclSpec
  -> HsM [Hs.Decl]
typedefFunPtrDecs opts haddockConfig origInfo n (args, res) origNames origSpec =
    fmap concat $ sequence [
        typedefDecs opts haddockConfig auxInfo  Origin.Aux     auxTypedef  auxSpec
      , typedefDecs opts haddockConfig origInfo Origin.Typedef mainTypedef origSpec
      ]
  where
    -- TODO: For historical reasons we currently implement this by making a
    -- "fake" C declaration, and then translating that immediately to Haskell.
    -- We should fuse this, and just generate the Haskell declarations directly.

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1379>
    -- The name of this auxiliary type should be configurable.
    --
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1427>
    -- This should be called "_Aux" instead.
    auxHsName :: Hs.Identifier
    auxHsName = origInfo.declId.hsName <> "_Deref"

    auxInfo :: C.DeclInfo
    auxInfo = C.DeclInfo {
          declLoc        = origInfo.declLoc
        , declHeaderInfo = origInfo.declHeaderInfo
        , declComment    = Just auxComment
        , declId         = C.DeclIdPair{
              -- Still refer to the /original/ C decl...?
              cName  = origInfo.declId.cName
            , hsName = auxHsName
            }
        }

    auxComment :: Clang.Comment C.CommentRef
    auxComment = Clang.Comment [
          Clang.Paragraph [
              Clang.TextContent "Auxiliary type used by "
            , Clang.InlineRefCommand $
                C.CommentRef
                  origInfo.declId.cName.name.text
                  (Just origInfo.declId)
            ]
        ]

    -- TODO: This duplicates logic from 'mkNewtypeNames' in the name mangler.
    auxTypedef :: C.Typedef
    auxTypedef = C.Typedef{
          typedefType  = C.TypeFun args res
        , typedefNames = MangleNames.NewtypeNames{
              newtypeConstr = Hs.unsafeHsIdHsName $          auxHsName
            , newtypeField  = Hs.unsafeHsIdHsName $ "un_" <> auxHsName
            }
        }

    -- TODO: Is this right..?
    auxSpec :: C.DeclSpec
    auxSpec = C.DeclSpec{
          declSpecC  = Nothing
        , declSpecHs = Nothing
        }

    mainTypedef :: C.Typedef
    mainTypedef = C.Typedef{
          typedefNames = origNames
        , typedefType  = C.TypePointers n $ C.TypeTypedef C.TypedefRef{
              ref        = auxInfo.declId
            , underlying = C.TypeFun args res
            }
        }

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

macroDecs ::
     TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.CheckedMacro
  -> C.DeclSpec
  -> HsM [Hs.Decl]
macroDecs opts haddockConfig info checkedMacro spec =
    case checkedMacro of
      C.MacroType ty   -> macroDecsTypedef opts haddockConfig info ty spec
      C.MacroExpr expr -> pure $ macroVarDecs haddockConfig info expr

macroDecsTypedef ::
     TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.CheckedMacroType
  -> C.DeclSpec
  -> HsM [Hs.Decl]
macroDecsTypedef opts haddockConfig info macroType spec = do
    nt <- newtypeDec
    pure $ aux nt
  where
    newtypeDec :: HsM Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.unsafeHsIdHsName info.declId.hsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = MangleNames.newtypeConstr (C.macroTypeNames macroType)

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
              fieldName    = MangleNames.newtypeField (C.macroTypeNames macroType)
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
        newtypeComment = mkHaddocks haddockConfig info newtypeName

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
-- > /* get_simpleGlobal */
-- > __attribute__ ((const)) signed int *abc949ab (void) {
-- >   return &simpleGlobal;
-- > }
--
-- ... and then create a foreign import for the stub. Note that the name of the
-- stub function is mangled, though the original name of the stub function is
-- included in a comment before the stub.
--
-- > foreign import ccall unsafe "hs_bindgen_abc949ab" hs_bindgen_abc949ab :: IO (Ptr CInt)
--
-- Note that stub function also has a @const@ function attribute to emphasise
-- that the function always returns the same address throughout the lifetime of
-- the program. This means we could omit the 'IO' from the foreign import to
-- make it a pure foreign import. Instead, we make the foreign import impure and
-- we generate an additional pure Haskell function that safely unsafely runs the
-- 'IO'.
--
-- > {-# NOINLINE simpleGlobal #-}
-- > global :: Ptr CInt
-- > global = unsafePerformIO hs_bindgen_abc949ab
--
-- === Global /constant/ (i.e., @const@) variables
--
-- We generate bindings for these as we would generate bindings for non-constant
-- global variables.
--
-- However, if the type of the global constant has a 'Storable' instance, we
-- also generate an additional \"getter\" function in Haskell land that returns
-- precisely the value of the constant rather than a /pointer/ to the value.
global ::
     TranslationConfig
  -> HaddockConfig
  -> BaseModuleName
  -> TranslationState
  -> C.DeclInfo
  -> C.Type Final
  -> C.DeclSpec
  -> [Hs.Decl]
global opts haddockConfig moduleName transState info ty _spec
    -- Generate getter if the type is @const@-qualified. We inspect the /erased/
    -- type because we want to see through newtypes as well.
    --
    -- We must have a storable instance available without any constraints.
    --
    -- We are generating a binding for a global variable here. This binding must
    -- be marked NOINLINE, so that it will be evaluated at most once. /If/ we
    -- have a Storable instance, but that storable instance has a superclass
    -- constraint, then we could _in principle_ add that superclass constraint
    -- as a constraint to the type of the global, but this would then turn the
    -- global into a function instead.
    --
    -- TODO: we don't yet check whether the Storable instance has no
    -- superclass constraints. See issue #993.
    | C.isErasedTypeConstQualified ty && Hs.Storable `elem` insts =
      let stubDecs     :: [Hs.Decl]
          pureStubName :: Hs.Name Hs.NsVar
          (stubDecs, pureStubName) = getStubDecsWith GlobalUniqueId
          constGetterOfType :: [Hs.Decl]
          constGetterOfType = constGetter (Type.topLevel ty) info pureStubName
      in  stubDecs ++ constGetterOfType
    -- Otherwise, do not generate a getter
    | otherwise = fst $ getStubDecsWith HaskellId
  where
    getStubDecsWith :: RunnerNameSpec -> ([Hs.Decl], Hs.Name Hs.NsVar)
    getStubDecsWith x = addressStubDecs opts haddockConfig moduleName info ty x _spec

    insts :: Set Hs.TypeClass
    insts =
      Hs.getInstances
        transState.instanceMap
        Nothing
        (Set.singleton Hs.Storable)
        [Type.topLevel ty]

-- | Getter for a constant (i.e., @const@) global variable
--
-- > simpleGlobal :: CInt
-- > simpleGlobal = unsafePerformIO (peek simpleGlobal)
--
-- We only generate a getter function if the type of the global constant has a
-- 'Storable' instance. In such cases, a user of the generated bindings should
-- use the foreign import of the stub function instead. Most notably, arrays of
-- unknown size do not have a 'Storable' instance.
constGetter ::
     HsType
  -> C.DeclInfo
  -> Hs.Name Hs.NsVar
  -> [Hs.Decl]
constGetter ty info pureStubName = singleton getterDecl
  where
    -- *** Getter ***
    --
    -- The "getter" peeks the value from the pointer
    getterDecl :: Hs.Decl
    getterDecl = Hs.DeclVar $ Hs.Var {
          name    = getterName
        , typ     = getterType
        , expr    = getterExpr
        , pragmas = [SHs.NOINLINE]
        , comment = Nothing
        }

    getterName = Hs.unsafeHsIdHsName info.declId.hsName
    getterType = SHs.translateType ty
    getterExpr = SHs.EGlobal SHs.IO_unsafePerformIO
                `SHs.EApp` (SHs.EGlobal SHs.Storable_peek
                `SHs.EApp` (SHs.EGlobal SHs.ConstPtr_unConstPtr
                `SHs.EApp` SHs.EFree pureStubName))

data RunnerNameSpec =
      -- | The runner is public (i.e, "exported"), and we give it the human
      --   readable Haskell ID.
      HaskellId
      -- | The runner is internal (i.e., "not exported"), so we use a globally
      --   unique identifier containing a hash.
    | GlobalUniqueId

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
  -> C.DeclInfo       -- ^ The given declaration
  -> C.Type Final     -- ^ The type of the given declaration
  -> RunnerNameSpec
  -> C.DeclSpec
  -> ( [Hs.Decl]
     , Hs.Name 'Hs.NsVar
     )
addressStubDecs opts haddockConfig moduleName info ty runnerNameSpec _spec =
    (foreignImport : runnerDecls, runnerName)
  where
    -- *** Stub (impure) ***
    stubImportType :: HsType
    stubImportType = HsIO $ Type.topLevel stubType

    stubSymbol :: UniqueSymbol
    stubSymbol =
        globallyUnique opts.translationUniqueId moduleName $
          "get_" ++ varName

    stubName :: Hs.Name Hs.NsVar
    stubName = Hs.InternalName stubSymbol

    varName :: String
    varName = T.unpack $ info.declId.cName.name.text

    stubType :: C.Type Final
    stubType = C.TypePointers 1 ty

    prettyStub :: String
    prettyStub = concat [
          "/* ", stubSymbol.source, " */\n"
        , PC.prettyDecl stubDecl ""
        ]

    stubDecl :: PC.Decl
    stubDecl =
        PC.withArgs [] $ \args' ->
          PC.FunDefn stubSymbol.unique stubType C.HaskellPureFunction args'
            [PC.Return $ PC.Address $ PC.NamedVar varName]

    cWrapper :: CWrapper
    cWrapper = CWrapper {
          cWrapperDefinition = prettyStub
        , cWrapperImport = getMainHashIncludeArg info
        }

    mbComment = mkHaddocks haddockConfig info runnerName

    foreignImport :: Hs.Decl
    foreignImport =
        HsFI.foreignImportDec
          stubName
          stubImportType
          []
          (uniqueCDeclName stubSymbol)
          (CallConvUserlandCAPI cWrapper)
          (Origin.Global ty)
          -- These imports can be unsafe. We're binding to simple address stubs,
          -- so there are no callbacks into Haskell code. Moreover, they are
          -- short running code.
          SHs.Unsafe

    -- *** Stub (pure) ***

    runnerDecls :: [Hs.Decl]
    runnerDecls = singleton runnerDecl

    runnerDecl :: Hs.Decl
    runnerDecl = Hs.DeclVar $ Hs.Var {
          name    = runnerName
        , typ     = runnerType
        , expr    = runnerExpr
        , pragmas = [SHs.NOINLINE]
        , comment = mbComment <> mbUniqueSymbolComment
        }

    mbUniqueSymbolComment :: Maybe HsDoc.Comment
    mbUniqueSymbolComment = case runnerName of
      Hs.ExportedName _ -> Nothing
      Hs.InternalName x -> Just $ HsDoc.uniqueSymbol x

    name :: Text
    name = info.declId.hsName.text

    uniquify :: Text -> UniqueSymbol
    uniquify =
      globallyUnique opts.translationUniqueId moduleName
      . Text.unpack

    runnerName = case runnerNameSpec of
        HaskellId      -> Hs.ExportedName (Hs.UnsafeExportedName name)
        GlobalUniqueId -> Hs.InternalName $ uniquify name

    runnerType = SHs.translateType (Type.topLevel stubType)
    runnerExpr = SHs.EGlobal SHs.IO_unsafePerformIO
                `SHs.EApp` SHs.EFree stubName

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
          { name    = hsVarName
          , body    = macroExpr
          , comment = mkHaddocks haddockConfig info hsVarName
          }
    ]
  where
    hsVarName :: Hs.Name Hs.NsVar
    hsVarName = Hs.unsafeHsIdHsName info.declId.hsName
