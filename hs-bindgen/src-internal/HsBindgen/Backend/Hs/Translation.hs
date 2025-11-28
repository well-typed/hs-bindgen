-- | Low-level translation of the C header to a Haskell module
module HsBindgen.Backend.Hs.Translation (
    generateDeclarations
  ) where

import Control.Arrow
import Control.Monad.State qualified as State
import Crypto.Hash.SHA256 (hash)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as B
import Data.Char (isLetter)
import Data.List (intercalate)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Type.Nat (SNatI)
import Data.Vec.Lazy qualified as Vec
import GHC.Unicode (isDigit)

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Config (HaddockConfig)
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Haddock.Translation
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Config
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.Internal
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Naming qualified as C
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
  -> Hs.ModuleName
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
  -> Hs.ModuleName
  -> DeclIndex
  -> [C.Decl]
  -> [WithCategory Hs.Decl]
generateDeclarations' opts haddockConfig moduleName declIndex decs =
    flip State.evalState Map.empty $ do
      let scannedFunctionPointerTypes = scanAllFunctionPointerTypes decs
          -- Generate ToFunPtr/FromFunPtr instances for nested callback types
          -- These go in the main module to avoid orphan instances
          --WithCategory c
          fFIStubsAndFunPtrInstances =
                   [ WithCategory BType d
                   | C.TypePointer (C.TypeFun args res) <- Set.toList scannedFunctionPointerTypes
                   , not (any hasUnsupportedType (res:args))
                   , any (isDefinedInCurrentModule declIndex) (res:args)
                   , d <- functionTypeFFIStubsAndFunPtrInstances args res
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
      C.TypePointer t                       -> scanTypeForFunctionPointers t
      C.TypeIncompleteArray  t              -> scanTypeForFunctionPointers t
      C.TypeConstArray _ t                  -> scanTypeForFunctionPointers t
      C.TypeBlock t                         -> scanTypeForFunctionPointers t
      C.TypeQualified _ t                   -> scanTypeForFunctionPointers t
      C.TypeTypedef (C.TypedefRegular _ t)  -> scanTypeForFunctionPointers t
      C.TypeTypedef (C.TypedefSquashed _ t) -> scanTypeForFunctionPointers t
      _                                     -> Set.empty

-- | Check if a type is defined in the current module
isDefinedInCurrentModule :: DeclIndex -> C.Type -> Bool
isDefinedInCurrentModule declIndex ty = case ty of
  C.TypeStruct namePair origin                 -> isInDeclIndex namePair origin (C.NameKindTagged C.TagKindStruct)
  C.TypeUnion namePair origin                  -> isInDeclIndex namePair origin (C.NameKindTagged C.TagKindUnion)
  C.TypeEnum namePair origin                   -> isInDeclIndex namePair origin (C.NameKindTagged C.TagKindEnum)
  C.TypePointer (C.TypeStruct namePair origin) -> isInDeclIndex namePair origin (C.NameKindTagged C.TagKindStruct)
  C.TypePointer (C.TypeUnion namePair origin)  -> isInDeclIndex namePair origin (C.NameKindTagged C.TagKindUnion)
  C.TypePointer (C.TypeEnum namePair origin)   -> isInDeclIndex namePair origin (C.NameKindTagged C.TagKindEnum)
  C.TypeTypedef (C.TypedefRegular namePair _)  -> isInDeclIndex namePair C.NameOriginInSource C.NameKindOrdinary
  C.TypeTypedef (C.TypedefSquashed name _)     -> isInDeclIndex (C.NamePair name (Hs.Identifier (C.getName name)))
                                                               C.NameOriginInSource C.NameKindOrdinary
  C.TypeQualified _ t                          -> isDefinedInCurrentModule declIndex t
  _                                            -> False
  where
    isInDeclIndex :: C.NamePair -> C.NameOrigin -> C.NameKind -> Bool
    isInDeclIndex namePair origin nameKind =
      let prelimDeclId =
            case origin of
              C.NameOriginInSource           -> C.PrelimDeclIdNamed (C.nameC namePair)
              C.NameOriginGenerated anonId   -> C.PrelimDeclIdAnon anonId
              C.NameOriginRenamedFrom _      -> C.PrelimDeclIdNamed (C.nameC namePair)
          qualPrelimDeclId = C.qualPrelimDeclId prelimDeclId nameKind
      in isJust $ DeclIndex.lookup qualPrelimDeclId declIndex

{-------------------------------------------------------------------------------
  Instance Map
-------------------------------------------------------------------------------}

type InstanceMap = Map (Hs.Name Hs.NsTypeConstr) (Set Hs.TypeClass)

getInstances ::
     HasCallStack
  => InstanceMap             -- ^ Current state
  -> Hs.Name Hs.NsTypeConstr -- ^ Name of current type
  -> Set Hs.TypeClass        -- ^ Candidate instances
  -> [HsType]                -- ^ Dependencies
  -> Set Hs.TypeClass
getInstances instanceMap name = aux
  where
    aux :: Set Hs.TypeClass -> [HsType] -> Set Hs.TypeClass
    aux acc [] = acc
    aux acc (hsType:hsTypes)
      | Set.null acc = acc
      | otherwise = case hsType of
          HsPrimType primType -> aux (acc /\ hsPrimTypeInsts primType) hsTypes
          HsTypRef name'
            | name' == name -> aux acc hsTypes
            | otherwise -> case Map.lookup name' instanceMap of
                Just instances -> aux (acc /\ instances) hsTypes
                Nothing -> panicPure $ "type not found: " ++ show name'
          HsConstArray _n hsType' ->
            -- constrain by ConstantArray item type in next step
            aux (acc /\ cArrayInsts) $ hsType' : hsTypes
          HsIncompleteArray hsType' ->
            -- constrain by Array item type in next step
            aux (acc /\ arrayInsts) $ hsType' : hsTypes
          HsPtr{} -> aux (acc /\ ptrInsts) hsTypes
          HsFunPtr{} -> aux (acc /\ ptrInsts) hsTypes
          HsIO{} -> Set.empty
          HsFun{} -> Set.empty
          HsExtBinding _ref _cTypeSpec mHsTypeSpec ->
            let acc' = case mHsTypeSpec of
                  Just hsTypeSpec -> acc /\ hsTypeSpecInsts hsTypeSpec
                  Nothing         -> acc
            in  aux acc' hsTypes
          HsByteArray{} ->
            let acc' = acc /\ Set.fromList [Hs.Eq, Hs.Ord, Hs.Show]
            in  aux acc' hsTypes
          HsSizedByteArray{} ->
            let acc' = acc /\ Set.fromList [Hs.Eq, Hs.Show]
            in  aux acc' hsTypes
          HsBlock t ->
            aux acc (t:hsTypes)
          HsComplexType primType -> aux (acc /\ hsPrimTypeInsts primType) hsTypes
          HsStrLit{} -> Set.empty

    (/\) :: Ord a => Set a -> Set a -> Set a
    (/\) = Set.intersection

    hsPrimTypeInsts :: HsPrimType -> Set Hs.TypeClass
    hsPrimTypeInsts = \case
      HsPrimVoid       -> Set.fromList [Hs.Eq, Hs.Ix, Hs.Ord, Hs.Read, Hs.Show]
      HsPrimUnit       -> unitInsts
      HsPrimCChar      -> integralInsts
      HsPrimCSChar     -> integralInsts
      HsPrimCUChar     -> integralInsts
      HsPrimCInt       -> integralInsts
      HsPrimCUInt      -> integralInsts
      HsPrimCShort     -> integralInsts
      HsPrimCUShort    -> integralInsts
      HsPrimCLong      -> integralInsts
      HsPrimCULong     -> integralInsts
      HsPrimCPtrDiff   -> integralInsts
      HsPrimCSize      -> integralInsts
      HsPrimCLLong     -> integralInsts
      HsPrimCULLong    -> integralInsts
      HsPrimCBool      -> integralInsts
      HsPrimCFloat     -> floatingInsts
      HsPrimCDouble    -> floatingInsts
      HsPrimCStringLen -> Set.fromList [Hs.Eq, Hs.Ord, Hs.Show]
      HsPrimInt        -> integralInsts

    unitInsts :: Set Hs.TypeClass
    unitInsts = Set.fromList [
        Hs.Eq
      , Hs.Ord
      , Hs.Read
      , Hs.ReadRaw
      , Hs.Show
      , Hs.StaticSize
      , Hs.Storable
      , Hs.WriteRaw
      ]

    integralInsts :: Set Hs.TypeClass
    integralInsts = Set.fromList [
        Hs.Bits
      , Hs.Bounded
      , Hs.Enum
      , Hs.Eq
      , Hs.FiniteBits
      , Hs.Integral
      , Hs.Ix
      , Hs.Num
      , Hs.Ord
      , Hs.Read
      , Hs.ReadRaw
      , Hs.Real
      , Hs.Show
      , Hs.StaticSize
      , Hs.Storable
      , Hs.WriteRaw
      ]

    floatingInsts :: Set Hs.TypeClass
    floatingInsts = Set.fromList [
        Hs.Enum
      , Hs.Eq
      , Hs.Floating
      , Hs.Fractional
      , Hs.Num
      , Hs.Ord
      , Hs.Read
      , Hs.ReadRaw
      , Hs.Real
      , Hs.RealFloat
      , Hs.RealFrac
      , Hs.Show
      , Hs.StaticSize
      , Hs.Storable
      , Hs.WriteRaw
      ]

    ptrInsts :: Set Hs.TypeClass
    ptrInsts = Set.fromList [
        Hs.Eq
      , Hs.Ord
      , Hs.ReadRaw
      , Hs.Show
      , Hs.StaticSize
      , Hs.Storable
      , Hs.WriteRaw
      ]

    cArrayInsts :: Set Hs.TypeClass
    cArrayInsts = Set.fromList [
        Hs.Eq
      , Hs.ReadRaw
      , Hs.Show
      , Hs.StaticSize
      , Hs.Storable
      , Hs.WriteRaw
      ]

    arrayInsts :: Set Hs.TypeClass
    arrayInsts = Set.fromList [
        Hs.Eq
      , Hs.Show
      ]

    hsTypeSpecInsts :: BindingSpec.HsTypeSpec -> Set Hs.TypeClass
    hsTypeSpecInsts hsTypeSpec = Set.fromAscList [
        cls
      | (cls, BindingSpec.Require{}) <-
           Map.toAscList (BindingSpec.hsTypeSpecInstances hsTypeSpec)
      ]

{-------------------------------------------------------------------------------
  Declarations
------------------------------------------------------------------------------}

-- TODO: Take DeclSpec into account
generateDecs ::
     State.MonadState InstanceMap m
  => TranslationConfig
  -> HaddockConfig
  -> Hs.ModuleName
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
      C.DeclGlobal ty ->
        State.get >>= \instsMap ->
          pure $ withCategory BGlobal $
            global opts haddockConfig moduleName instsMap info ty spec
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
     (SNatI n, State.MonadState InstanceMap m)
  => TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.Struct
  -> C.DeclSpec
  -> Vec n C.StructField
  -> m [Hs.Decl]
structDecs opts haddockConfig info struct spec fields = do
    (insts, decls) <- aux <$> State.get
    State.modify' $ Map.insert structName insts
    pure decls
  where
    structName :: Hs.Name Hs.NsTypeConstr
    structName = C.nameHs (C.declId info)

    structFields :: Vec n Hs.Field
    structFields = flip Vec.map fields $ \f -> Hs.Field {
        fieldName    = C.nameHs (C.fieldName (C.structFieldInfo f))
      , fieldType    = typ (C.structFieldType f)
      , fieldOrigin  = Origin.StructField f
      , fieldComment = generateHaddocksWithFieldInfo haddockConfig info (C.structFieldInfo f)
      }

    candidateInsts :: Set Hs.TypeClass
    candidateInsts = Set.union (Set.singleton Hs.Storable) $
      Set.fromList (snd <$> translationDeriveStruct opts)

    -- everything in aux is state-dependent
    aux :: InstanceMap -> (Set Hs.TypeClass, [Hs.Decl])
    aux instanceMap = (insts,) $
        structDecl : storableDecl ++ optDecls ++ hasFlamDecl ++
        concatMap (structFieldDecls structName) (C.structFields struct)
        -- TODO: generate zero-copy bindings for the FLAM field. See issue
        -- #1286.
      where
        insts :: Set Hs.TypeClass
        insts = getInstances instanceMap structName candidateInsts $
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
                                               (typ (C.structFieldType flam))
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
    fieldType = typ (C.structFieldType f)

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
     State.MonadState InstanceMap m
  => C.NameKind
  -> HaddockConfig
  -> C.DeclInfo
  -> C.DeclSpec
  -> m [Hs.Decl]
opaqueDecs cNameKind haddockConfig info spec = do
    State.modify' $ Map.insert name Set.empty
    return [decl]
  where
    name :: Hs.Name Hs.NsTypeConstr
    name = C.nameHs (C.declId info)

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
     State.MonadState InstanceMap m
  => HaddockConfig
  -> C.DeclInfo
  -> C.Union
  -> C.DeclSpec
  -> m [Hs.Decl]
unionDecs haddockConfig info union spec = do
    decls <- aux <$> State.get
    State.modify' $ Map.insert newtypeName insts
    pure decls
  where
    newtypeName :: Hs.Name Hs.NsTypeConstr
    newtypeName = C.nameHs (C.declId info)

    insts :: Set Hs.TypeClass
    insts = Set.singleton Hs.Storable

    hsNewtype :: Hs.Newtype
    hsNewtype = Hs.Newtype {
        newtypeName      = newtypeName
      , newtypeConstr    = C.newtypeConstr (C.unionNames union)
      , newtypeInstances = insts

      , newtypeField = Hs.Field {
            fieldName    = C.newtypeField (C.unionNames union)
          , fieldType    = Hs.HsByteArray
          , fieldOrigin  = Origin.GeneratedField
          , fieldComment = Nothing
          }
      , newtypeOrigin = Origin.Decl{
            declInfo = info
          , declKind = Origin.Union union
          , declSpec = spec
          }
      , newtypeComment = generateHaddocksWithInfo haddockConfig info
      }

    newtypeDecl :: Hs.Decl
    newtypeDecl = Hs.DeclNewtype hsNewtype

    storableDecl :: Hs.Decl
    storableDecl =
      Hs.DeclDeriveInstance
        Hs.DeriveInstance {
          deriveInstanceStrategy = Hs.DeriveVia sba
        , deriveInstanceClass    = Hs.Storable
        , deriveInstanceName     = newtypeName
        , deriveInstanceComment  = Nothing
        }

    sba :: Hs.HsType
    sba =
      HsSizedByteArray
        (fromIntegral (C.unionSizeof union))
        (fromIntegral (C.unionAlignment union))

    -- everything in aux is state-dependent
    aux :: InstanceMap -> [Hs.Decl]
    aux instanceMap =
        newtypeDecl : storableDecl : accessorDecls ++
        concatMap (unionFieldDecls newtypeName) (C.unionFields union)
      where
        accessorDecls :: [Hs.Decl]
        accessorDecls = concatMap getAccessorDecls (C.unionFields union)

        -- TODO: Should the name mangler take care of the "get" and "set" prefixes?
        getAccessorDecls :: C.UnionField -> [Hs.Decl]
        getAccessorDecls C.UnionField{..} =
          let hsType = typ unionFieldType
              fInsts = getInstances instanceMap newtypeName insts [hsType]
              getterName = "get_" <> C.nameHs (C.fieldName unionFieldInfo)
              setterName = "set_" <> C.nameHs (C.fieldName unionFieldInfo)
              commentRefName name = Just
                HsDoc.Comment {
                  commentTitle      = Nothing
                , commentOrigin     = Nothing
                , commentLocation   = Nothing
                , commentHeaderInfo = Nothing
                , commentChildren   = [ HsDoc.Paragraph
                                        [ HsDoc.Bold [HsDoc.TextContent "See:"]
                                        , HsDoc.Identifier name
                                        ]
                                      ]
                }
          in  if Hs.Storable `Set.notMember` fInsts
                then []
                else
                  [ Hs.DeclUnionGetter
                      Hs.UnionGetter {
                        unionGetterName    = getterName
                      , unionGetterType    = hsType
                      , unionGetterConstr  = newtypeName
                      , unionGetterComment = generateHaddocksWithFieldInfo haddockConfig info unionFieldInfo
                                          <> commentRefName (Hs.getName setterName)
                      }
                  , Hs.DeclUnionSetter
                      Hs.UnionSetter {
                        unionSetterName    = setterName
                      , unionSetterType    = hsType
                      , unionSetterConstr  = newtypeName
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
    fieldType = typ (C.unionFieldType f)

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
     State.MonadState InstanceMap m
  => TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.Enum
  -> C.DeclSpec
  -> m [Hs.Decl]
enumDecs opts haddockConfig info e spec = do
    State.modify' $ Map.insert newtypeName insts
    pure $
      newtypeDecl : storableDecl : optDecls ++ cEnumInstanceDecls ++ valueDecls
  where
    newtypeName :: Hs.Name Hs.NsTypeConstr
    newtypeName = C.nameHs (C.declId info)

    newtypeConstr :: Hs.Name Hs.NsConstr
    newtypeConstr = C.newtypeConstr (C.enumNames e)

    newtypeField :: Hs.Field
    newtypeField = Hs.Field {
        fieldName    = C.newtypeField (C.enumNames e)
      , fieldType    = typ (C.enumType e)
      , fieldOrigin  = Origin.GeneratedField
      , fieldComment = Nothing
      }

    insts :: Set Hs.TypeClass
    insts = Set.union (Set.fromList [Hs.Show, Hs.Read, Hs.Storable]) $
      Set.fromList (snd <$> translationDeriveEnum opts)

    hsNewtype :: Hs.Newtype
    hsNewtype = Hs.Newtype {
        newtypeName      = newtypeName
      , newtypeConstr    = newtypeConstr
      , newtypeField     = newtypeField
      , newtypeInstances = insts
      , newtypeOrigin    = Origin.Decl{
            declInfo = info
          , declKind = Origin.Enum e
          , declSpec = spec
          }
      , newtypeComment = generateHaddocksWithInfo haddockConfig info
      }

    newtypeDecl :: Hs.Decl
    newtypeDecl = Hs.DeclNewtype hsNewtype

    hsStruct :: Hs.Struct (S Z)
    hsStruct = Hs.Struct {
        structName      = newtypeName
      , structConstr    = newtypeConstr
      , structFields    = Vec.singleton newtypeField
      , structInstances = insts
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
            deriveInstanceName     = newtypeName
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
          , patSynType    = newtypeName
          , patSynConstr  = newtypeConstr
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
          fTyp = Hs.fieldType newtypeField
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
     State.MonadState InstanceMap m
  => TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.Typedef
  -> C.DeclSpec
  -> m [Hs.Decl]
typedefDecs opts haddockConfig info typedef spec = do
    (insts, decls) <- aux <$> State.get
    State.modify' $ Map.insert newtypeName insts
    pure decls
  where
    newtypeName :: Hs.Name Hs.NsTypeConstr
    newtypeName = C.nameHs (C.declId info)

    newtypeField :: Hs.Field
    newtypeField = Hs.Field {
        fieldName    = C.newtypeField (C.typedefNames typedef)
      , fieldType    = typ (C.typedefType typedef)
      , fieldOrigin  = Origin.GeneratedField
      , fieldComment = Nothing
      }

    candidateInsts :: Set Hs.TypeClass
    candidateInsts = Set.unions
                   [ Set.singleton Hs.Storable
                   , Set.fromList (snd <$> translationDeriveTypedef opts)
                   ]

    newtypeWrapper :: [Hs.Decl]
    newtypeWrapper =
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
        t@(C.TypeFun args res)
          | not (any hasUnsupportedType (res:args)) ->
            let newtypeNameTo   = "to" <> coerce newtypeName
                newtypeNameFrom = "from" <> coerce newtypeName

            in [ Hs.DeclForeignImport Hs.ForeignImportDecl
                 { foreignImportName       = newtypeNameTo
                 , foreignImportResultType = NormalResultType $ HsIO $ HsFunPtr $ HsTypRef newtypeName
                 , foreignImportParameters = [wrapperParam (HsTypRef newtypeName)]
                 , foreignImportOrigName   = "wrapper"
                 , foreignImportCallConv   = CallConvGhcCCall ImportAsValue
                 , foreignImportOrigin     = Origin.ToFunPtr t
                 , foreignImportComment    = Nothing
                 , foreignImportSafety     = SHs.Safe
                 }
               , Hs.DeclForeignImport Hs.ForeignImportDecl
                 { foreignImportName       = newtypeNameFrom
                 , foreignImportResultType = NormalResultType $ HsTypRef newtypeName
                 , foreignImportParameters = [wrapperParam (HsFunPtr $ HsTypRef newtypeName)]
                 , foreignImportOrigName   = "dynamic"
                 , foreignImportCallConv   = CallConvGhcCCall ImportAsValue
                 , foreignImportOrigin     = Origin.FromFunPtr t
                 , foreignImportComment    = Nothing
                 , foreignImportSafety     = SHs.Safe
                 }
               , Hs.DeclDefineInstance $ Hs.DefineInstance
                 { defineInstanceDeclarations = Hs.InstanceToFunPtr
                   Hs.ToFunPtrInstance
                   { toFunPtrInstanceType = HsTypRef newtypeName
                   , toFunPtrInstanceBody = newtypeNameTo
                   }
                 , defineInstanceComment = Nothing
                 }
               , Hs.DeclDefineInstance $ Hs.DefineInstance
                 { defineInstanceDeclarations = Hs.InstanceFromFunPtr
                   Hs.FromFunPtrInstance
                   { fromFunPtrInstanceType = HsTypRef newtypeName
                   , fromFunPtrInstanceBody = newtypeNameFrom
                   }
                 , defineInstanceComment = Nothing
                 }
               ]
        _ -> []

    -- everything in aux is state-dependent
    aux :: InstanceMap -> (Set Hs.TypeClass, [Hs.Decl])
    aux instanceMap = (insts,) $
        (newtypeDecl : newtypeWrapper) ++ storableDecl ++ optDecls ++
        typedefFieldDecls hsNewtype
      where
        insts :: Set Hs.TypeClass
        insts =
          getInstances
            instanceMap
            newtypeName
            candidateInsts
            [Hs.fieldType newtypeField]

        hsNewtype :: Hs.Newtype
        hsNewtype = Hs.Newtype {
            newtypeName      = newtypeName
          , newtypeConstr    = C.newtypeConstr (C.typedefNames typedef)
          , newtypeField     = newtypeField
          , newtypeInstances = insts
          , newtypeOrigin    = Origin.Decl{
                declInfo = info
              , declKind = Origin.Typedef typedef
              , declSpec = spec
              }
          , newtypeComment = generateHaddocksWithInfo haddockConfig info
          }

        newtypeDecl :: Hs.Decl
        newtypeDecl = Hs.DeclNewtype hsNewtype

        storableDecl :: [Hs.Decl]
        storableDecl
          | Hs.Storable `Set.notMember` insts = []
          | otherwise = singleton $
              Hs.DeclDeriveInstance
                Hs.DeriveInstance {
                  deriveInstanceStrategy = Hs.DeriveNewtype
                , deriveInstanceClass    = Hs.Storable
                , deriveInstanceName     = newtypeName
                , deriveInstanceComment  = Nothing
                }

        optDecls :: [Hs.Decl]
        optDecls = [
            Hs.DeclDeriveInstance
              Hs.DeriveInstance {
                deriveInstanceStrategy = strat
              , deriveInstanceClass    = clss
              , deriveInstanceName     = newtypeName
              , deriveInstanceComment  = Nothing
              }
          | (strat, clss) <- translationDeriveTypedef opts
          , clss `Set.member` insts
          ]

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
     State.MonadState InstanceMap m
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
     State.MonadState InstanceMap m
  => TranslationConfig
  -> HaddockConfig
  -> C.DeclInfo
  -> C.CheckedMacroType
  -> C.DeclSpec
  -> m [Hs.Decl]
macroDecsTypedef opts haddockConfig info macroType spec = do
    (insts, decls) <- aux (C.macroType macroType) <$> State.get
    State.modify' $ Map.insert newtypeName insts
    pure $ decls
  where
    newtypeName :: Hs.Name Hs.NsTypeConstr
    newtypeName = C.nameHs (C.declId info)

    candidateInsts :: Set Hs.TypeClass
    candidateInsts = Set.union (Set.singleton Hs.Storable) $
      Set.fromList (snd <$> translationDeriveTypedef opts)

    -- everything in aux is state-dependent
    aux :: C.Type -> InstanceMap -> (Set Hs.TypeClass, [Hs.Decl])
    aux ty instanceMap = (insts,) $
        newtypeDecl : storableDecl ++ optDecls
      where
        fieldType :: HsType
        fieldType = typ ty

        insts :: Set Hs.TypeClass
        insts = getInstances instanceMap newtypeName candidateInsts [fieldType]

        hsNewtype :: Hs.Newtype
        hsNewtype = Hs.Newtype {
            newtypeName      = newtypeName
          , newtypeConstr    = C.newtypeConstr (C.macroTypeNames macroType)
          , newtypeInstances = insts

          , newtypeField = Hs.Field {
                fieldName    = C.newtypeField (C.macroTypeNames macroType)
              , fieldType    = fieldType
              , fieldOrigin  = Origin.GeneratedField
              , fieldComment = Nothing
              }
          , newtypeOrigin = Origin.Decl {
                declInfo = info
              , declKind = Origin.Macro macroType
              , declSpec = spec
              }
          , newtypeComment = generateHaddocksWithInfo haddockConfig info
          }

        newtypeDecl :: Hs.Decl
        newtypeDecl = Hs.DeclNewtype hsNewtype

        storableDecl :: [Hs.Decl]
        storableDecl
          | Hs.Storable `Set.notMember` insts = []
          | otherwise = singleton $
              Hs.DeclDeriveInstance
                Hs.DeriveInstance {
                  deriveInstanceStrategy = Hs.DeriveNewtype
                , deriveInstanceClass    = Hs.Storable
                , deriveInstanceName     = newtypeName
                , deriveInstanceComment  = Nothing
                }

        optDecls :: [Hs.Decl]
        optDecls = [
            Hs.DeclDeriveInstance
              Hs.DeriveInstance {
                deriveInstanceStrategy = strat
              , deriveInstanceClass    = clss
              , deriveInstanceName     = newtypeName
              , deriveInstanceComment  = Nothing
              }
          | (strat, clss) <- translationDeriveTypedef opts
          , clss `Set.member` insts
          ]

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data TypeContext =
    CTop     -- ^ Anything else
  | CFunArg  -- ^ Function argument
  | CFunRes  -- ^ Function result
  | CPtrArg  -- ^ Pointer argument
  deriving stock (Show)

typ :: HasCallStack => C.Type -> Hs.HsType
typ = typ' CTop

typ' :: HasCallStack => TypeContext -> C.Type -> Hs.HsType
typ' ctx = go ctx
  where
    go :: TypeContext -> C.Type -> Hs.HsType
    go _ (C.TypeTypedef (C.TypedefRegular name _)) =
        Hs.HsTypRef (C.nameHs name)
    go c (C.TypeTypedef (C.TypedefSquashed _name ty)) =
        go c ty
    go _ (C.TypeStruct name _origin) =
        Hs.HsTypRef (C.nameHs name)
    go _ (C.TypeUnion name _origin) =
        Hs.HsTypRef (C.nameHs name)
    go _ (C.TypeEnum name _origin) =
        Hs.HsTypRef (C.nameHs name)
    go _ (C.TypeMacroTypedef name _origin) =
        Hs.HsTypRef (C.nameHs name)
    go c C.TypeVoid =
        Hs.HsPrimType (goVoid c)
    go _ (C.TypePrim p) =
        Hs.HsPrimType (goPrim p)
    go _ (C.TypePointer t)
      -- Use a 'FunPtr' if the type is a function type. We inspect the
      -- /canonical/ type because we want to see through typedefs and type
      -- qualifiers like @const@.
      | C.isCanonicalTypeFunction t
      = Hs.HsFunPtr (go CPtrArg t)
      | otherwise
      = Hs.HsPtr (go CPtrArg t)
    go _ (C.TypeConstArray n ty) =
        Hs.HsConstArray n $ go CTop ty
    go _ (C.TypeIncompleteArray ty) =
        Hs.HsIncompleteArray $ go CTop ty
    go _ (C.TypeFun xs y) =
        foldr (\x res -> Hs.HsFun (go CFunArg x) res) (Hs.HsIO (go CFunRes y)) xs
    go _ (C.TypeBlock ty) =
        HsBlock $ go CTop ty
    go _ (C.TypeExtBinding ext) =
        Hs.HsExtBinding (C.extHsRef ext) (C.extCSpec ext) (C.extHsSpec ext)
    go c (C.TypeQualified C.TypeQualifierConst ty) =
        go c ty
    go _ (C.TypeComplex p) =
        Hs.HsComplexType (goPrim p)

    goPrim :: C.PrimType -> HsPrimType
    goPrim C.PrimBool           = HsPrimCBool
    goPrim (C.PrimIntegral i s) = integralType i s
    goPrim (C.PrimFloating f)   = floatingType f
    goPrim C.PrimPtrDiff        = HsPrimCPtrDiff
    goPrim C.PrimSize           = HsPrimCSize
    goPrim (C.PrimChar sign)    =
        case sign of
          C.PrimSignImplicit _          -> HsPrimCChar
          C.PrimSignExplicit C.Signed   -> HsPrimCSChar
          C.PrimSignExplicit C.Unsigned -> HsPrimCUChar

    goVoid :: TypeContext -> HsPrimType
    goVoid CFunRes = HsPrimUnit
    goVoid CPtrArg = HsPrimVoid
    goVoid c       = panicPure $ "unexpected type void in context " ++ show c

integralType :: C.PrimIntType -> C.PrimSign -> HsPrimType
integralType C.PrimInt      C.Signed   = HsPrimCInt
integralType C.PrimInt      C.Unsigned = HsPrimCUInt
integralType C.PrimShort    C.Signed   = HsPrimCShort
integralType C.PrimShort    C.Unsigned = HsPrimCUShort
integralType C.PrimLong     C.Signed   = HsPrimCLong
integralType C.PrimLong     C.Unsigned = HsPrimCULong
integralType C.PrimLongLong C.Signed   = HsPrimCLLong
integralType C.PrimLongLong C.Unsigned = HsPrimCULLong

floatingType :: C.PrimFloatType -> HsPrimType
floatingType = \case
  C.PrimFloat  -> HsPrimCFloat
  C.PrimDouble -> HsPrimCDouble

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
hasUnsupportedType = C.getCanonicalType >>> \case
    C.TypeStruct {}          -> True
    C.TypeUnion {}           -> True
    C.TypeComplex {}         -> True
    C.TypeConstArray {}      -> True
    C.TypeIncompleteArray {} -> True
    C.TypePrim {}            -> False
    C.TypeEnum {}            -> False
    C.TypeMacroTypedef {}    -> False
    C.TypePointer {}         -> False
    C.TypeFun {}             -> False
    C.TypeVoid               -> False
    C.TypeBlock {}           -> False
    C.TypeExtBinding {}      -> False

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
unwrapType (WrapType ty)   = ty
unwrapType (HeapType ty)   = C.TypePointer ty
unwrapType (CAType _ _ ty) = C.TypePointer ty
unwrapType (AType _ ty)    = C.TypePointer ty

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
  let resType = typ' CFunRes $ unwrapOrigType res
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

    goA' :: Env ctx' WrappedType -> Env ctx' (Idx ctx) -> [Idx ctx] -> SHs.SExpr ctx
    goA' EmptyEnv    EmptyEnv  zs
        = shsApps (SHs.EGlobal SHs.CAPI_allocaAndPeek)
          [ SHs.ELam "z" $ shsApps (SHs.EFree loName) (map SHs.EBound (fmap IS zs ++ [IZ]))
          ]

    goA' (tys :> ty) (xs :> x) zs = case ty of
        HeapType {} -> shsApps (SHs.EGlobal SHs.CAPI_with)
            [ SHs.EBound x
            , SHs.ELam "y" $ goA' tys (IS <$> xs) (IZ : fmap IS zs)
            ]

        CAType {} -> shsApps (SHs.EGlobal SHs.ConstantArray_withPtr)
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goA' tys (IS <$> xs) (IZ : fmap IS zs)
            ]

        AType {} -> shsApps (SHs.EGlobal SHs.IncompleteArray_withPtr)
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goA' tys (IS <$> xs) (IZ : fmap IS zs)
            ]

        WrapType {} ->
            goA' tys xs (x : zs)

    -- wrapper for non-fancy result.
    goB :: Env ctx WrappedType -> [WrappedType] -> SHs.SExpr ctx
    goB env []     = goB' env (tabulateEnv (sizeEnv env) id) []
    goB env (x:xs) = SHs.ELam "x" $ goB (env :> x) xs

    goB' :: Env ctx' WrappedType -> Env ctx' (Idx ctx) -> [Idx ctx] -> SHs.SExpr ctx
    goB' EmptyEnv    EmptyEnv  zs
        = shsApps (SHs.EFree loName) (map SHs.EBound zs)

    goB' (tys :> ty) (xs :> x) zs = case ty of
        HeapType {} -> shsApps (SHs.EGlobal SHs.CAPI_with)
          [ SHs.EBound x
          , SHs.ELam "y" $ goB' tys (IS <$> xs) (IZ : fmap IS zs)
          ]

        CAType {} -> shsApps (SHs.EGlobal SHs.ConstantArray_withPtr)
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goB' tys (IS <$> xs) (IZ : fmap IS zs)
            ]

        AType {} -> shsApps (SHs.EGlobal SHs.IncompleteArray_withPtr)
            [ SHs.EBound x
            , SHs.ELam "ptr" $ goB' tys (IS <$> xs) (IZ : fmap IS zs)
            ]

        WrapType {} ->
            goB' tys xs (x : zs)

shsApps :: SHs.SExpr ctx -> [SHs.SExpr ctx] -> SHs.SExpr ctx
shsApps = foldl' SHs.EApp

wrapperParam :: HsType -> Hs.FunctionParameter
wrapperParam hsType = Hs.FunctionParameter
  { functionParameterName    = Nothing
  , functionParameterType    = hsType
  , functionParameterComment = Nothing
  }
-- | Generate a unique name for FFI stubs based on function signature
genNameFromArgs :: [C.Type] -> C.Type -> String -> Hs.Name 'Hs.NsVar
genNameFromArgs args' res' suffix =
  Hs.Name $ T.pack $ "funPtr_" ++ typeHash args' res' ++ "_" ++ suffix
  where
    typeHash :: [C.Type] -> C.Type -> String
    typeHash args res = B.unpack $ B.take 8 $ B16.encode $
      hash $ B.pack $ show (args, res)

functionDecs ::
     HasCallStack
  => SHs.Safety
  -> TranslationConfig
  -> HaddockConfig
  -> Hs.ModuleName
  -> C.DeclInfo
  -> C.Function
  -> C.DeclSpec
  -> [Hs.Decl]
functionDecs safety opts haddockConfig moduleName info f _spec =
    funDecl : [
        hsWrapperDeclFunction highlevelName importName res wrappedArgTypes wrapParsedArgs f mbWrapComment
      | areFancy
      ]
  where
    areFancy = anyFancy (res : wrappedArgTypes)
    funDecl :: Hs.Decl
    funDecl = Hs.DeclForeignImport $ Hs.ForeignImportDecl
        { foreignImportName       = importName
        , foreignImportResultType = resType
        , foreignImportParameters = if areFancy then ffiParams else ffiParsedArgs
        , foreignImportOrigName   = T.pack wrapperName
        , foreignImportCallConv   = CallConvUserlandCAPI userlandCapiWrapper
        , foreignImportOrigin     = Origin.Function f
        , foreignImportComment    = (if areFancy then Just nonFancyComment else mbFFIComment) <> ioComment
        , foreignImportSafety     = safety
        }

    userlandCapiWrapper :: UserlandCapiWrapper
    userlandCapiWrapper = UserlandCapiWrapper {
          capiWrapperDefinition =
            PC.prettyDecl (wrapperDecl innerName wrapperName res wrappedArgTypes) ""
        , capiWrapperImport =
            getMainHashIncludeArg info
        }

    highlevelName = C.nameHs (C.declId info)
    importName
        | areFancy  = highlevelName <> "_wrapper" -- TODO: Add to NameMangler pass
        | otherwise = highlevelName

    res = wrapType $ C.functionRes f

    -- Parameters for FFI import
    ffiParams = [ Hs.FunctionParameter
                   { functionParameterName    = fmap C.nameHs mbName
                   , functionParameterType    = typ' CFunArg (unwrapType (wrapType ty))
                   , functionParameterComment = Nothing
                   }
                | (mbName, ty) <- C.functionArgs f
                ]

    (mbFFIComment, ffiParsedArgs) =
      generateHaddocksWithInfoParams haddockConfig info ffiParams

    -- Parameters for wrapper decl
    wrapperParams = [ Hs.FunctionParameter
                     { functionParameterName    = fmap C.nameHs mbName
                     , functionParameterType    = typ' CFunArg (unwrapOrigType (wrapType ty))
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

    resType :: ResultType HsType
    resType =
      case res of
        HeapType {} -> HeapResultType $ typ' CFunRes $ unwrapType res

        WrapType {} -> NormalResultType $ hsIO $ typ' CFunRes $ unwrapType res

        CAType {} ->
            panicPure "ConstantArray cannot occur as a result type"

        AType {} ->
            panicPure "Array cannot occur as a result type"

    -- | A comment to put on wrapper functions
    --
    -- "Pointer-based API for '<function>'"
    nonFancyComment :: HsDoc.Comment
    nonFancyComment =
      HsDoc.Comment {
        HsDoc.commentTitle      = Just
          [ HsDoc.TextContent "Pointer-based API for"
          , HsDoc.Identifier (Hs.getName highlevelName)
          ]
      , HsDoc.commentOrigin     = Nothing
      , HsDoc.commentLocation   = Nothing
      , HsDoc.commentHeaderInfo = Nothing
      , HsDoc.commentChildren   = []

      }

    -- | Decide based on the function attributes whether to include 'IO' in the
    -- result type of the foreign import. See the documentation on
    -- 'C.FunctionPurity'.
    --
    -- An exception to the rules: the foreign import function returns @void@
    -- when @res@ is a heap type, in which case a @const@ or @pure@ attribute
    -- does not make much sense, and so we just return the result in 'IO'.
    hsIO :: Hs.HsType -> Hs.HsType
    -- | C-pure functions can be safely encapsulated using 'unsafePerformIO' to
    -- create a Haskell-pure functions. We include a comment in the generated
    -- bindings to this effect.
    ioComment :: Maybe HsDoc.Comment
    (hsIO, ioComment) = case C.functionPurity (C.functionAttrs f) of
        C.HaskellPureFunction -> (id  , Nothing)
        C.CPureFunction       -> (HsIO, Just pureComment)
        C.ImpureFunction      -> (HsIO, Nothing)

    -- | A comment to put on bindings for C-pure functions
    --
    -- "Marked @__attribute((pure))__@"
    pureComment :: HsDoc.Comment
    pureComment =
      HsDoc.Comment {
        HsDoc.commentTitle      = Nothing
      , HsDoc.commentOrigin     = Nothing
      , HsDoc.commentLocation   = Nothing
      , HsDoc.commentHeaderInfo = Nothing
      , HsDoc.commentChildren   =
          [ HsDoc.Paragraph
            [ HsDoc.TextContent "Marked"
            , HsDoc.Monospace
              [ HsDoc.Bold
                [ HsDoc.TextContent "attribute((pure))" ]
              ]
            ]
          ]
      }

    -- Generation of C wrapper for userland-capi.
    innerName :: String
    innerName = T.unpack (C.getName . C.nameC . C.declId $ info)

    wrapperName :: String
    wrapperName = unUniqueSymbolId $
      getUniqueSymbolId (translationUniqueId opts) moduleName (Just safety) innerName

-- | Generate ToFunPtr/FromFunPtr instances for nested function pointer types
--
-- This function analyses the function declaration arguments and return types,
-- and for each function pointer argument/return type containing at least one
-- non-orphan type, generates the FFI wrapper and dynamic stubs along with
-- the respective ToFunPtr and FromFunPtr instances.
--
-- These instances are placed in the main module to avoid orphan instances.
functionTypeFFIStubsAndFunPtrInstances ::
     [C.Type]
  -> C.Type
  -> [Hs.Decl]
functionTypeFFIStubsAndFunPtrInstances args res =
  let ft = (C.TypeFun args res)
      tf = C.TypePointer ft
      tfHsType    = typ tf
      ftHsType    = typ ft
      ffiStubTo   = genNameFromArgs args res "to"
      ffiStubFrom = genNameFromArgs args res "from"
   in [ Hs.DeclForeignImport Hs.ForeignImportDecl
        { foreignImportName       = ffiStubTo
        , foreignImportResultType = NormalResultType $ HsIO $ tfHsType
        , foreignImportParameters = [wrapperParam ftHsType]
        , foreignImportOrigName   = "wrapper"
        , foreignImportCallConv   = CallConvGhcCCall ImportAsValue
        , foreignImportOrigin     = Origin.ToFunPtr tf
        , foreignImportComment    = Nothing
        , foreignImportSafety     = SHs.Safe
        }
      , Hs.DeclForeignImport Hs.ForeignImportDecl
        { foreignImportName       = ffiStubFrom
        , foreignImportResultType = NormalResultType ftHsType
        , foreignImportParameters = [wrapperParam tfHsType]
        , foreignImportOrigName   = "dynamic"
        , foreignImportCallConv   = CallConvGhcCCall ImportAsValue
        , foreignImportOrigin     = Origin.FromFunPtr tf
        , foreignImportComment    = Nothing
        , foreignImportSafety     = SHs.Safe
        }
      , Hs.DeclDefineInstance $ Hs.DefineInstance
        { defineInstanceDeclarations = Hs.InstanceToFunPtr
          Hs.ToFunPtrInstance
          { toFunPtrInstanceType = ftHsType
          , toFunPtrInstanceBody = ffiStubTo
          }
        , defineInstanceComment = Nothing
        }
      , Hs.DeclDefineInstance $ Hs.DefineInstance
        { defineInstanceDeclarations = Hs.InstanceFromFunPtr
          Hs.FromFunPtrInstance
          { fromFunPtrInstanceType = ftHsType
          , fromFunPtrInstanceBody = ffiStubFrom
          }
        , defineInstanceComment = Nothing
        }
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
  -> Hs.ModuleName
  -> InstanceMap
  -> C.DeclInfo
  -> C.Type
  -> C.DeclSpec
  -> [Hs.Decl]
global opts haddockConfig moduleName instsMap info ty _spec
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
    getConstGetterOfType t = constGetter (typ t) instsMap info pureStubName

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
  -> InstanceMap
  -> C.DeclInfo
  -> Hs.Name Hs.NsVar
  -> [Hs.Decl]
constGetter ty instsMap info pureStubName = concat [
          [ Hs.DeclSimple $ SHs.DPragma (SHs.NOINLINE getterName)
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
              getInstances instsMap "unused" (Set.singleton Hs.Storable) [ty]
        ]
  where
    -- *** Getter ***
    --
    -- The "getter" peeks the value from the pointer
    getterDecl :: Hs.Decl
    getterDecl = Hs.DeclSimple $ SHs.DVar $ SHs.Var {
          varName    = getterName
        , varType    = getterType
        , varExpr    = getterExpr
        , varComment = Nothing
        }

    getterName = C.nameHs (C.declId info)
    getterType = SHs.translateType ty
    getterExpr = SHs.EGlobal SHs.IO_unsafePerformIO
                `SHs.EApp` (SHs.EGlobal SHs.Storable_peek
                `SHs.EApp` SHs.EFree pureStubName)

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
  -> Hs.ModuleName
  -> C.DeclInfo -- ^ The given declaration
  -> C.Type -- ^ The type of the given declaration
  -> C.DeclSpec
  -> ( [Hs.Decl]
     , Hs.Name 'Hs.NsVar
     )
addressStubDecs opts haddockConfig moduleName info ty _spec =
    (foreignImport : runnerDecls, runnerName)
  where
    -- *** Stub (impure) ***

    -- | We reuse the mangled stub name here, since the import is supposed to be
    -- internal. Users should use functioned identified by @runnerName@ instead,
    -- which does not include 'IO' in the return type.
    stubImportName :: Hs.Name 'Hs.NsVar
    stubImportName = Hs.Name $ T.pack stubNameMangled

    stubImportType :: ResultType HsType
    stubImportType = NormalResultType $ HsIO $ typ stubType

    stubNameMangled :: String
    stubNameMangled = unUniqueSymbolId $
        getUniqueSymbolId (translationUniqueId opts) moduleName Nothing stubName

    stubName :: String
    stubName = "get_" ++ varName ++ "_ptr"

    varName :: String
    varName = T.unpack (C.getName . C.nameC . C.declId $ info)

    stubType :: C.Type
    stubType = C.TypePointer ty

    prettyStub :: String
    prettyStub = concat [
          "/* ", stubName, " */\n"
        , PC.prettyDecl stubDecl ""
        ]

    stubDecl :: PC.Decl
    stubDecl =
        PC.withArgs [] $ \args' ->
          PC.FunDefn stubNameMangled stubType C.HaskellPureFunction args'
            [PC.Return $ PC.Address $ PC.NamedVar varName]

    userlandCapiWrapper :: UserlandCapiWrapper
    userlandCapiWrapper = UserlandCapiWrapper {
          capiWrapperDefinition = prettyStub
        , capiWrapperImport = getMainHashIncludeArg info
        }

    mbComment = generateHaddocksWithInfo haddockConfig info

    foreignImport :: Hs.Decl
    foreignImport = Hs.DeclForeignImport $ Hs.ForeignImportDecl
        { foreignImportName     = stubImportName
        , foreignImportParameters = []
        , foreignImportResultType = stubImportType
        , foreignImportOrigName = T.pack stubNameMangled
        , foreignImportCallConv = CallConvUserlandCAPI userlandCapiWrapper
        , foreignImportOrigin   = Origin.Global ty
        , foreignImportComment  = Nothing
          -- These imports can be unsafe. We're binding to simple address stubs,
          -- so there are no callbacks into Haskell code. Moreover, they are
          -- short running code.
        , foreignImportSafety   = SHs.Unsafe
        }

    -- *** Stub (pure) ***

    runnerDecls :: [Hs.Decl]
    runnerDecls = [
          Hs.DeclSimple $ SHs.DPragma (SHs.NOINLINE runnerName)
        , runnerDecl
        ]

    runnerDecl :: Hs.Decl
    runnerDecl = Hs.DeclSimple $ SHs.DVar $ SHs.Var {
          varName    = runnerName
        , varType    = runnerType
        , varExpr    = runnerExpr
        , varComment = mbComment
        }

    runnerName = Hs.Name $ Hs.getIdentifier (C.nameHsIdent (C.declId info)) <> "_ptr"
    runnerType = SHs.translateType (typ stubType)
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
    hsVarName = C.nameHs (C.declId info)

{-------------------------------------------------------------------------------
  Unique module identifier
-------------------------------------------------------------------------------}

-- | An identifier string used to generate morally module-private but externally
-- visible symbols (e.g., C symbols).
--
-- The unique identifier for a function with identifier @fn@ is generated in the
-- following way:
--
-- @
--   "hs_bindgen_" ++ 'UniqueModuleId' ++ "_" ++ hashOf 'Hs.ModuleName' fn
-- @
--
-- where @hashOf@ computes the hash of its arguments.
--
-- The reason for the @hashOf 'Hs.ModuleName' fn@ rather than just
-- @'Hs.ModuleName' ++ fn@ is that only the first 64 characters may be used to
-- distinguish C names by the linker, and Haskell module names can be quite
-- long.
newtype UniqueSymbolId = UniqueSymbolId { unUniqueSymbolId :: String }
  deriving newtype (Show)

getUniqueSymbolId ::
     UniqueId
  -> Hs.ModuleName
  -> Maybe Safety
  -> String
  -> UniqueSymbolId
getUniqueSymbolId (UniqueId uniqueId) moduleName msafety symbolName =
    UniqueSymbolId $ intercalate "_" components
  where
    components :: [String]
    components =
         [ "hs_bindgen"                   ]
      ++ [ x | let x = sanitize uniqueId, not (null x) ]
      ++ [ getHash moduleName msafety symbolName  ]

    sanitize :: String -> String
    sanitize [] = []
    sanitize (x:xs)
      | isLetter x          = x   : sanitize xs
      | isDigit  x          = x   : sanitize xs
      | x `elem` ['.', '_'] = '_' : sanitize xs
      | otherwise           =       sanitize xs

    -- We use `cryptohash-sha256` to avoid potential dynamic linker problems
    -- (https://github.com/haskell-haskey/xxhash-ffi/issues/4).
    getHash :: Hs.ModuleName -> Maybe Safety -> String -> String
    getHash x y z = B.unpack $ B.take 16 $ B16.encode $
      hash $ getString x y z

    -- We use ByteString to avoid hash changes induced by a change of how Text
    -- is encoded in GHC 9.2.
    getString :: Hs.ModuleName -> Maybe Safety -> String -> ByteString
    getString x y z = B.pack $ Hs.moduleNameToString x <> show y <> z
