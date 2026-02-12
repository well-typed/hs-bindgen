-- | Low-level translation of the C header to a Haskell module
module HsBindgen.Backend.Hs.Translation (
    generateDeclarations
  ) where

import Control.Monad.State qualified as State hiding (MonadState)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vec.Lazy qualified as Vec
import DeBruijn (Add (..), Idx (..), pattern I2)

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
import HsBindgen.Backend.Hs.Translation.ForeignImport qualified as Hs.ForeignImport
import HsBindgen.Backend.Hs.Translation.Function
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.Newtype qualified as Hs
import HsBindgen.Backend.Hs.Translation.State (HsM, TranslationState)
import HsBindgen.Backend.Hs.Translation.State qualified as State
import HsBindgen.Backend.Hs.Translation.Structure
import HsBindgen.Backend.Hs.Translation.ToFromFunPtr qualified as ToFromFunPtr
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Config.FixCandidate qualified as FixCandidate
import HsBindgen.Config.Internal
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.PrettyC qualified as PC

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

generateDeclarations ::
     UniqueId
  -> FieldNamingStrategy
  -> HaddockConfig
  -> BaseModuleName
  -> DeclIndex
  -> C.Sizeofs
  -> [C.Decl Final]
  -> ByCategory_ [Hs.Decl]
generateDeclarations uniqueId fns config name declIndex sizeofs =
    fmap reverse .
      foldl' partitionBindingCategories mempty .
      generateDeclarations' uniqueId fns config name declIndex sizeofs
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
     UniqueId
  -> FieldNamingStrategy
  -> HaddockConfig
  -> BaseModuleName
  -> DeclIndex
  -> C.Sizeofs
  -> [C.Decl Final]
  -> [WithCategory Hs.Decl]
generateDeclarations' uniqueId fns haddockConfig moduleName declIndex sizeofs decs =
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
                   , d <- ToFromFunPtr.forFunction sizeofs (args, res)
                   ]
      hsDecls <- concat <$> mapM (generateDecs uniqueId fns haddockConfig moduleName sizeofs) decs
      pure $ hsDecls ++ fFIStubsAndFunPtrInstances

-- | This function takes a list of all declarations and collects all function
-- pointer callback types, i.e. all function types that exist as either
-- arguments for other functions (nth-order functions), fields of structs,
-- unions and enums.
--
-- This explicitly excludes typedef function pointers (e.g., @typedef void (*F)()@)
-- because these are handled separately by 'typedefFunPtrDecs' in 'generateDecs'.
--
scanAllFunctionPointerTypes :: [C.Decl Final] -> Set (C.Type Final)
scanAllFunctionPointerTypes = foldMap $ \decl ->
    case decl.kind of
      C.DeclStruct struct ->
        foldMap (scanTypeForFunctionPointers . (.typ)) struct.fields
      C.DeclUnion union   ->
        foldMap (scanTypeForFunctionPointers . (.typ)) union.fields
      C.DeclFunction fn ->
        foldMap scanTypeForFunctionPointers (fn.res : map snd fn.args)
      _ ->
        Set.empty
  where
    -- | Recursively scan a type for all function pointers, including nested ones
    scanTypeForFunctionPointers :: C.Type Final -> Set (C.Type Final)
    scanTypeForFunctionPointers ty = case ty of
      -- Use TypePointers pattern to safely match N levels of indirection
      fp@(C.TypePointers _n (C.TypeFun args res)) ->
           Set.singleton fp
        <> foldMap scanTypeForFunctionPointers (res : args)
      C.TypePointers _ t        -> scanTypeForFunctionPointers t
      C.TypeIncompleteArray  t  -> scanTypeForFunctionPointers t
      C.TypeConstArray _ t      -> scanTypeForFunctionPointers t
      C.TypeBlock t             -> scanTypeForFunctionPointers t
      C.TypeQual _ t            -> scanTypeForFunctionPointers t

      -- Don't scan through typedef function pointers; these are handled by
      -- typedefFunPtrDecs (see generateDecs). Scanning would create duplicate
      -- ToFunPtr/FromFunPtr instances. We still scan other typedefs to find
      -- nested function pointers.
      -- When #1520 is addressed this could should be updated.
      C.TypeTypedef (C.Ref _ t) -> case t of
        C.TypePointers _ (C.TypeFun _ _) -> Set.empty
        _                                -> scanTypeForFunctionPointers t
      _                         -> Set.empty

-- | Check if a type is defined in the current module
isDefinedInCurrentModule :: DeclIndex -> C.Type Final -> Bool
isDefinedInCurrentModule declIndex =
    any (isInDeclIndex . snd) . C.depsOfType
  where
    isInDeclIndex :: DeclIdPair -> Bool
    isInDeclIndex declId = isJust $ DeclIndex.lookup declId.cName declIndex

{-------------------------------------------------------------------------------
  Declarations
------------------------------------------------------------------------------}

-- TODO: Take DeclSpec into account
generateDecs ::
     UniqueId
  -> FieldNamingStrategy
  -> HaddockConfig
  -> BaseModuleName
  -> C.Sizeofs
  -> C.Decl Final
  -> HsM [WithCategory Hs.Decl]
generateDecs uniqueId fns haddockConfig moduleName sizeofs (C.Decl info kind spec) =
    case kind of
      C.DeclStruct struct -> withCategoryM CType $
        structDecs supInsts.struct haddockConfig info struct spec
      C.DeclUnion union -> withCategoryM CType $
        unionDecs fns haddockConfig info union spec
      C.DeclEnum enum -> withCategoryM CType $
        enumDecs supInsts.enum fns haddockConfig info enum spec
      C.DeclAnonEnumConstant anonEnumConst -> withCategoryM CType $
        pure $ anonEnumConstantDecs haddockConfig info anonEnumConst
      C.DeclTypedef typedef -> withCategoryM CType $
        -- Deal with typedefs around function pointers (#1380)
        case typedef.typ of
          C.TypePointers n (C.TypeFun args res) ->
            typedefFunPtrDecs supInsts.typedef fns haddockConfig sizeofs info n (args, res) typedef.names spec
          _otherwise ->
            typedefDecs supInsts.typedef haddockConfig sizeofs info Origin.Typedef typedef spec
      C.DeclOpaque -> withCategoryM CType $
        opaqueDecs haddockConfig info spec
      C.DeclFunction function -> do
        let funDeclsWith safety =
              functionDecs safety uniqueId haddockConfig moduleName sizeofs info function spec
            funType = C.TypeFun (map snd function.args) function.res
            -- Declare a function pointer. We can pass this 'FunPtr' to C
            -- functions that take a function pointer of the appropriate type.
            funPtrDecls = fst $
              addressStubDecs uniqueId haddockConfig moduleName sizeofs info funType HaskellId spec
        pure $ withCategory (CTerm CSafe)   (funDeclsWith SHs.Safe)
            ++ withCategory (CTerm CUnsafe) (funDeclsWith SHs.Unsafe)
            ++ withCategory (CTerm CFunPtr)  funPtrDecls
      C.DeclMacro macro -> withCategoryM CType $
        macroDecs supInsts.typedef haddockConfig info macro spec
      C.DeclGlobal ty -> do
        transState <- State.get
        pure $ withCategory (CTerm CGlobal) $
          global uniqueId haddockConfig moduleName transState sizeofs info ty spec
  where
    withCategory :: Category -> [a] -> [WithCategory a]
    withCategory c = map (WithCategory c)

    withCategoryM :: Functor m => Category -> m [a] -> m [WithCategory a]
    withCategoryM c = fmap (withCategory c)

    supInsts :: Inst.SupportedInstances
    supInsts = def

{-------------------------------------------------------------------------------
  Opaque struct and opaque enum
-------------------------------------------------------------------------------}

opaqueDecs ::
     HaddockConfig
  -> C.DeclInfo Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl]
opaqueDecs haddockConfig info spec = do
    State.modify' $ #instanceMap %~ Map.insert name Set.empty
    return [decl]
  where
    name :: Hs.Name Hs.NsTypeConstr
    name = Hs.unsafeHsIdHsName info.id.unsafeHsName

    decl :: Hs.Decl
    decl = Hs.DeclEmpty Hs.EmptyData {
          name   = name
        , comment = mkHaddocks haddockConfig info name
        , origin = Origin.Decl{
              info = info
            , kind = Origin.Opaque info.id.cName.name.kind
            , spec = spec
            }
        }

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

unionDecs ::
     FieldNamingStrategy
  -> HaddockConfig
  -> C.DeclInfo Final
  -> C.Union Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl]
unionDecs fieldNaming haddockConfig info union spec = do
    nt <- newtypeDec
    flip aux nt <$> State.get
  where
    newtypeDec :: HsM Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.unsafeHsIdHsName info.id.unsafeHsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = union.names.constr

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
              name    = union.names.field
            , typ     = Hs.HsByteArray
            , origin  = Origin.GeneratedField
            , comment = Nothing
            }

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin =  Origin.Decl {
              info = info
            , kind = Origin.Union union
            , spec = spec
            }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment = mkHaddocks haddockConfig info newtypeName

        candidateInsts :: Set Inst.TypeClass
        candidateInsts = Set.empty

        knownInsts :: Set Inst.TypeClass
        knownInsts = Set.fromList $ catMaybes [
          -- TODO <https://github.com/well-typed/hs-bindgen/issues/1253>
          -- Should correctly detect 'Inst.HasCBitField' and 'Inst.HasCField'
          -- when bit-fields in unions are supported.
            if null union.fields then Nothing else Just Inst.HasCField
          , if null union.fields then Nothing else Just Inst.HasField
          , Just Inst.ReadRaw
          , Just Inst.StaticSize
          , Just Inst.Storable
          , Just Inst.WriteRaw
          ]

    -- everything in aux is state-dependent
    aux :: TranslationState -> Hs.Newtype -> [Hs.Decl]
    aux transState nt =
        Hs.DeclNewtype nt : marshalDecls ++ accessorDecls ++
        concatMap (unionFieldDecls nt.name) union.fields
      where
        marshalDecls :: [Hs.Decl]
        marshalDecls = [
            Hs.DeclDeriveInstance Hs.DeriveInstance{
                strategy = Hs.DeriveVia sba
              , clss     = Inst.StaticSize
              , name     = nt.name
              , comment  = Nothing
              }
          , Hs.DeclDeriveInstance Hs.DeriveInstance{
                strategy = Hs.DeriveVia sba
              , clss     = Inst.ReadRaw
              , name     = nt.name
              , comment  = Nothing
              }
          , Hs.DeclDeriveInstance Hs.DeriveInstance{
                strategy = Hs.DeriveVia sba
              , clss     = Inst.WriteRaw
              , name     = nt.name
              , comment  = Nothing
              }
          , Hs.DeclDeriveInstance Hs.DeriveInstance{
                strategy = Hs.DeriveVia (HsEquivStorable (Hs.HsTypRef nt.name Nothing))
              , clss     = Inst.Storable
              , name     = nt.name
              , comment  = Nothing
              }
          ]

        sba :: Hs.HsType
        sba =
            HsSizedByteArray
              (fromIntegral union.sizeof)
              (fromIntegral union.alignment)

        accessorDecls :: [Hs.Decl]
        accessorDecls = concatMap getAccessorDecls union.fields

        getAccessorDecls :: C.UnionField Final -> [Hs.Decl]
        getAccessorDecls field =
            if Inst.Storable `Set.notMember` fInsts
              then []
              else [
                  Hs.DeclUnionGetter Hs.UnionGetter{
                      name    = getterName
                    , typ     = hsType
                    , constr  = nt.name
                    , comment = mkHaddocksFieldInfo haddockConfig info field.info
                             <> commentRefName (Hs.getName setterName)
                    }
                , Hs.DeclUnionSetter Hs.UnionSetter{
                      name    = setterName
                    , typ     = hsType
                    , constr  = nt.name
                    , comment = commentRefName (Hs.getName getterName)
                    }
                ]
          where
            hsType     = Type.topLevel field.typ
            fInsts     = Hs.getInstances
                            transState.instanceMap
                            (Just nt.name)
                            (Set.singleton Inst.Storable)
                            [hsType]

            -- TODO <https://github.com/well-typed/hs-bindgen/issues/1504>
            -- This should happen in the name mangler, so that we can deal with
            -- collisions, errors thrown by 'fixCandidate', etc.
            --
            -- With PrefixedFieldNames: field.info.name.hsName already contains the
            -- type prefix (e.g., "dimPayload_dim2"), so we use it directly.
            -- With EnableRecordDot: field.info.name.hsName is just the C field name
            -- (e.g., "dim2"), so we need to add the type name for uniqueness.
            getterName = Hs.unsafeHsIdHsName $ "get_" <> fieldNameWithPrefix
            setterName = Hs.unsafeHsIdHsName $ "set_" <> fieldNameWithPrefix

            -- We ensure that we generate the /same/ getter and setter name
            -- independent of whether record dot syntax is enabled or not.
            fieldNameWithPrefix :: Hs.Identifier
            fieldNameWithPrefix = case fieldNaming of
              PrefixedFieldNames -> field.info.name.hsName
              EnableRecordDot    ->
                let candidate :: Text
                    candidate = Hs.getName nt.name <> "_" <> field.info.name.hsName.text

                    exportedName :: Hs.ExportedName Hs.NsVar
                    exportedName =
                       fromMaybe (panicPure $ "could not construct name for " ++ show candidate) $
                         FixCandidate.fixCandidate
                           FixCandidate.fixCandidateDefault
                           candidate

                in Hs.Identifier exportedName.text

            commentRefName :: Text -> Maybe HsDoc.Comment
            commentRefName name = Just $ HsDoc.paragraph [
                HsDoc.Bold [HsDoc.TextContent "See:"]
              , HsDoc.Identifier name
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
-- > newtype MyUnion = MyUnion { unwrapMyUnion :: ByteArray }
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
unionFieldDecls :: Hs.Name Hs.NsTypeConstr -> C.UnionField Final -> [Hs.Decl]
unionFieldDecls unionName field = [
      Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment      = Nothing
          , instanceDecl =
              case unionFieldWidth field of
                Nothing -> Hs.InstanceHasCField $ hasCFieldDecl
                Just w  -> Hs.InstanceHasCBitfield $ hasCBitfieldDecl w
          }
    , Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment      = Nothing
          , instanceDecl = Hs.InstanceHasField hasFieldDecl
          }
    ]
  where
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1253>
    -- Should be changed to @C.unionFieldWidth f@ when bit-fields in unions are
    -- supported.
    unionFieldWidth :: C.UnionField Final -> Maybe Int
    unionFieldWidth _f = Nothing

    parentType :: HsType
    parentType = Hs.HsTypRef unionName Nothing

    fieldName :: Hs.Name Hs.NsVar
    fieldName = Hs.unsafeHsIdHsName field.info.name.hsName

    fieldType :: HsType
    fieldType = Type.topLevel field.typ

    hasFieldDecl :: Hs.HasFieldInstance
    hasFieldDecl = Hs.HasFieldInstance {
          parentType = parentType
        , fieldName  = fieldName
        , fieldType  = fieldType
        , deriveVia  =
            case unionFieldWidth field of
              Nothing -> Hs.ViaHasCField
              Just _  -> Hs.ViaHasCBitfield
        }

    hasCFieldDecl :: Hs.HasCFieldInstance
    hasCFieldDecl = Hs.HasCFieldInstance {
          parentType  = parentType
        , fieldName   = fieldName
        , cFieldType  = fieldType
        , fieldOffset = 0
        }

    hasCBitfieldDecl :: Int -> Hs.HasCBitfieldInstance
    hasCBitfieldDecl w = Hs.HasCBitfieldInstance {
          parentType    = parentType
        , fieldName     = fieldName
        , cBitfieldType = fieldType
        , bitOffset     = 0
        , bitWidth      = w
        }

{-------------------------------------------------------------------------------
  Enum
-------------------------------------------------------------------------------}

enumDecs ::
     Map Inst.TypeClass Inst.SupportedStrategies
  -> FieldNamingStrategy
  -> HaddockConfig
  -> C.DeclInfo Final
  -> C.Enum Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl]
enumDecs supInsts fns haddockConfig info enum spec = aux <$> newtypeDec
  where
    valueMap :: Map Integer (NonEmpty (C.FieldInfo Final, Hs.Name Hs.NsConstr))
    valueMap = Map.fromListWith (flip (<>)) [ -- preserve source order
        let name = Hs.unsafeHsIdHsName constant.info.name.hsName
        in  (constant.value, NonEmpty.singleton (constant.info, name))
      | constant <- enum.constants
      ]

    valueNames :: Map Integer (NonEmpty String)
    valueNames = NonEmpty.map (Text.unpack . Hs.getName . snd) <$> valueMap

    mSeqBounds :: Maybe (Hs.Name Hs.NsConstr, Hs.Name Hs.NsConstr)
    mSeqBounds = do
      (minV, minNames) <- fmap (NonEmpty.map snd) <$> Map.lookupMin valueMap
      (maxV, maxNames) <- fmap (NonEmpty.map snd) <$> Map.lookupMax valueMap
      guard $ maxV - minV + 1 == fromIntegral (Map.size valueMap)
      return (NonEmpty.head minNames, NonEmpty.head maxNames)

    newtypeDec :: HsM Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.unsafeHsIdHsName info.id.unsafeHsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = enum.names.constr

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
              name    = enum.names.field
            , typ     = Type.topLevel enum.typ
            , origin  = Origin.GeneratedField
            , comment = Nothing
            }

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin = Origin.Decl{
              info = info
            , kind = Origin.Enum enum
            , spec = spec
            }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment = mkHaddocks haddockConfig info newtypeName

        candidateInsts :: Set Inst.TypeClass
        candidateInsts = Hs.getCandidateInsts supInsts

        knownInsts :: Set Inst.TypeClass
        knownInsts = Set.fromList $ catMaybes [
            Just Inst.CEnum
          , Just Inst.HasCField
          , Just Inst.HasField
          , Just Inst.Prim
          , Just Inst.Read
          , Just Inst.ReadRaw
          , Inst.SequentialCEnum <$ mSeqBounds
          , Just Inst.Show
          , Just Inst.StaticSize
          , Just Inst.Storable
          , Just Inst.WriteRaw
          ]

    -- everything in aux is state-dependent
    aux :: Hs.Newtype -> [Hs.Decl]
    aux nt =
        Hs.DeclNewtype nt
        : marshalDecls
        ++ primDecl
        : optDecls
        ++ cEnumInstanceDecls
        ++ typedefFieldDecls nt
        ++ valueDecls
      where
        hsStruct :: Hs.Struct (S Z)
        hsStruct = Hs.Struct {
              name      = nt.name
            , constr    = nt.constr
            , fields    = Vec.singleton nt.field
            , instances = nt.instances
            , origin    = Nothing
            , comment   = Nothing
            }

        marshalDecls :: [Hs.Decl]
        marshalDecls = [
            Hs.DeclDefineInstance Hs.DefineInstance{
                comment      = Nothing
              , instanceDecl =
                  Hs.InstanceStaticSize hsStruct Hs.StaticSizeInstance{
                      staticSizeOf    = enum.sizeof
                    , staticAlignment = enum.alignment
                    }
              }
          , Hs.DeclDefineInstance Hs.DefineInstance{
                comment      = Nothing
              , instanceDecl =
                  Hs.InstanceReadRaw hsStruct Hs.ReadRawInstance{
                      readRaw = Hs.Lambda "ptr" $
                        Hs.Ap (Hs.StructCon hsStruct) [ Hs.ReadRawByteOff IZ 0 ]
                    }
              }
          , Hs.DeclDefineInstance Hs.DefineInstance{
                comment      = Nothing
              , instanceDecl =
                  Hs.InstanceWriteRaw hsStruct Hs.WriteRawInstance{
                      writeRaw = Hs.Lambda "ptr" $ Hs.Lambda "s" $
                        Hs.ElimStruct IZ hsStruct (AS AZ) $
                          Hs.Seq [ Hs.WriteRawByteOff I2 0 IZ ]
                    }
              }
          , Hs.DeclDeriveInstance Hs.DeriveInstance{
                strategy = Hs.DeriveVia (HsEquivStorable (Hs.HsTypRef nt.name Nothing))
              , clss     = Inst.Storable
              , name     = nt.name
              , comment  = Nothing
              }
          ]

        primDecl :: Hs.Decl
        primDecl = Hs.DeclDeriveInstance Hs.DeriveInstance{
              strategy = Hs.DeriveVia nt.field.typ
            , clss     = Inst.Prim
            , name     = nt.name
            , comment  = Nothing
            }

        optDecls :: [Hs.Decl]
        optDecls = catMaybes [
            case Hs.getDeriveStrat supStrats of
              Nothing    -> Nothing
              Just strat -> Just $ Hs.DeclDeriveInstance Hs.DeriveInstance{
                  name     = nt.name
                , clss     = clss
                , strategy = strat
                , comment  = Nothing
                }
          | (clss, supStrats) <- Map.assocs supInsts
          ]

        cEnumInstanceDecls :: [Hs.Decl]
        cEnumInstanceDecls =
          let cEnumDecl = Hs.DeclDefineInstance Hs.DefineInstance{
                  comment      = Nothing
                , instanceDecl =
                    Hs.InstanceCEnum
                      hsStruct
                      nt.field.typ
                      valueNames
                      (isJust mSeqBounds)
                      fns
                }
              cEnumShowDecl = Hs.DeclDefineInstance Hs.DefineInstance{
                  comment      = Nothing
                , instanceDecl = Hs.InstanceCEnumShow hsStruct
                }
              cEnumReadDecl = Hs.DeclDefineInstance Hs.DefineInstance{
                  comment      = Nothing
                , instanceDecl = Hs.InstanceCEnumRead hsStruct
                }
              sequentialCEnumDecl = case mSeqBounds of
                Just (nameMin, nameMax) -> List.singleton $
                  Hs.DeclDefineInstance Hs.DefineInstance{
                      comment      = Nothing
                    , instanceDecl =
                        Hs.InstanceSequentialCEnum hsStruct nameMin nameMax
                    }
                Nothing -> []
          in  cEnumDecl : sequentialCEnumDecl ++ [cEnumShowDecl, cEnumReadDecl]

        valueDecls :: [Hs.Decl]
        valueDecls = [
              Hs.DeclPatSyn Hs.PatSyn{
                  name    = Hs.unsafeHsIdHsName constant.info.name.hsName
                , typ     = HsTypRef nt.name (Just nt.field.typ)
                , constr  = Just nt.constr
                , value   = constant.value
                , origin  = Origin.EnumConstant constant
                , comment = mkHaddocksFieldInfo haddockConfig info constant.info
                }
            | constant <- enum.constants
            ]

{-------------------------------------------------------------------------------
  Typedef
-------------------------------------------------------------------------------}

typedefDecs ::
     Map Inst.TypeClass Inst.SupportedStrategies
  -> HaddockConfig
  -> C.Sizeofs
  -> C.DeclInfo Final
  -> (C.Typedef Final -> Origin.Newtype)
  -> C.Typedef Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl]
typedefDecs supInsts haddockConfig sizeofs info mkNewtypeOrigin typedef spec = do
    nt <- newtypeDec
    pure $ aux nt
  where
    newtypeDec :: HsM Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.unsafeHsIdHsName info.id.unsafeHsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = typedef.names.constr

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
              name    = typedef.names.field
            , typ     = Type.topLevel typedef.typ
            , origin  = Origin.GeneratedField
            , comment = Nothing
            }

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin =  Origin.Decl{
              info = info
            , kind = mkNewtypeOrigin typedef
            , spec = spec
            }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment =  mkHaddocks haddockConfig info newtypeName

        candidateInsts :: Set Inst.TypeClass
        candidateInsts = Hs.getCandidateInsts supInsts

        knownInsts :: Set Inst.TypeClass
        knownInsts = Set.fromList $ catMaybes [
            Inst.FromFunPtr <$ mFunPtr
          , Just Inst.HasCField
          , Just Inst.HasField
          , Inst.ToFunPtr <$ mFunPtr
          ]

        -- See comment in 'newtypeWrapper` below
        mFunPtr :: Maybe ()
        mFunPtr = case typedef.typ of
          C.TypeFun args res | not (any C.hasUnsupportedType (res:args)) ->
            Just ()
          _otherwise -> Nothing

    -- everything in aux is state-dependent
    aux :: Hs.Newtype -> [Hs.Decl]
    aux nt =
        Hs.DeclNewtype nt
        : newtypeWrapper
        ++ optDecls
        ++ typedefFieldDecls nt
      where
        optDecls :: [Hs.Decl]
        optDecls = catMaybes [
            case Hs.getDeriveStrat supStrats of
              Nothing    -> Nothing
              Just strat -> Just $ Hs.DeclDeriveInstance Hs.DeriveInstance{
                  name     = nt.name
                , clss     = clss
                , strategy = strat
                , comment  = Nothing
                }
          | (clss, supStrats) <- Map.assocs supInsts
          , clss `Set.member` nt.instances
          ]

        newtypeWrapper :: [Hs.Decl]
        newtypeWrapper  =
          case typedef.typ of
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
              ToFromFunPtr.forNewtype sizeofs nt (args, res)
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
-- > newtype MyInt = MyInt { unwrapMyInt :: CInt }
--
-- Then, 'typedefFieldDecls' will generate roughly the following class
-- instances.
--
-- > instance HasCField "unwrapMyInt" MyInt where
-- >   type CFieldType "unwrapMyInt" MyInt = CInt
-- > instance HasField "unwrapMyInt" (Ptr MyInt) (Ptr CInt)
--
-- These instance help eliminating newtypes from 'Ptr' types. Naturally,
-- newtypes can also be introduced in 'Ptr' types, but this should be done using
-- 'castPtr' or some similar function.
typedefFieldDecls :: Hs.Newtype -> [Hs.Decl]
typedefFieldDecls hsNewType = [
    -- * Eliminate newtypes

      Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment      = Nothing
          , instanceDecl = Hs.InstanceHasField elimHasFieldDecl
          }
    , Hs.DeclDefineInstance $
        Hs.DefineInstance {
            comment      = Nothing
          , instanceDecl = Hs.InstanceHasCField elimHasCFieldDecl
          }
    ]
  where
    parentType :: HsType
    parentType = Hs.HsTypRef hsNewType.name (Just hsNewType.field.typ)

    elimHasFieldDecl :: Hs.HasFieldInstance
    elimHasFieldDecl = Hs.HasFieldInstance{
          parentType = parentType
        , fieldName  = hsNewType.field.name
        , fieldType  = hsNewType.field.typ
        , deriveVia  = Hs.ViaHasCField
        }

    elimHasCFieldDecl :: Hs.HasCFieldInstance
    elimHasCFieldDecl = Hs.HasCFieldInstance{
          parentType  = parentType
        , fieldName   = hsNewType.field.name
        , cFieldType  = hsNewType.field.typ
        , fieldOffset = 0
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
-- > newtype F     = F (Ptr.FunPtr F_Aux)
--
-- so that @F_Aux@ can be given @ToFunPtr@/@FromFunPtr@ instances.
typedefFunPtrDecs ::
     Map Inst.TypeClass Inst.SupportedStrategies
  -> FieldNamingStrategy
  -> HaddockConfig
  -> C.Sizeofs
  -> C.DeclInfo Final
  -> Int                             -- ^ Number of indirections
  -> ([C.Type Final], C.Type Final)  -- ^ Function arguments and result
  -> MangleNames.NewtypeNames
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl]
typedefFunPtrDecs supInsts fns haddockConfig sizeofs origInfo n (args, res) origNames origSpec =
    fmap concat $ sequence [
        typedefDecs supInsts haddockConfig sizeofs auxInfo  Origin.Aux     auxTypedef  auxSpec
      , typedefDecs supInsts haddockConfig sizeofs origInfo Origin.Typedef mainTypedef origSpec
      ]
  where
    -- TODO: For historical reasons we currently implement this by making a
    -- "fake" C declaration, and then translating that immediately to Haskell.
    -- We should fuse this, and just generate the Haskell declarations directly.

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1379>
    -- The name of this auxiliary type should be configurable.
    auxDeclIdPair :: DeclIdPair
    auxDeclIdPair =
        -- Still refer to the /original/ C decl...?
        renameHsName (<> "_Aux") origInfo.id

    auxInfo :: C.DeclInfo Final
    auxInfo = C.DeclInfo {
          loc          = origInfo.loc
        , headerInfo   = origInfo.headerInfo
        , comment      = Just auxComment
        , availability = C.Available
        , id           = auxDeclIdPair
        }

    auxComment :: C.Comment Final
    auxComment = C.Comment $ Clang.Comment [
          Clang.Paragraph [
              Clang.TextContent "Auxiliary type used by "
            , Clang.InlineRefCommand $
                C.CommentRef
                  origInfo.id.cName.name.text
                  (Just origInfo.id)
            ]
        ]

    -- TODO: This duplicates logic from 'mkNewtypeNames' in the name mangler.
    auxTypedef :: C.Typedef Final
    auxTypedef = C.Typedef{
          typ = C.TypeFun args res
        , ann = MangleNames.NewtypeNames{
              constr = Hs.unsafeHsIdHsName auxDeclIdPair.unsafeHsName
            , field  = Hs.unsafeHsIdHsName
                     $ case fns of
                         EnableRecordDot    -> "unwrap"
                         PrefixedFieldNames -> "unwrap" <> auxDeclIdPair.unsafeHsName

            }
        }

    -- TODO: Is this right..?
    auxSpec :: PrescriptiveDeclSpec
    auxSpec = PrescriptiveDeclSpec{
          cSpec  = Nothing
        , hsSpec = Nothing
        }

    mainTypedef :: C.Typedef Final
    mainTypedef = C.Typedef{
          ann = origNames
        , typ = C.TypePointers n $ C.TypeTypedef $ C.Ref {
              name       = auxInfo.id
            , underlying = C.TypeFun args res
            }
        }

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

macroDecs ::
     Map Inst.TypeClass Inst.SupportedStrategies
  -> HaddockConfig
  -> C.DeclInfo Final
  -> CheckedMacro Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl]
macroDecs supInsts haddockConfig info checkedMacro spec =
    case checkedMacro of
      MacroType ty   -> macroDecsTypedef supInsts haddockConfig info ty spec
      MacroExpr expr -> pure $ macroVarDecs haddockConfig info expr

macroDecsTypedef ::
     Map Inst.TypeClass Inst.SupportedStrategies
  -> HaddockConfig
  -> C.DeclInfo Final
  -> CheckedMacroType Final
  -> PrescriptiveDeclSpec
  -> HsM [Hs.Decl]
macroDecsTypedef supInsts haddockConfig info macroType spec = do
    nt <- newtypeDec
    pure $ aux nt
  where
    newtypeDec :: HsM Hs.Newtype
    newtypeDec =
        Hs.newtypeDec newtypeName newtypeConstr newtypeField
          newtypeOrigin newtypeComment candidateInsts knownInsts
      where
        newtypeName :: Hs.Name Hs.NsTypeConstr
        newtypeName = Hs.unsafeHsIdHsName info.id.unsafeHsName

        newtypeConstr :: Hs.Name Hs.NsConstr
        newtypeConstr = macroType.names.constr

        newtypeField :: Hs.Field
        newtypeField = Hs.Field {
              name    = macroType.names.field
            , typ     = Type.topLevel macroType.typ
            , origin  = Origin.GeneratedField
            , comment = Nothing
            }

        newtypeOrigin :: Origin.Decl Origin.Newtype
        newtypeOrigin = Origin.Decl {
              info = info
            , kind = Origin.Macro macroType
            , spec = spec
            }

        newtypeComment :: Maybe HsDoc.Comment
        newtypeComment = mkHaddocks haddockConfig info newtypeName

        candidateInsts :: Set Inst.TypeClass
        candidateInsts = Hs.getCandidateInsts supInsts

        knownInsts :: Set Inst.TypeClass
        knownInsts = Set.fromList [Inst.HasCField, Inst.HasField]

    -- everything in aux is state-dependent
    aux :: Hs.Newtype -> [Hs.Decl]
    aux nt = Hs.DeclNewtype nt : optDecls ++ typedefFieldDecls nt
      where
        optDecls :: [Hs.Decl]
        optDecls = catMaybes [
            case Hs.getDeriveStrat supStrats of
              Nothing    -> Nothing
              Just strat -> Just $ Hs.DeclDeriveInstance Hs.DeriveInstance{
                  name     = nt.name
                , clss     = clss
                , strategy = strat
                , comment  = Nothing
                }
          | (clss, supStrats) <- Map.assocs supInsts
          , clss `Set.member` nt.instances
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
     UniqueId
  -> HaddockConfig
  -> BaseModuleName
  -> TranslationState
  -> C.Sizeofs
  -> C.DeclInfo Final
  -> C.Type Final
  -> PrescriptiveDeclSpec
  -> [Hs.Decl]
global uniqueId haddockConfig moduleName transState sizeofs info ty _spec
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
    | C.isErasedTypeConstQualified ty && Inst.Storable `elem` insts =
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
    getStubDecsWith x = addressStubDecs uniqueId haddockConfig moduleName sizeofs info ty x _spec

    insts :: Set Inst.TypeClass
    insts =
      Hs.getInstances
        transState.instanceMap
        Nothing
        (Set.singleton Inst.Storable)
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
  -> C.DeclInfo Final
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

    getterName = Hs.unsafeHsIdHsName info.id.unsafeHsName
    getterType = SHs.translateType ty
    getterExpr = SHs.EGlobal SHs.IO_unsafePerformIO
                `SHs.EApp` (SHs.EGlobal SHs.PtrConst_peek
                `SHs.EApp` SHs.EFree pureStubName)

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
     UniqueId
  -> HaddockConfig
  -> BaseModuleName
  -> C.Sizeofs
  -> C.DeclInfo Final -- ^ The given declaration
  -> C.Type Final     -- ^ The type of the given declaration
  -> RunnerNameSpec
  -> PrescriptiveDeclSpec
  -> ( [Hs.Decl]
     , Hs.Name 'Hs.NsVar
     )
addressStubDecs uniqueId haddockConfig moduleName sizeofs info ty runnerNameSpec _spec =
    (foreignImport ++ runnerDecls, runnerName)
  where
    -- *** Stub (impure) ***
    stubImportType :: HsType
    stubImportType = HsIO $ Type.topLevel stubType

    stubSymbol :: UniqueSymbol
    stubSymbol = globallyUnique uniqueId moduleName $ "get_" ++ varName

    stubName :: Hs.Name Hs.NsVar
    stubName = Hs.InternalName stubSymbol

    varName :: String
    varName = Text.unpack $ info.id.cName.name.text

    stubType :: C.Type Final
    stubType = C.TypePointers 1 ty

    prettyStub :: String
    prettyStub = concat [
          "/* ", stubSymbol.source, " */\n"
        , PC.prettyFunDefn stubDecl ""
        ]

    stubDecl :: PC.FunDefn
    stubDecl =
        PC.withArgs [] $ \args' ->
          PC.FunDefn stubSymbol.unique stubType C.HaskellPureFunction args' $
            PC.CSList $
            PC.CSStatement
              (PC.ExpressionStatement $ PC.Return $ PC.Address $ PC.NamedVar varName)
              PC.CSNil

    cWrapper :: CWrapper
    cWrapper = CWrapper {
          definition     = prettyStub
        , hashIncludeArg = getMainHashIncludeArg info
        }

    mbComment = mkHaddocks haddockConfig info runnerName

    foreignImport :: [Hs.Decl]
    foreignImport =
        Hs.ForeignImport.foreignImportDec
          sizeofs
          (Hs.ForeignImport.FunName stubSymbol)
          []
          (Hs.ForeignImport.FunRes stubImportType)
          (uniqueCDeclName stubSymbol)
          (CallConvUserlandCapi cWrapper)
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
    name = info.id.unsafeHsName.text

    uniquify :: Text -> UniqueSymbol
    uniquify = globallyUnique uniqueId moduleName . Text.unpack

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
  -> C.DeclInfo Final
  -> CheckedMacroExpr
  -> [Hs.Decl]
macroVarDecs haddockConfig info macroExpr = [
      Hs.DeclMacroExpr $
        Hs.MacroExpr
          { name    = hsVarName
          , expr    = macroExpr
          , comment = mkHaddocks haddockConfig info hsVarName
          }
    ]
  where
    hsVarName :: Hs.Name Hs.NsVar
    hsVarName = Hs.unsafeHsIdHsName info.id.unsafeHsName

{-------------------------------------------------------------------------------
  Anon Enum Constants
-------------------------------------------------------------------------------}

anonEnumConstantDecs ::
     HaddockConfig
  -> C.DeclInfo Final
  -> C.AnonEnumConstant Final
  -> [Hs.Decl]
anonEnumConstantDecs haddockConfig info anonEnumConstant =
    let
      patSynName :: Hs.Name Hs.NsConstr
      patSynName = Hs.unsafeHsIdHsName anonEnumConstant.constant.info.name.hsName

      patSynType :: HsType
      patSynType = Type.topLevel (C.TypePrim anonEnumConstant.typ)

      typeSigDecl :: Hs.Decl
      typeSigDecl = Hs.DeclPatSyn Hs.PatSyn{
            name    = patSynName
          , typ     = patSynType
          , constr  = Nothing
          , value   = fromInteger anonEnumConstant.constant.value
          , origin  = Origin.EnumConstant anonEnumConstant.constant
          , comment = mkHaddocks haddockConfig info patSynName
          }
  in  [typeSigDecl]

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

-- TODO https://github.com/well-typed/hs-bindgen/issues/1504:
-- Incorporate all name mangling into the name mangler.
renameHsName :: (Hs.Identifier -> Hs.Identifier) -> DeclIdPair -> DeclIdPair
renameHsName f dip = DeclIdPair {
      cName = dip.cName
    , hsName = renameAssignedIdentifier dip.hsName
    }
  where
    renameAssignedIdentifier :: AssignedIdentifier -> AssignedIdentifier
    renameAssignedIdentifier = \case
        NoAssignedIdentifier cstack reason -> NoAssignedIdentifier cstack reason
        AssignedIdentifier x -> AssignedIdentifier (f x)
