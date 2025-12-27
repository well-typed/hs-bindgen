{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Simplified HS translation (from high level HS)
module HsBindgen.Backend.SHs.Translation (
    translateDecls,
    translateType,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vec.Lazy qualified as Vec

import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.Macro
import HsBindgen.Backend.SHs.Translation.Common
import HsBindgen.Backend.SHs.Translation.Prim qualified as SHsPrim
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

translateDecls ::
  ByCategory_ [Hs.Decl] -> ByCategory_ ([CWrapper], [SDecl])
translateDecls = fmap go
  where
    go :: [Hs.Decl] -> ([CWrapper], [SDecl])
    go decls = (wrappers, concatMap translateDecl decls)
      where
        wrappers = getCWrappers decls

-- Find and assemble C sources required by foreign imports.
getCWrappers :: [Hs.Decl] -> [CWrapper]
getCWrappers decls = mapMaybe getCWrapper decls
  where
    getCWrapper :: Hs.Decl -> Maybe CWrapper
    getCWrapper = \case
      Hs.DeclForeignImport importDecl ->
        case importDecl.callConv of
          CallConvUserlandCAPI w -> Just w
          _otherCallConv         -> Nothing
      _otherDecl -> Nothing

translateDecl :: Hs.Decl -> [SDecl]
translateDecl (Hs.DeclData d)           = singleton $ translateDeclData d
translateDecl (Hs.DeclEmpty d)          = singleton $ translateDeclEmpty d
translateDecl (Hs.DeclNewtype n)        = singleton $ translateNewtype n
translateDecl (Hs.DeclDefineInstance i) = singleton $ translateDefineInstanceDecl i
translateDecl (Hs.DeclDeriveInstance i) = singleton $ translateDeriveInstance i
translateDecl (Hs.DeclMacroExpr e)      = singleton $ translateMacroExpr e
translateDecl (Hs.DeclForeignImport i)  = translateForeignImportDecl i
translateDecl (Hs.DeclFunction f)       = singleton $ translateFunctionDecl f
translateDecl (Hs.DeclPatSyn ps)        = singleton $ translatePatSyn ps
translateDecl (Hs.DeclUnionGetter u)    = singleton $ translateUnionGetter u
translateDecl (Hs.DeclUnionSetter u)    = singleton $ translateUnionSetter u
translateDecl (Hs.DeclVar d)            = singleton $ translateDeclVar d

translateDefineInstanceDecl :: Hs.DefineInstance -> SDecl
translateDefineInstanceDecl defInst =
    case defInst.instanceDecl of
      Hs.InstanceStorable struct i ->
        DInst $ translateStorableInstance struct i defInst.comment
      Hs.InstancePrim struct i ->
        DInst $ SHsPrim.translatePrimInstance struct i defInst.comment
      Hs.InstanceHasCField i ->
        DInst $ translateHasCFieldInstance i defInst.comment
      Hs.InstanceHasCBitfield i ->
        DInst $ translateHasCBitfieldInstance i defInst.comment
      Hs.InstanceHasField i ->
        DInst $ translateHasFieldInstance i defInst.comment
      Hs.InstanceHasFLAM struct fty i ->
        DInst Instance{
            instanceClass   = HasFlexibleArrayMember_class
          , instanceArgs    = [ translateType fty, TCon struct.name ]
          , instanceSuperClasses = []
          , instanceTypes   = []
          , instanceDecs    = [(HasFlexibleArrayMember_offset, ELam "_ty" $ EIntegral (toInteger i) Nothing)]
          , instanceComment = defInst.comment
          }
      Hs.InstanceCEnum struct fTyp vMap isSequential ->
        DInst $ translateCEnumInstance struct fTyp vMap isSequential defInst.comment
      Hs.InstanceSequentialCEnum struct nameMin nameMax ->
        DInst $ translateSequentialCEnum struct nameMin nameMax defInst.comment
      Hs.InstanceCEnumShow struct ->
        DInst $ translateCEnumInstanceShow struct defInst.comment
      Hs.InstanceCEnumRead struct ->
        DInst $ translateCEnumInstanceRead struct defInst.comment
      Hs.InstanceToFunPtr inst ->
        DInst Instance{
            instanceClass        = ToFunPtr_class
          , instanceArgs         = [translateType inst.typ]
          , instanceSuperClasses = []
          , instanceTypes        = []
          , instanceDecs         = [ ( ToFunPtr_toFunPtr
                                     , EFree $ Hs.InternalName inst.body
                                     )
                                   ]
          , instanceComment      = defInst.comment
          }
      Hs.InstanceFromFunPtr inst ->
        DInst Instance{
            instanceClass        = FromFunPtr_class
          , instanceArgs         = [translateType inst.typ]
          , instanceSuperClasses = []
          , instanceTypes        = []
          , instanceDecs         = [ ( FromFunPtr_fromFunPtr
                                     , EFree $ Hs.InternalName inst.body
                                     )
                                   ]
          , instanceComment      = defInst.comment
          }

translateDeclData :: Hs.Struct n -> SDecl
translateDeclData struct = DRecord Record{
      dataType = struct.name
    , dataCon  = struct.constr
    , dataFields =
        [ Field {
              fieldName    = f.name
            , fieldType    = translateType f.typ
            , fieldOrigin  = f.origin
            , fieldComment = f.comment
            }
        | f <- toList struct.fields
        ]
    , dataOrigin =
        case struct.origin of
          Just origin -> origin
          Nothing     -> panicPure "Missing structOrigin"
    , dataDeriv   = []
    , dataComment = struct.comment
    }

translateDeclEmpty :: Hs.EmptyData -> SDecl
translateDeclEmpty d = DEmptyData EmptyData{
      emptyDataName    = d.name
    , emptyDataOrigin  = d.origin
    , emptyDataComment = d.comment
    }

translateNewtype :: Hs.Newtype -> SDecl
translateNewtype n = DNewtype Newtype{
      newtypeName    = n.name
    , newtypeCon     = n.constr
    , newtypeOrigin  = n.origin
    , newtypeDeriv   = []
    , newtypeComment = n.comment
    , newtypeField   = Field {
          fieldName    = n.field.name
        , fieldType    = translateType n.field.typ
        , fieldOrigin  = n.field.origin
        , fieldComment = n.field.comment
        }
    }

translateDeriveInstance :: Hs.DeriveInstance -> SDecl
translateDeriveInstance deriv = DDerivingInstance DerivingInstance {
      derivingInstanceStrategy = fmap translateType deriv.strategy
    , derivingInstanceType     = TApp (translateTypeClass deriv.clss) (TCon deriv.name)
    , derivingInstanceComment  = deriv.comment
    }

translateTypeClass :: Hs.TypeClass -> ClosedType
translateTypeClass Hs.Bits               = TGlobal Bits_class
translateTypeClass Hs.Bounded            = TGlobal Bounded_class
translateTypeClass Hs.Enum               = TGlobal Enum_class
translateTypeClass Hs.Eq                 = TGlobal Eq_class
translateTypeClass Hs.FiniteBits         = TGlobal FiniteBits_class
translateTypeClass Hs.Floating           = TGlobal Floating_class
translateTypeClass Hs.Fractional         = TGlobal Fractional_class
translateTypeClass Hs.Integral           = TGlobal Integral_class
translateTypeClass Hs.Ix                 = TGlobal Ix_class
translateTypeClass Hs.Num                = TGlobal Num_class
translateTypeClass Hs.Ord                = TGlobal Ord_class
translateTypeClass Hs.Prim               = TGlobal Prim_class
translateTypeClass Hs.Read               = TGlobal Read_class
translateTypeClass Hs.ReadRaw            = TGlobal ReadRaw_class
translateTypeClass Hs.Real               = TGlobal Real_class
translateTypeClass Hs.RealFloat          = TGlobal RealFloat_class
translateTypeClass Hs.RealFrac           = TGlobal RealFrac_class
translateTypeClass Hs.Show               = TGlobal Show_class
translateTypeClass Hs.StaticSize         = TGlobal StaticSize_class
translateTypeClass Hs.Storable           = TGlobal Storable_class
translateTypeClass Hs.WriteRaw           = TGlobal WriteRaw_class
translateTypeClass Hs.HasBaseForeignType = TGlobal HasBaseForeignType_class

translateForeignImportDecl :: Hs.ForeignImportDecl -> [SDecl]
translateForeignImportDecl importDecl = [
        DForeignImport ForeignImport{
            foreignImportParameters =
              map translateParam importDecl.parameters
          , foreignImportResult =
              Result (translateType importDecl.result) Nothing

            -- The rest of the fields are copied over as-is
          , foreignImportName     = importDecl.name
          , foreignImportOrigName = importDecl.origName
          , foreignImportCallConv = importDecl.callConv
          , foreignImportOrigin   = importDecl.origin
          , foreignImportComment  = importDecl.comment
          , foreignImportSafety   = importDecl.safety
          }
      ]

translateParam :: Hs.FunctionParameter -> Parameter
translateParam param = Parameter {
      name    = param.name
    , typ     = translateType param.typ
    , comment = param.comment
    }

translateFunctionDecl :: Hs.FunctionDecl -> SDecl
translateFunctionDecl functionDecl = DBinding Binding{
      parameters = map translateParam functionDecl.parameters
    , result     = Result (translateType functionDecl.result) Nothing
      -- The other fields are copied as-is
    , name       = functionDecl.name
    , body       = functionDecl.body
    , pragmas    = functionDecl.pragmas
    , comment    = functionDecl.comment
    }

translatePatSyn :: Hs.PatSyn -> SDecl
translatePatSyn patSyn = DPatternSynonym PatternSynonym{
      patSynType    = TCon patSyn.typ
    , patSynRHS     = PEApps patSyn.constr [PELit patSyn.value]
      -- The other fields are copied as-is
    , patSynName    = patSyn.name
    , patSynOrigin  = patSyn.origin
    , patSynComment = patSyn.comment
    }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

translateType :: Hs.HsType -> ClosedType
translateType = \case
    Hs.HsPrimType t         -> TGlobal (PrimType t)
    Hs.HsTypRef r           -> TCon r
    Hs.HsConstArray n t     -> TGlobal ConstantArray `TApp` TLit n `TApp` (translateType t)
    Hs.HsIncompleteArray t  -> TGlobal IncompleteArray `TApp` (translateType t)
    Hs.HsPtr t              -> TApp (TGlobal Foreign_Ptr) (translateType t)
    Hs.HsFunPtr t           -> TApp (TGlobal Foreign_FunPtr) (translateType t)
    Hs.HsStablePtr t        -> TApp (TGlobal Foreign_StablePtr) (translateType t)
    Hs.HsConstPtr t         -> TApp (TGlobal ConstPtr_type) (translateType t)
    Hs.HsIO t               -> TApp (TGlobal IO_type) (translateType t)
    Hs.HsFun a b            -> TFun (translateType a) (translateType b)
    Hs.HsExtBinding r c hs  -> TExt r c hs
    Hs.HsByteArray          -> TGlobal ByteArray_type
    Hs.HsSizedByteArray n m -> TGlobal SizedByteArray_type `TApp` TLit n `TApp` TLit m
    Hs.HsBlock t            -> TGlobal Block_type `TApp` translateType t
    Hs.HsComplexType t      -> TApp (TGlobal ComplexType) (translateType (HsPrimType t))
    Hs.HsStrLit s           -> TStrLit s

{-------------------------------------------------------------------------------
  'Storable'
-------------------------------------------------------------------------------}

translateStorableInstance ::
     Hs.Struct n
  -> Hs.StorableInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateStorableInstance struct inst mbComment = Instance{
      instanceClass        = Storable_class
    , instanceArgs         = [TCon struct.name]
    , instanceSuperClasses = []
    , instanceTypes        = []
    , instanceComment      = mbComment
    , instanceDecs         = [
          (Storable_sizeOf    , EUnusedLam $ EInt inst.sizeOf)
        , (Storable_alignment , EUnusedLam $ EInt inst.alignment)
        , (Storable_peek      , peek)
        , (Storable_poke      , poke)
        ]
    }
  where
    peek = lambda (idiom structCon translatePeekCField) inst.peek
    poke = lambda (lambda (translateElimStruct (doAll translatePokeCField))) inst.poke

translatePeekCField :: Hs.PeekCField ctx -> SExpr ctx
translatePeekCField (Hs.PeekCField field ptr) = appMany HasCField_peekCField [EGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr]
translatePeekCField (Hs.PeekCBitfield field ptr) = appMany HasCBitfield_peekCBitfield [EGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr]
translatePeekCField (Hs.PeekByteOff ptr i) = appMany Storable_peekByteOff [EBound ptr, EInt i]

translatePokeCField :: Hs.PokeCField ctx -> SExpr ctx
translatePokeCField (Hs.PokeCField field ptr x) = appMany HasCField_pokeCField [EGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr, EBound x]
translatePokeCField (Hs.PokeCBitfield field ptr x) = appMany HasCBitfield_pokeCBitfield [EGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr, EBound x]
translatePokeCField (Hs.PokeByteOff ptr i x) = appMany Storable_pokeByteOff [EBound ptr, EInt i, EBound x]

{-------------------------------------------------------------------------------
  'HasCField'
-------------------------------------------------------------------------------}

translateHasCFieldInstance ::
     Hs.HasCFieldInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasCFieldInstance Hs.HasCFieldInstance{..} mbComment = do
    Instance {
        instanceClass   = HasCField_class
      , instanceArgs    = [parentType, fieldNameLitType]
      , instanceSuperClasses = []
      , instanceTypes   = [
            (HasCField_CFieldType, [parentType, fieldNameLitType], fieldType)
          ]
      , instanceDecs    = [
            (HasCField_offset#, EUnusedLam $ EUnusedLam $ EIntegral o Nothing)
          ]
      , instanceComment = mbComment
      }
  where
    parentType = translateType hasCFieldInstanceParentType
    fieldNameLitType = translateType $ HsStrLit $ T.unpack $ Hs.getName hasCFieldInstanceFieldName
    fieldType = translateType hasCFieldInstanceCFieldType
    o = fromIntegral hasCFieldInstanceFieldOffset

{-------------------------------------------------------------------------------
  'HasCBitfield'
-------------------------------------------------------------------------------}

translateHasCBitfieldInstance ::
     Hs.HasCBitfieldInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasCBitfieldInstance Hs.HasCBitfieldInstance{..} mbComment = do
    Instance
      { instanceClass   = HasCBitfield_class
      , instanceArgs    = [parentType, fieldNameLitType]
      , instanceSuperClasses = []
      , instanceTypes   = [
            (HasCBitfield_CBitfieldType, [parentType, fieldNameLitType], fieldType)
          ]
      , instanceDecs    = [
            (HasCBitfield_bitOffset#, EUnusedLam $ EUnusedLam $ EIntegral o Nothing )
          , (HasCBitfield_bitWidth#, EUnusedLam $ EUnusedLam $ EIntegral w Nothing)
          ]
      , instanceComment = mbComment
      }
  where
    parentType = translateType hasCBitfieldInstanceParentType
    fieldNameLitType = translateType $ HsStrLit $ T.unpack $ Hs.getName hasCBitfieldInstanceFieldName
    fieldType = translateType hasCBitfieldInstanceCBitfieldType
    o = fromIntegral hasCBitfieldInstanceBitOffset
    w = fromIntegral hasCBitfieldInstanceBitWidth

translateHasFieldInstance ::
     Hs.HasFieldInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasFieldInstance Hs.HasFieldInstance{..} mbComment = do
    Instance {
        instanceClass   = HasField_class
      , instanceArgs    = [fieldNameLitType, parentPtr, tyPtr]
      , instanceSuperClasses = [
            (NomEq_class, [
                tyTypeVar
              , TGlobal fieldTypeGlobal `TApp` parentType `TApp` fieldNameLitType]
              )
          ]
      , instanceTypes   = []
      , instanceDecs    = [
            (HasField_getField,
              EGlobal ptrToFieldGlobal `EApp`
              (EGlobal Proxy_constructor `ETypeApp` fieldNameLitType)
            )
          ]
      , instanceComment = mbComment
      }
  where
    (fieldTypeGlobal, ptrToFieldGlobal, tyPtr) = case hasFieldInstanceVia of
      Hs.ViaHasCField    ->
        ( HasCField_CFieldType
        , HasCField_ptrToCField
        , TGlobal Foreign_Ptr `TApp` tyTypeVar
        )
      Hs.ViaHasCBitfield ->
        ( HasCBitfield_CBitfieldType
        , HasCBitfield_ptrToCBitfield
        , TGlobal HasCBitfield_BitfieldPtr `TApp` parentType `TApp` fieldNameLitType
        )

    parentType = translateType hasFieldInstanceParentType
    parentPtr = TGlobal Foreign_Ptr `TApp` parentType
    fieldNameLitType = translateType $ HsStrLit $ T.unpack $ Hs.getName hasFieldInstanceFieldName
    -- TODO: this is not actually a free type variable. See issue #1287.
    tyTypeVar = TFree $ Hs.ExportedName "ty"

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

translateUnionGetter :: Hs.UnionGetter -> SDecl
translateUnionGetter getter = DBinding Binding{
      name       = getter.name
    , result     = Result (translateType getter.typ) Nothing
    , body       = EGlobal ByteArray_getUnionPayload
    , pragmas    = []
    , comment    = getter.comment
    , parameters = [
          Parameter {
              name    = Nothing
            , typ     = TCon getter.constr
            , comment = Nothing
            }
        ]
    }

translateUnionSetter :: Hs.UnionSetter -> SDecl
translateUnionSetter setter = DBinding Binding{
      name       = setter.name
    , result     = Result (TCon setter.constr) Nothing
    , body       = EGlobal ByteArray_setUnionPayload
    , pragmas    = []
    , comment    = setter.comment
    , parameters = [
          Parameter {
             name    = Nothing
           , typ     = translateType setter.typ
           , comment = Nothing
           }
        ]
    }

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

translateDeclVar :: Hs.Var -> SDecl
translateDeclVar Hs.Var{..} = DBinding Binding {
      name      = name
    , parameters= []
    , result    = Result typ Nothing
    , body      = expr
    , pragmas   = pragmas
    , comment   = comment
    }

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

translateCEnumInstance ::
     Hs.Struct (S Z)
  -> HsType
  -> Map Integer (NonEmpty String)
  -> Bool
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstance struct fTyp vMap isSequential mbComment = Instance {
      instanceClass = CEnum_class
    , instanceArgs  = [tcon]
    , instanceSuperClasses = []
    , instanceTypes = [(CEnumZ_tycon, [tcon], translateType fTyp)]
    , instanceDecs  = [
          (CEnum_toCEnum, ECon struct.constr)
        , (CEnum_fromCEnum, EFree fname)
        , (CEnum_declaredValues, EUnusedLam declaredValuesE)
        , (CEnum_showsUndeclared, EApp (EGlobal CEnum_showsWrappedUndeclared) dconStrE)
        , (CEnum_readPrecUndeclared, EApp (EGlobal CEnum_readPrecWrappedUndeclared) dconStrE)
        ] ++ seqDecs
    , instanceComment = mbComment
    }
  where
    tcon :: ClosedType
    tcon = TCon struct.name

    dconStrE :: SExpr ctx
    dconStrE = EString . T.unpack $ Hs.getName struct.constr

    fname :: Hs.Name Hs.NsVar
    fname = (NonEmpty.head $ Vec.toNonEmpty struct.fields).name

    declaredValuesE :: SExpr ctx
    declaredValuesE = EApp (EGlobal CEnum_declaredValuesFromList) $ EList [
        ETup [
            EIntegral v Nothing
          , if null names
              then EApp (EGlobal NonEmpty_singleton) (EString name)
              else
                EInfix
                  NonEmpty_constructor
                  (EString name)
                  (EList (EString <$> names))
          ]
      | (v, name :| names) <- Map.toList vMap
      ]

    seqDecs :: [(Global, ClosedExpr)]
    seqDecs
      | isSequential = [
            (CEnum_isDeclared, EGlobal CEnum_seqIsDeclared)
          , (CEnum_mkDeclared, EGlobal CEnum_seqMkDeclared)
          ]
      | otherwise = []

translateSequentialCEnum ::
     Hs.Struct (S Z)
  -> Hs.Name Hs.NsConstr
  -> Hs.Name Hs.NsConstr
  -> Maybe HsDoc.Comment
  -> Instance
translateSequentialCEnum struct nameMin nameMax mbComment = Instance {
      instanceClass = SequentialCEnum_class
    , instanceArgs  = [tcon]
    , instanceSuperClasses = []
    , instanceTypes = []
    , instanceDecs  = [
          (SequentialCEnum_minDeclaredValue, ECon nameMin)
        , (SequentialCEnum_maxDeclaredValue, ECon nameMax)
        ]
    , instanceComment = mbComment
    }
  where
    tcon :: ClosedType
    tcon = TCon struct.name

translateCEnumInstanceShow ::
     Hs.Struct (S Z)
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstanceShow struct mbComment = Instance {
      instanceClass = Show_class
    , instanceArgs  = [tcon]
    , instanceSuperClasses = []
    , instanceTypes = []
    , instanceDecs  = [
          (Show_showsPrec, EGlobal CEnum_showsCEnum)
        ]
    , instanceComment = mbComment
    }
  where
    tcon :: ClosedType
    tcon = TCon struct.name

translateCEnumInstanceRead ::
     Hs.Struct (S Z)
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstanceRead struct mbComment = Instance {
      instanceClass = Read_class
    , instanceArgs  = [tcon]
    , instanceSuperClasses = []
    , instanceTypes = []
    , instanceDecs  = [
          (Read_readPrec, EGlobal CEnum_readPrecCEnum)
        , (Read_readList, EGlobal Read_readListDefault)
        , (Read_readListPrec, EGlobal Read_readListPrecDefault)
        ]
    , instanceComment = mbComment
    }
  where
    tcon :: ClosedType
    tcon = TCon struct.name
