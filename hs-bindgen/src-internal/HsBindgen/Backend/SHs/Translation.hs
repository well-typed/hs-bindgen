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
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

translateDecls :: ByCategory_ [Hs.Decl] -> ByCategory_ ([CWrapper], [SDecl])
translateDecls = fmap go
  where
    go :: [Hs.Decl] -> ([CWrapper], [SDecl])
    go decls = (wrappers, map translateDecl decls)
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
          CallConvUserlandCapi w -> Just w
          _otherCallConv         -> Nothing
      _otherDecl -> Nothing

translateDecl :: Hs.Decl -> SDecl
translateDecl = \case
    Hs.DeclTypSyn         x -> translateDeclTypSyn         x
    Hs.DeclData           x -> translateDeclData           x
    Hs.DeclEmpty          x -> translateDeclEmpty          x
    Hs.DeclNewtype        x -> translateNewtype            x
    Hs.DeclDefineInstance x -> translateDefineInstanceDecl x
    Hs.DeclDeriveInstance x -> translateDeriveInstance     x
    Hs.DeclMacroExpr      x -> translateMacroExpr          x
    Hs.DeclForeignImport  x -> translateForeignImportDecl  x
    Hs.DeclForeignImportWrapper x -> translateForeignImportWrapper x
    Hs.DeclForeignImportDynamic x -> translateForeignImportDynamic x
    Hs.DeclFunction       x -> translateFunctionDecl       x
    Hs.DeclPatSyn         x -> translatePatSyn             x
    Hs.DeclUnionGetter    x -> translateUnionGetter        x
    Hs.DeclUnionSetter    x -> translateUnionSetter        x
    Hs.DeclVar            x -> translateDeclVar            x

translateDeclTypSyn :: Hs.TypSyn -> SDecl
translateDeclTypSyn d = DTypSyn $ TypeSynonym {
      name    = d.name
    , typ     = translateType d.typ
    , origin  = d.origin
    , comment = d.comment
    }

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
      Hs.InstanceHasFlam struct fty i ->
        DInst Instance{
            clss    = Flam_Offset_class
          , args    = [ translateType fty, TCon struct.name ]
          , super   = []
          , types   = []
          , comment = defInst.comment
          , decs    = [ ( Flam_Offset_offset
                        , ELam "_ty" $ EIntegral (toInteger i) Nothing)
                      ]
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
            clss    = ToFunPtr_class
          , args    = [translateType inst.typ]
          , super   = []
          , types   = []
          , comment = defInst.comment
          , decs    = [ ( ToFunPtr_toFunPtr
                        , EFree $ Hs.InternalName inst.body
                        )
                      ]
          }
      Hs.InstanceFromFunPtr inst ->
        DInst Instance{
            clss    = FromFunPtr_class
          , args    = [translateType inst.typ]
          , super   = []
          , types   = []
          , comment = defInst.comment
          , decs    = [ ( FromFunPtr_fromFunPtr
                        , EFree $ Hs.InternalName inst.body
                        )
                      ]
          }

translateDeclData :: Hs.Struct n -> SDecl
translateDeclData struct = DRecord Record{
      typ     = struct.name
    , con     = struct.constr
    , deriv   = []
    , comment = struct.comment
    , origin  = case struct.origin of
                  Just origin -> origin
                  Nothing     -> panicPure "Missing structOrigin"
    , fields  = [
          Field{
              name    = f.name
            , typ     = translateType f.typ
            , origin  = f.origin
            , comment = f.comment
            }
        | f <- toList struct.fields
        ]
    }

translateDeclEmpty :: Hs.EmptyData -> SDecl
translateDeclEmpty d = DEmptyData EmptyData{
      name    = d.name
    , origin  = d.origin
    , comment = d.comment
    }

translateNewtype :: Hs.Newtype -> SDecl
translateNewtype n = DNewtype Newtype{
      name    = n.name
    , con     = n.constr
    , origin  = n.origin
    , deriv   = []
    , comment = n.comment
    , field   = Field {
          name    = n.field.name
        , typ     = translateType n.field.typ
        , origin  = n.field.origin
        , comment = n.field.comment
        }
    }

translateDeriveInstance :: Hs.DeriveInstance -> SDecl
translateDeriveInstance deriv = DDerivingInstance DerivingInstance {
      strategy = fmap translateType deriv.strategy
    , typ      = TApp (translateTypeClass deriv.clss) (TCon deriv.name)
    , comment  = deriv.comment
    }

translateTypeClass :: Hs.TypeClass -> ClosedType
translateTypeClass = \case
    Hs.Bitfield   -> TGlobal Bitfield_class
    Hs.Bits       -> TGlobal Bits_class
    Hs.Bounded    -> TGlobal Bounded_class
    Hs.Enum       -> TGlobal Enum_class
    Hs.Eq         -> TGlobal Eq_class
    Hs.FiniteBits -> TGlobal FiniteBits_class
    Hs.Floating   -> TGlobal Floating_class
    Hs.Fractional -> TGlobal Fractional_class
    Hs.Integral   -> TGlobal Integral_class
    Hs.Ix         -> TGlobal Ix_class
    Hs.Num        -> TGlobal Num_class
    Hs.Ord        -> TGlobal Ord_class
    Hs.Prim       -> TGlobal Prim_class
    Hs.Read       -> TGlobal Read_class
    Hs.ReadRaw    -> TGlobal ReadRaw_class
    Hs.Real       -> TGlobal Real_class
    Hs.RealFloat  -> TGlobal RealFloat_class
    Hs.RealFrac   -> TGlobal RealFrac_class
    Hs.Show       -> TGlobal Show_class
    Hs.StaticSize -> TGlobal StaticSize_class
    Hs.Storable   -> TGlobal Storable_class
    Hs.WriteRaw   -> TGlobal WriteRaw_class
    Hs.HasFFIType -> TGlobal HasFFIType_class

translateForeignImportDecl :: Hs.ForeignImportDecl -> SDecl
translateForeignImportDecl importDecl = DForeignImport ForeignImport{
      parameters = map translateParam importDecl.parameters
    , result     = Result (translateType importDecl.result) Nothing
      -- The rest of the fields are copied over as-is
    , name       = importDecl.name
    , origName   = importDecl.origName
    , callConv   = importDecl.callConv
    , origin     = importDecl.origin
    , comment    = importDecl.comment
    , safety     = importDecl.safety
    }

translateParam :: Hs.FunctionParameter -> Parameter
translateParam param = Parameter {
      typ     = translateType param.typ
    , comment = param.comment
    }

translateForeignImportWrapper :: Hs.ForeignImportWrapper -> SDecl
translateForeignImportWrapper importWrapper = DForeignImport ForeignImport{
      parameters = [
            Parameter {
                typ = translateType importWrapper.funType
              , comment = Nothing
              }
          ]
    , result     = flip Result Nothing $
          TGlobal IO_type `TApp`
          (TGlobal Foreign_FunPtr `TApp`
          translateType importWrapper.funType)
    , name       = importWrapper.name
    , origName   = C.DeclName "wrapper" C.NameKindOrdinary
    , callConv   = CallConvGhcCCall ImportAsValue
    , origin     = importWrapper.origin
    , comment    = importWrapper.comment
    , safety     = Safe
    }

translateForeignImportDynamic :: Hs.ForeignImportDynamic -> SDecl
translateForeignImportDynamic importDyn = DForeignImport ForeignImport{
      parameters = [
            Parameter {
                typ =
                    TGlobal Foreign_FunPtr `TApp`
                    translateType importDyn.funType
              , comment = Nothing
              }
          ]
    , result     = Result (translateType importDyn.funType) Nothing
    , name       = importDyn.name
    , origName   = C.DeclName "dynamic" C.NameKindOrdinary
    , callConv   = CallConvGhcCCall ImportAsValue
    , origin     = importDyn.origin
    , comment    = importDyn.comment
    , safety     = Safe
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
      typ     = translateType patSyn.typ
    , rhs     =
      case patSyn.constr of
        Just c -> PEApps c [PELit patSyn.value]
        Nothing -> PELit patSyn.value
      -- The other fields are copied as-is
    , name    = patSyn.name
    , origin  = patSyn.origin
    , comment = patSyn.comment
    }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

translateType :: Hs.HsType -> ClosedType
translateType = \case
    Hs.HsPrimType t                  -> TGlobal (PrimType t)
    Hs.HsTypRef r _                  -> TCon r
    Hs.HsConstArray n t              -> TGlobal ConstantArray `TApp` TLit n `TApp` (translateType t)
    Hs.HsIncompleteArray t           -> TGlobal IncompleteArray `TApp` (translateType t)
    Hs.HsPtr t                       -> TApp (TGlobal Foreign_Ptr) (translateType t)
    Hs.HsFunPtr t                    -> TApp (TGlobal Foreign_FunPtr) (translateType t)
    Hs.HsStablePtr t                 -> TApp (TGlobal Foreign_StablePtr) (translateType t)
    Hs.HsConstPtr t                  -> TApp (TGlobal ConstPtr_type) (translateType t)
    Hs.HsIO t                        -> TApp (TGlobal IO_type) (translateType t)
    Hs.HsFun a b                     -> TFun (translateType a) (translateType b)
    Hs.HsExtBinding r c hs _         -> TExt r c hs
    Hs.HsByteArray                   -> TGlobal ByteArray_type
    Hs.HsSizedByteArray n m          -> TGlobal SizedByteArray_type `TApp` TLit n `TApp` TLit m
    Hs.HsBlock t                     -> TGlobal Block_type `TApp` translateType t
    Hs.HsComplexType t               -> TApp (TGlobal ComplexType) (translateType (HsPrimType t))
    Hs.HsStrLit s                    -> TStrLit s
    Hs.HsWithFlam x y -> TApp
                                          (TApp
                                            (TGlobal WithFlam)
                                            (translateType x))
                                          (translateType y)

{-------------------------------------------------------------------------------
  'Storable'
-------------------------------------------------------------------------------}

translateStorableInstance ::
     Hs.Struct n
  -> Hs.StorableInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateStorableInstance struct inst mbComment = Instance{
      clss    = Storable_class
    , args    = [TCon struct.name]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [
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
translateHasCFieldInstance inst mbComment = Instance {
      clss    = HasCField_class
    , args    = [parent, fieldLit]
    , super   = []
    , comment = mbComment
    , types   = [ ( HasCField_CFieldType
                  , [parent, fieldLit], fieldType)
                ]
    , decs    = [ ( HasCField_offset#
                  , EUnusedLam $ EUnusedLam $ EIntegral o Nothing
                  )
                ]
    }
  where
    parent    = translateType inst.parentType
    fieldLit  = translateType $ HsStrLit $ T.unpack $ Hs.getName inst.fieldName
    fieldType = translateType inst.cFieldType
    o         = fromIntegral inst.fieldOffset

{-------------------------------------------------------------------------------
  'HasCBitfield'
-------------------------------------------------------------------------------}

translateHasCBitfieldInstance ::
     Hs.HasCBitfieldInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasCBitfieldInstance inst mbComment = Instance{
      clss    = HasCBitfield_class
    , args    = [parent, fieldLit]
    , super   = []
    , comment = mbComment
    , types   = [ ( HasCBitfield_CBitfieldType
                  , [parent, fieldLit], fieldType
                  )
                ]
    , decs    = [ ( HasCBitfield_bitOffset#
                  , EUnusedLam $ EUnusedLam $ EIntegral o Nothing
                  )
                , ( HasCBitfield_bitWidth#
                  , EUnusedLam $ EUnusedLam $ EIntegral w Nothing
                  )
                ]
    }
  where
    parent    = translateType inst.parentType
    fieldLit  = translateType $ HsStrLit $ T.unpack $ Hs.getName inst.fieldName
    fieldType = translateType inst.cBitfieldType
    o         = fromIntegral inst.bitOffset
    w         = fromIntegral inst.bitWidth

translateHasFieldInstance ::
     Hs.HasFieldInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasFieldInstance inst mbComment = Instance{
      clss   = HasField_class
    , args    = [fieldLit, parentPtr, tyPtr]
    , types   = []
    , comment = mbComment
    , super   = [ ( NomEq_class
                  , [ tyTypeVar
                    , TGlobal fieldTypeGlobal `TApp` parent `TApp` fieldLit
                    ]
                  )
                ]
    , decs    = [ ( HasField_getField
                  , EGlobal ptrToFieldGlobal `EApp`
                      (EGlobal Proxy_constructor `ETypeApp` fieldLit)
                  )
                ]
    }
  where
    (fieldTypeGlobal, ptrToFieldGlobal, tyPtr) =
      case inst.deriveVia of
        Hs.ViaHasCField -> (
            HasCField_CFieldType
          , HasCField_ptrToCField
          , TGlobal Foreign_Ptr `TApp` tyTypeVar
          )
        Hs.ViaHasCBitfield -> (
            HasCBitfield_CBitfieldType
          , HasCBitfield_ptrToCBitfield
          , TGlobal HasCBitfield_BitfieldPtr `TApp` parent `TApp` fieldLit
          )

    parent    = translateType inst.parentType
    parentPtr = TGlobal Foreign_Ptr `TApp` parent
    fieldLit  = translateType $ HsStrLit $ T.unpack $ Hs.getName inst.fieldName
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
              typ     = TCon getter.constr
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
              typ     = translateType setter.typ
            , comment = Nothing
            }
        ]
    }

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

translateDeclVar :: Hs.Var -> SDecl
translateDeclVar var = DBinding Binding {
      name      = var.name
    , parameters= []
    , result    = Result var.typ Nothing
    , body      = var.expr
    , pragmas   = var.pragmas
    , comment   = var.comment
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
      clss    = CEnum_class
    , args    = [tcon]
    , super   = []
    , types   = [(CEnumZ_tycon, [tcon], translateType fTyp)]
    , comment = mbComment
    , decs    = [
          (CEnum_toCEnum            , ECon struct.constr)
        , (CEnum_fromCEnum          , EFree fname)
        , (CEnum_declaredValues     , EUnusedLam declaredValuesE)
        , (CEnum_showsUndeclared    , EApp (EGlobal CEnum_showsWrappedUndeclared) dconStrE)
        , (CEnum_readPrecUndeclared , EApp (EGlobal CEnum_readPrecWrappedUndeclared) dconStrE)
        ] ++ seqDecs
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
      clss    = SequentialCEnum_class
    , args    = [tcon]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [
          (SequentialCEnum_minDeclaredValue, ECon nameMin)
        , (SequentialCEnum_maxDeclaredValue, ECon nameMax)
        ]
    }
  where
    tcon :: ClosedType
    tcon = TCon struct.name

translateCEnumInstanceShow ::
     Hs.Struct (S Z)
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstanceShow struct mbComment = Instance {
      clss    = Show_class
    , args    = [tcon]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [(Show_showsPrec, EGlobal CEnum_showsCEnum)]
    }
  where
    tcon :: ClosedType
    tcon = TCon struct.name

translateCEnumInstanceRead ::
     Hs.Struct (S Z)
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstanceRead struct mbComment = Instance {
      clss    = Read_class
    , args    = [tcon]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [
          (Read_readPrec     , EGlobal CEnum_readPrecCEnum)
        , (Read_readList     , EGlobal Read_readListDefault)
        , (Read_readListPrec , EGlobal Read_readListPrecDefault)
        ]
    }
  where
    tcon :: ClosedType
    tcon = TCon struct.name
