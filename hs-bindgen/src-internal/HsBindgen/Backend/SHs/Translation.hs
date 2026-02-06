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
import HsBindgen.Config.Prelims (FieldNamingStrategy (..))
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
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
      Hs.InstanceStaticSize struct i ->
        DInst $ translateStaticSizeInstance struct i defInst.comment
      Hs.InstanceReadRaw struct i ->
        DInst $ translateReadRawInstance struct i defInst.comment
      Hs.InstanceWriteRaw struct i ->
        DInst $ translateWriteRawInstance struct i defInst.comment
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
      Hs.InstanceCEnum struct fTyp vMap isSequential fieldNamingStrategy ->
        DInst $ translateCEnumInstance struct fTyp vMap isSequential fieldNamingStrategy defInst.comment
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
    , deriv   = [(Hs.DeriveStock, [Generic_class])]
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
    , deriv   = [(Hs.DeriveStock, [Generic_class])]
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

translateTypeClass :: Inst.TypeClass -> ClosedType
translateTypeClass = \case
    Inst.Bitfield        -> TGlobal Bitfield_class
    Inst.Bits            -> TGlobal Bits_class
    Inst.Bounded         -> TGlobal Bounded_class
    Inst.CEnum           -> TGlobal CEnum_class
    Inst.Enum            -> TGlobal Enum_class
    Inst.Eq              -> TGlobal Eq_class
    Inst.FiniteBits      -> TGlobal FiniteBits_class
    Inst.Floating        -> TGlobal Floating_class
    Inst.Fractional      -> TGlobal Fractional_class
    Inst.FromFunPtr      -> TGlobal FromFunPtr_class
    Inst.HasCBitField    -> TGlobal HasCBitfield_class
    Inst.HasCField       -> TGlobal HasCField_class
    Inst.HasFFIType      -> TGlobal HasFFIType_class
    Inst.HasField        -> TGlobal HasField_class
    Inst.Flam_Offset     -> TGlobal Flam_Offset_class
    Inst.Integral        -> TGlobal Integral_class
    Inst.Ix              -> TGlobal Ix_class
    Inst.Num             -> TGlobal Num_class
    Inst.Ord             -> TGlobal Ord_class
    Inst.Prim            -> TGlobal Prim_class
    Inst.Read            -> TGlobal Read_class
    Inst.ReadRaw         -> TGlobal ReadRaw_class
    Inst.Real            -> TGlobal Real_class
    Inst.RealFloat       -> TGlobal RealFloat_class
    Inst.RealFrac        -> TGlobal RealFrac_class
    Inst.SequentialCEnum -> TGlobal SequentialCEnum_class
    Inst.Show            -> TGlobal Show_class
    Inst.StaticSize      -> TGlobal StaticSize_class
    Inst.Storable        -> TGlobal Storable_class
    Inst.ToFunPtr        -> TGlobal ToFunPtr_class
    Inst.WriteRaw        -> TGlobal WriteRaw_class

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
    Hs.HsPrimType t          -> TGlobal (PrimType t)
    Hs.HsTypRef r _          -> TCon r
    Hs.HsConstArray n t      -> TGlobal ConstantArray `TApp` TLit n `TApp` (translateType t)
    Hs.HsIncompleteArray t   -> TGlobal IncompleteArray `TApp` (translateType t)
    Hs.HsPtr t               -> TApp (TGlobal Foreign_Ptr) (translateType t)
    Hs.HsFunPtr t            -> TApp (TGlobal Foreign_FunPtr) (translateType t)
    Hs.HsStablePtr t         -> TApp (TGlobal Foreign_StablePtr) (translateType t)
    Hs.HsPtrConst t          -> TApp (TGlobal PtrConst_type) (translateType t)
    Hs.HsIO t                -> TApp (TGlobal IO_type) (translateType t)
    Hs.HsFun a b             -> TFun (translateType a) (translateType b)
    Hs.HsExtBinding r c hs _ -> TExt r c hs
    Hs.HsByteArray           -> TGlobal ByteArray_type
    Hs.HsSizedByteArray n m  -> TGlobal SizedByteArray_type `TApp` TLit n `TApp` TLit m
    Hs.HsBlock t             -> TGlobal Block_type `TApp` translateType t
    Hs.HsComplexType t       -> TApp (TGlobal ComplexType) (translateType (HsPrimType t))
    Hs.HsStrLit s            -> TStrLit s
    Hs.HsWithFlam x y        ->
      TApp (TApp (TGlobal WithFlam) (translateType x)) (translateType y)
    Hs.HsEquivStorable t     -> TApp (TGlobal EquivStorable_type) (translateType t)

{-------------------------------------------------------------------------------
  @StaticSize@, @ReadRaw@, @WriteRaw@
-------------------------------------------------------------------------------}

translateStaticSizeInstance ::
     Hs.Struct n
  -> Hs.StaticSizeInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateStaticSizeInstance struct inst mbComment = Instance{
      clss    = StaticSize_class
    , args    = [TCon struct.name]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [
          (StaticSize_staticSizeOf    , EUnusedLam $ EInt inst.staticSizeOf)
        , (StaticSize_staticAlignment , EUnusedLam $ EInt inst.staticAlignment)
        ]
    }

translateReadRawInstance ::
     Hs.Struct n
  -> Hs.ReadRawInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateReadRawInstance struct inst mbComment = Instance{
      clss    = ReadRaw_class
    , args    = [TCon struct.name]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [(ReadRaw_readRaw, readRaw)]
    }
  where
    readRaw = lambda (idiom structCon translateReadRawCField) inst.readRaw

translateWriteRawInstance ::
     Hs.Struct n
  -> Hs.WriteRawInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateWriteRawInstance struct inst mbComment = Instance{
      clss    = WriteRaw_class
    , args    = [TCon struct.name]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [(WriteRaw_writeRaw, writeRaw)]
    }
  where
    writeRaw =
      lambda
        (lambda (translateElimStruct (doAll translateWriteRawCField)))
        inst.writeRaw

translateReadRawCField :: Hs.ReadRawCField ctx -> SExpr ctx
translateReadRawCField = \case
    Hs.ReadRawCField field ptr ->
      appMany HasCField_readRaw [
          EGlobal Proxy_constructor `ETypeApp` translateType field
        , EBound ptr
        ]
    Hs.ReadRawCBitfield field ptr ->
      appMany HasCBitfield_peek [
          EGlobal Proxy_constructor `ETypeApp` translateType field
        , EBound ptr
        ]
    Hs.ReadRawByteOff ptr i ->
      appMany ReadRaw_readRawByteOff [EBound ptr, EInt i]

translateWriteRawCField :: Hs.WriteRawCField ctx -> SExpr ctx
translateWriteRawCField = \case
    Hs.WriteRawCField field ptr x ->
      appMany HasCField_writeRaw [
          EGlobal Proxy_constructor `ETypeApp` translateType field
        , EBound ptr
        , EBound x
        ]
    Hs.WriteRawCBitfield field ptr x ->
      appMany HasCBitfield_poke [
          EGlobal Proxy_constructor `ETypeApp` translateType field
        , EBound ptr
        , EBound x
        ]
    Hs.WriteRawByteOff ptr i x ->
      appMany WriteRaw_writeRawByteOff [EBound ptr, EInt i, EBound x]

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
translatePeekCField (Hs.PeekCField field ptr) = appMany HasCField_peek [EGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr]
translatePeekCField (Hs.PeekCBitfield field ptr) = appMany HasCBitfield_peek [EGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr]
translatePeekCField (Hs.PeekByteOff ptr i) = appMany Storable_peekByteOff [EBound ptr, EInt i]

translatePokeCField :: Hs.PokeCField ctx -> SExpr ctx
translatePokeCField (Hs.PokeCField field ptr x) = appMany HasCField_poke [EGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr, EBound x]
translatePokeCField (Hs.PokeCBitfield field ptr x) = appMany HasCBitfield_poke [EGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr, EBound x]
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
    , decs    = [ ( HasCBitfield_bitfieldOffset#
                  , EUnusedLam $ EUnusedLam $ EIntegral o Nothing
                  )
                , ( HasCBitfield_bitfieldWidth#
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

{-------------------------------------------------------------------------------
  'HasField'
-------------------------------------------------------------------------------}

translateHasFieldInstance ::
     Hs.HasFieldInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasFieldInstance inst mbComment = Instance{
      clss   = HasField_class
    , args    = [fieldLit, parentPtr, tyPtr]
    , types   = []
    , comment = mbComment
    , super   = []
    , decs    = [ ( HasField_getField
                  , EGlobal ptrToFieldGlobal `EApp`
                      (EGlobal Proxy_constructor `ETypeApp` fieldLit)
                  )
                ]
    }
  where
    (ptrToFieldGlobal, tyPtr) =
      case inst.deriveVia of
        Hs.ViaHasCField -> (
            HasCField_fromPtr
          , TGlobal Foreign_Ptr `TApp` field
          )
        Hs.ViaHasCBitfield -> (
            HasCBitfield_toPtr
          , TGlobal HasCBitfield_BitfieldPtr `TApp` field
          )

    parent    = translateType inst.parentType
    parentPtr = TGlobal Foreign_Ptr `TApp` parent
    field     = translateType inst.fieldType
    fieldLit  = translateType $ HsStrLit $ T.unpack $ Hs.getName inst.fieldName

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
  -> FieldNamingStrategy
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstance struct fTyp vMap isSequential fieldNamingStrategy mbComment = Instance {
      clss    = CEnum_class
    , args    = [tcon]
    , super   = []
    , types   = [(CEnumZ_tycon, [tcon], translateType fTyp)]
    , comment = mbComment
    , decs    = [
          (CEnum_toCEnum            , ECon struct.constr)
        , (CEnum_fromCEnum          , fromCEnumE)
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

    -- When using EnableRecordDot, many newtypes will have fields named "unwrap",
    -- which makes the bare identifier "unwrap" ambiguous. We use getField with
    -- type application only in that case. Otherwise, we use the bare field name
    -- directly since it's unique (e.g., unwrapE, unwrapValue).
    fromCEnumE :: ClosedExpr
    fromCEnumE =
      case fieldNamingStrategy of
        EnableRecordDot ->
          EGlobal HasField_getField `ETypeApp` translateType (Hs.HsStrLit "unwrap")
        PrefixedFieldNames ->
          EFree fname

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
