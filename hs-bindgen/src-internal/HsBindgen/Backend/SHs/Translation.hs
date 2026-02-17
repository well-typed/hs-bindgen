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
import HsBindgen.Backend.Global
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
            clss    = Inst.Flam_Offset
          , args    = [ translateType fty, TCon struct.name ]
          , super   = []
          , types   = []
          , comment = defInst.comment
          , decs    = [ ( bindgenGlobalExpr Flam_Offset_offset
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
            clss    = Inst.ToFunPtr
          , args    = [translateType inst.typ]
          , super   = []
          , types   = []
          , comment = defInst.comment
          , decs    = [ ( bindgenGlobalExpr ToFunPtr_toFunPtr
                        , EFree $ Hs.InternalName inst.body
                        )
                      ]
          }
      Hs.InstanceFromFunPtr inst ->
        DInst Instance{
            clss    = Inst.FromFunPtr
          , args    = [translateType inst.typ]
          , super   = []
          , types   = []
          , comment = defInst.comment
          , decs    = [ ( bindgenGlobalExpr FromFunPtr_fromFunPtr
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
    , typ      = TApp (TClass deriv.clss) (TCon deriv.name)
    , comment  = deriv.comment
    }

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
          tBindgenGlobal IO_type `TApp`
          (tBindgenGlobal Foreign_FunPtr_type `TApp`
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
                    tBindgenGlobal Foreign_FunPtr_type `TApp`
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
    Hs.HsPrimType t          -> translatePrimType t
    Hs.HsTypRef r _          -> TCon r
    Hs.HsConstArray n t      -> tBindgenGlobal ConstantArray_type `TApp` TLit n `TApp` (translateType t)
    Hs.HsIncompleteArray t   -> tBindgenGlobal IncompleteArray_type `TApp` (translateType t)
    Hs.HsPtr t               -> TApp (tBindgenGlobal Foreign_Ptr_type) (translateType t)
    Hs.HsFunPtr t            -> TApp (tBindgenGlobal Foreign_FunPtr_type) (translateType t)
    Hs.HsStablePtr t         -> TApp (tBindgenGlobal Foreign_StablePtr_type) (translateType t)
    Hs.HsPtrConst t          -> TApp (tBindgenGlobal PtrConst_type) (translateType t)
    Hs.HsIO t                -> TApp (tBindgenGlobal IO_type) (translateType t)
    Hs.HsFun a b             -> TFun (translateType a) (translateType b)
    Hs.HsExtBinding r c hs _ -> TExt r c hs
    Hs.HsByteArray           -> tBindgenGlobal ByteArray_type
    Hs.HsSizedByteArray n m  -> tBindgenGlobal SizedByteArray_type `TApp` TLit n `TApp` TLit m
    Hs.HsBlock t             -> tBindgenGlobal Block_type `TApp` translateType t
    Hs.HsComplexType t       -> TApp (tBindgenGlobal Complex_type) (translateType (HsPrimType t))
    Hs.HsStrLit s            -> TStrLit s
    Hs.HsWithFlam x y        ->
      TApp (TApp (tBindgenGlobal Flam_WithFlam_type) (translateType x)) (translateType y)
    Hs.HsEquivStorable t     -> TApp (tBindgenGlobal EquivStorable_type) (translateType t)

{-------------------------------------------------------------------------------
  @StaticSize@, @ReadRaw@, @WriteRaw@
-------------------------------------------------------------------------------}

translateStaticSizeInstance ::
     Hs.Struct n
  -> Hs.StaticSizeInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateStaticSizeInstance struct inst mbComment = Instance{
      clss    = Inst.StaticSize
    , args    = [TCon struct.name]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [
          (bindgenGlobalExpr StaticSize_staticSizeOf    , EUnusedLam $ eInt inst.staticSizeOf)
        , (bindgenGlobalExpr StaticSize_staticAlignment , EUnusedLam $ eInt inst.staticAlignment)
        ]
    }

translateReadRawInstance ::
     Hs.Struct n
  -> Hs.ReadRawInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateReadRawInstance struct inst mbComment = Instance{
      clss    = Inst.ReadRaw
    , args    = [TCon struct.name]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [(bindgenGlobalExpr ReadRaw_readRaw, readRaw)]
    }
  where
    readRaw = lambda (idiom structCon translateReadRawCField) inst.readRaw

translateWriteRawInstance ::
     Hs.Struct n
  -> Hs.WriteRawInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateWriteRawInstance struct inst mbComment = Instance{
      clss    = Inst.WriteRaw
    , args    = [TCon struct.name]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [(bindgenGlobalExpr WriteRaw_writeRaw, writeRaw)]
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
          eBindgenGlobal Proxy_constructor `ETypeApp` translateType field
        , EBound ptr
        ]
    Hs.ReadRawCBitfield field ptr ->
      appMany HasCBitfield_peek [
          eBindgenGlobal Proxy_constructor `ETypeApp` translateType field
        , EBound ptr
        ]
    Hs.ReadRawByteOff ptr i ->
      appMany ReadRaw_readRawByteOff [EBound ptr, eInt i]

translateWriteRawCField :: Hs.WriteRawCField ctx -> SExpr ctx
translateWriteRawCField = \case
    Hs.WriteRawCField field ptr x ->
      appMany HasCField_writeRaw [
          eBindgenGlobal Proxy_constructor `ETypeApp` translateType field
        , EBound ptr
        , EBound x
        ]
    Hs.WriteRawCBitfield field ptr x ->
      appMany HasCBitfield_poke [
          eBindgenGlobal Proxy_constructor `ETypeApp` translateType field
        , EBound ptr
        , EBound x
        ]
    Hs.WriteRawByteOff ptr i x ->
      appMany WriteRaw_writeRawByteOff [EBound ptr, eInt i, EBound x]

translatePrimType :: Hs.HsPrimType -> SType ctx
translatePrimType = \case
    HsPrimVoid    -> tBindgenGlobal Void_type
    HsPrimUnit    -> TBoxedOpenTup 0
    HsPrimChar    -> tBindgenGlobal Char_type
    HsPrimInt     -> tBindgenGlobal Int_type
    HsPrimDouble  -> tBindgenGlobal Double_type
    HsPrimFloat   -> tBindgenGlobal Float_type
    HsPrimBool    -> tBindgenGlobal Bool_type
    HsPrimInt8    -> tBindgenGlobal Int8_type
    HsPrimInt16   -> tBindgenGlobal Int16_type
    HsPrimInt32   -> tBindgenGlobal Int32_type
    HsPrimInt64   -> tBindgenGlobal Int64_type
    HsPrimWord    -> tBindgenGlobal Word_type
    HsPrimWord8   -> tBindgenGlobal Word8_type
    HsPrimWord16  -> tBindgenGlobal Word16_type
    HsPrimWord32  -> tBindgenGlobal Word32_type
    HsPrimWord64  -> tBindgenGlobal Word64_type
    HsPrimCChar   -> tBindgenGlobal CChar_type
    HsPrimCSChar  -> tBindgenGlobal CSChar_type
    HsPrimCUChar  -> tBindgenGlobal CUChar_type
    HsPrimCShort  -> tBindgenGlobal CShort_type
    HsPrimCUShort -> tBindgenGlobal CUShort_type
    HsPrimCInt    -> tBindgenGlobal CInt_type
    HsPrimCUInt   -> tBindgenGlobal CUInt_type
    HsPrimCLong   -> tBindgenGlobal CLong_type
    HsPrimCULong  -> tBindgenGlobal CULong_type
    HsPrimCLLong  -> tBindgenGlobal CLLong_type
    HsPrimCULLong -> tBindgenGlobal CULLong_type
    HsPrimCBool   -> tBindgenGlobal CBool_type
    HsPrimCFloat  -> tBindgenGlobal CFloat_type
    HsPrimCDouble -> tBindgenGlobal CDouble_type

{-------------------------------------------------------------------------------
  'Storable'
-------------------------------------------------------------------------------}

translateStorableInstance ::
     Hs.Struct n
  -> Hs.StorableInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateStorableInstance struct inst mbComment = Instance{
      clss    = Inst.Storable
    , args    = [TCon struct.name]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = map (first bindgenGlobalExpr) [
          (Storable_sizeOf    , EUnusedLam $ eInt inst.sizeOf)
        , (Storable_alignment , EUnusedLam $ eInt inst.alignment)
        , (Storable_peek      , peek)
        , (Storable_poke      , poke)
        ]
    }
  where
    peek = lambda (idiom structCon translatePeekCField) inst.peek
    poke = lambda (lambda (translateElimStruct (doAll translatePokeCField))) inst.poke

translatePeekCField :: Hs.PeekCField ctx -> SExpr ctx
translatePeekCField (Hs.PeekCField field ptr) =
    appMany HasCField_peek [eBindgenGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr]
translatePeekCField (Hs.PeekCBitfield field ptr) =
    appMany HasCBitfield_peek [eBindgenGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr]
translatePeekCField (Hs.PeekByteOff ptr i) =
    appMany Storable_peekByteOff [EBound ptr, eInt i]

translatePokeCField :: Hs.PokeCField ctx -> SExpr ctx
translatePokeCField (Hs.PokeCField field ptr x) =
    appMany HasCField_poke [eBindgenGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr, EBound x]
translatePokeCField (Hs.PokeCBitfield field ptr x) =
    appMany HasCBitfield_poke [eBindgenGlobal Proxy_constructor `ETypeApp` translateType field, EBound ptr, EBound x]
translatePokeCField (Hs.PokeByteOff ptr i x) =
    appMany Storable_pokeByteOff [EBound ptr, eInt i, EBound x]

{-------------------------------------------------------------------------------
  'HasCField'
-------------------------------------------------------------------------------}

translateHasCFieldInstance ::
     Hs.HasCFieldInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasCFieldInstance inst mbComment = Instance {
      clss    = Inst.HasCField
    , args    = [parent, fieldLit]
    , super   = []
    , comment = mbComment
    , types   = [ ( bindgenGlobalType HasCField_CFieldType
                  , [parent, fieldLit], fieldType)
                ]
    , decs    = [ ( bindgenGlobalExpr HasCField_offset#
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
      clss    = Inst.HasCBitfield
    , args    = [parent, fieldLit]
    , super   = []
    , comment = mbComment
    , types   = [ (bindgenGlobalType HasCBitfield_CBitfieldType
                  , [parent, fieldLit], fieldType
                  )
                ]
    , decs    = [ ( bindgenGlobalExpr HasCBitfield_bitfieldOffset#
                  , EUnusedLam $ EUnusedLam $ EIntegral o Nothing
                  )
                , ( bindgenGlobalExpr HasCBitfield_bitfieldWidth#
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
      clss   = Inst.HasField
    , args    = [fieldLit, parentPtr, tyPtr]
    , types   = []
    , comment = mbComment
    , super   = [ TApp (TApp TEq tyTypeVar) field ]
    , decs    = [ ( bindgenGlobalExpr HasField_getField
                  , eBindgenGlobal ptrToFieldGlobal `EApp`
                      (eBindgenGlobal Proxy_constructor `ETypeApp` fieldLit)
                  )
                ]
    }
  where
    (ptrToFieldGlobal, tyPtr) =
      case inst.deriveVia of
        Hs.ViaHasCField -> (
            HasCField_fromPtr
          , tBindgenGlobal Foreign_Ptr_type `TApp` tyTypeVar
          )
        Hs.ViaHasCBitfield -> (
            HasCBitfield_toPtr
          , tBindgenGlobal HasCBitfield_BitfieldPtr_type `TApp` tyTypeVar
          )

    parent    = translateType inst.parentType
    parentPtr = tBindgenGlobal Foreign_Ptr_type `TApp` parent
    field     = translateType inst.fieldType
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
    , body       = eBindgenGlobal ByteArray_getUnionPayload
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
    , body       = eBindgenGlobal ByteArray_setUnionPayload
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
      clss    = Inst.CEnum
    , args    = [tcon]
    , super   = []
    , types   = [(bindgenGlobalType CEnumZ_type, [tcon], translateType fTyp)]
    , comment = mbComment
    , decs    = map (first bindgenGlobalExpr) [
          (CEnum_toCEnum            , ECon struct.constr)
        , (CEnum_fromCEnum          , fromCEnumE)
        , (CEnum_declaredValues     , EUnusedLam declaredValuesE)
        , (CEnum_showsUndeclared    , EApp (eBindgenGlobal CEnum_showsWrappedUndeclared) dconStrE)
        , (CEnum_readPrecUndeclared , EApp (eBindgenGlobal CEnum_readPrecWrappedUndeclared) dconStrE)
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
          eBindgenGlobal HasField_getField `ETypeApp` translateType (Hs.HsStrLit "unwrap")
        PrefixedFieldNames ->
          EFree fname

    declaredValuesE :: SExpr ctx
    declaredValuesE = EApp (eBindgenGlobal CEnum_declaredValuesFromList) $ EList [
        EBoxedClosedTup [
            EIntegral v Nothing
          , if null names
              then EApp (eBindgenGlobal NonEmpty_singleton) (EString name)
              else
                EInfix
                  InfixNonEmpty_constructor
                  (EString name)
                  (EList (EString <$> names))
          ]
      | (v, name :| names) <- Map.toList vMap
      ]

    seqDecs :: [(Global GExpr, ClosedExpr)]
    seqDecs
      | isSequential = [
            (bindgenGlobalExpr CEnum_isDeclared, eBindgenGlobal CEnum_seqIsDeclared)
          , (bindgenGlobalExpr CEnum_mkDeclared, eBindgenGlobal CEnum_seqMkDeclared)
          ]
      | otherwise = []

translateSequentialCEnum ::
     Hs.Struct (S Z)
  -> Hs.Name Hs.NsConstr
  -> Hs.Name Hs.NsConstr
  -> Maybe HsDoc.Comment
  -> Instance
translateSequentialCEnum struct nameMin nameMax mbComment = Instance {
      clss    = Inst.SequentialCEnum
    , args    = [tcon]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [
          (bindgenGlobalExpr SequentialCEnum_minDeclaredValue, ECon nameMin)
        , (bindgenGlobalExpr SequentialCEnum_maxDeclaredValue, ECon nameMax)
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
      clss    = Inst.Show
    , args    = [tcon]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [(bindgenGlobalExpr Show_showsPrec, eBindgenGlobal CEnum_showsCEnum)]
    }
  where
    tcon :: ClosedType
    tcon = TCon struct.name

translateCEnumInstanceRead ::
     Hs.Struct (S Z)
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstanceRead struct mbComment = Instance {
      clss    = Inst.Read
    , args    = [tcon]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = map (bimap bindgenGlobalExpr eBindgenGlobal) [
          (Read_readPrec     , CEnum_readPrecCEnum)
        , (Read_readList     , Read_readListDefault)
        , (Read_readListPrec , Read_readListPrecDefault)
        ]
    }
  where
    tcon :: ClosedType
    tcon = TCon struct.name
