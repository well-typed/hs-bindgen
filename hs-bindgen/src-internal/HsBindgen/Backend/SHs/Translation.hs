{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Simplified HS translation (from high level HS)
module HsBindgen.Backend.SHs.Translation (
    translateDecls,
    translateType,
) where

import Data.ByteString qualified as BS
import Data.Char qualified
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import DeBruijn.Idx

import HsBindgen.Backend.Category
import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Level
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.AST.Expr (FBind (FBind))
import HsBindgen.Backend.SHs.Translation.Common
import HsBindgen.Errors
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Hs qualified as Hs
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.NameHint

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

translateDecls ::
     forall l. Macro.HasTypes l
  => Macro.Lang l
  -> ByCategory_ [Hs.Decl l]
  -> ByCategory_ ([CWrapper], [SDecl])
translateDecls macroLang = fmap go
  where
    go :: [Hs.Decl l] -> ([CWrapper], [SDecl])
    go decls = (wrappers, map (translateDecl macroLang) decls)
      where
        wrappers = getCWrappers decls

-- Find and assemble C sources required by foreign imports.
getCWrappers :: [Hs.Decl l] -> [CWrapper]
getCWrappers decls = mapMaybe getCWrapper decls
  where
    getCWrapper :: Hs.Decl l -> Maybe CWrapper
    getCWrapper = \case
      Hs.DeclForeignImport importDecl ->
        case importDecl.callConv of
          CallConvUserlandCapi w -> Just w
          _otherCallConv         -> Nothing
      _otherDecl -> Nothing

translateDecl :: Macro.HasTypes l => Macro.Lang l -> Hs.Decl l -> SDecl
translateDecl macroLang = \case
  Hs.DeclTypSyn               x -> translateDeclTypSyn            x
  Hs.DeclData                 x -> translateDeclData              x
  Hs.DeclEmpty                x -> translateDeclEmpty             x
  Hs.DeclNewtype              x -> translateNewtype               x
  Hs.DeclDefineInstance       x -> translateDefineInstanceDecl    x
  Hs.DeclDeriveInstance       x -> translateDeriveInstance        x
  Hs.DeclMacroValue           x -> translateMacroValue' macroLang x
  Hs.DeclForeignImport        x -> translateForeignImportDecl     x
  Hs.DeclForeignImportWrapper x -> translateForeignImportWrapper  x
  Hs.DeclForeignImportDynamic x -> translateForeignImportDynamic  x
  Hs.DeclFunction             x -> translateFunctionDecl          x
  Hs.DeclPatSyn               x -> translatePatSyn                x
  Hs.DeclCompletePragma       x -> translateCompletePragma        x
  Hs.DeclVar                  x -> translateDeclVar               x

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
      Hs.InstanceStaticSize name i ->
        DInst $ translateStaticSizeInstance name i defInst.comment
      Hs.InstanceReadRaw struct i ->
        DInst $ translateReadRawInstance struct i defInst.comment
      Hs.InstanceWriteRaw struct i ->
        DInst $ translateWriteRawInstance struct i defInst.comment
      Hs.InstanceStorable struct i ->
        DInst $ translateStorableInstance struct i defInst.comment
      Hs.InstanceHasCField i ->
        DInst $ translateHasCFieldInstance i defInst.comment
      Hs.InstanceHasCBitfield i ->
        DInst $ translateHasCBitfieldInstance i defInst.comment
      Hs.InstanceHasField i ->
        DInst $ translateHasFieldInstance i defInst.comment
      Hs.InstanceHasFieldCompat i ->
        DInst $ translateHasFieldCompatInstance i defInst.comment
      Hs.InstanceHasFieldPtr i ->
        DInst $ translateHasFieldPtrInstance i defInst.comment
      Hs.InstanceHasFlam struct i ->
        DInst Instance{
            clss    = Inst.Flam_Offset
          , args    = [ translateType i.typ, TCon struct.name ]
          , super   = []
          , types   = []
          , comment = defInst.comment
          , decs    = [ ( bindgenGlobalTerm Flam_Offset_offset
                        , ELam (NameHint "_proxy") $
                            EIntegral (toInteger i.offset) Nothing)
                      ]
          }
      Hs.InstanceCEnum struct i ->
        DInst $
          translateCEnumInstance
            struct
            i.fieldType
            i.valueNames
            i.isSequential
            defInst.comment
      Hs.InstanceSequentialCEnum struct i ->
        DInst $
          translateSequentialCEnum struct i.minName i.maxName defInst.comment
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
          , decs    = [ ( bindgenGlobalTerm ToFunPtr_toFunPtr
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
          , decs    = [ ( bindgenGlobalTerm FromFunPtr_fromFunPtr
                        , EFree $ Hs.InternalName inst.body
                        )
                      ]
          }

translateDeclData :: Hs.Struct -> SDecl
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
        | f <- struct.fields
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

translateMacroValue' :: Macro.HasTypes l => Macro.Lang l -> Hs.MacroValue l -> SDecl
translateMacroValue' macroLang macro = DBinding $
    withCLiteralComment $
    macroLang.translateValue
      macro.name
      (fmap macroIdToHsName macro.expr.body)
      macro.comment
  where
    macroIdToHsName :: Id Final -> Hs.TermName
    macroIdToHsName namePair =
        Hs.ExportedName $ Hs.assertNs (Proxy @Hs.NsVar) namePair.hsName

-- | Augment a macro binding's Haddock comment with the C literal representation
-- when the binding body is a character or string literal.
withCLiteralComment :: Binding -> Binding
withCLiteralComment binding =
    case cLiteralText binding.body of
      Nothing  -> binding
      Just lit -> binding
        & #comment .~ Just ((fromMaybe mempty binding.comment) & #literal .~ Just lit)

cLiteralText :: ClosedExpr -> Maybe Text
cLiteralText = \case
    ECChar c    -> Just $ Text.pack $ show (Data.Char.chr (fromIntegral c))
    ECString bs -> Just $ Text.pack $ show (map (Data.Char.chr . fromIntegral) (BS.unpack bs))
    _           -> Nothing

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
    , name       = Hs.InternalName importWrapper.name
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
    , name       = Hs.InternalName importDyn.name
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

translateCompletePragma :: Hs.CompletePragma -> SDecl
translateCompletePragma = DCompletePragma

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

translateType :: Hs.Type -> ClosedType
translateType = \case
    Hs.PrimType t          -> translatePrimType t
    Hs.TypRef r _          -> TCon r
    Hs.ConstArray n t      -> tBindgenGlobal ConstantArray_type `TApp` TLit n `TApp` (translateType t)
    Hs.IncompleteArray t   -> tBindgenGlobal IncompleteArray_type `TApp` (translateType t)
    Hs.PtrArrayElem t      -> tBindgenGlobal Foreign_Ptr_type `TApp` (tBindgenGlobal IsArray_Elem `TApp` translateType t)
    Hs.PtrConstArrayElem t -> tBindgenGlobal PtrConst_type `TApp` (tBindgenGlobal IsArray_Elem `TApp` translateType t)
    Hs.Ptr t               -> TApp (tBindgenGlobal Foreign_Ptr_type) (translateType t)
    Hs.FunPtr t            -> TApp (tBindgenGlobal Foreign_FunPtr_type) (translateType t)
    Hs.StablePtr t         -> TApp (tBindgenGlobal Foreign_StablePtr_type) (translateType t)
    Hs.PtrConst t          -> TApp (tBindgenGlobal PtrConst_type) (translateType t)
    Hs.IO t                -> TApp (tBindgenGlobal IO_type) (translateType t)
    Hs.Fun a b             -> TFun (translateType a) (translateType b)
    Hs.ExtBinding r c hs _ -> TExt r c hs
    Hs.ByteArray           -> tBindgenGlobal ByteArray_type
    Hs.SizedByteArray n m  -> tBindgenGlobal SizedByteArray_type `TApp` TLit n `TApp` TLit m
    Hs.Block t             -> tBindgenGlobal Block_type `TApp` translateType t
    Hs.ComplexType t       -> TApp (tBindgenGlobal Complex_type) (translateType (Hs.PrimType t))
    Hs.StrLit s            -> TStrLit s
    Hs.WithFlam x y        ->
      TApp (TApp (tBindgenGlobal Flam_WithFlam_type) (translateType x)) (translateType y)
    Hs.EquivStorable t     -> TApp (tBindgenGlobal EquivStorable_type) (translateType t)

{-------------------------------------------------------------------------------
  @StaticSize@, @ReadRaw@, @WriteRaw@
-------------------------------------------------------------------------------}

translateStaticSizeInstance ::
     Hs.Name Hs.NsTypeConstr
  -> Hs.StaticSizeInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateStaticSizeInstance name inst mbComment = Instance{
      clss    = Inst.StaticSize
    , args    = [TCon name]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [
          (bindgenGlobalTerm StaticSize_staticSizeOf    , EUnusedLam $ eInt inst.staticSizeOf)
        , (bindgenGlobalTerm StaticSize_staticAlignment , EUnusedLam $ eInt inst.staticAlignment)
        ]
    }

translateReadRawInstance ::
     Hs.Struct
  -> Hs.ReadRawInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateReadRawInstance struct inst mbComment = Instance{
      clss    = Inst.ReadRaw
    , args    = [TCon struct.name]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [(bindgenGlobalTerm ReadRaw_readRaw, readRaw)]
    }
  where
    readRaw = lambda (idiom structCon translateReadRawCField) inst.readRaw

translateWriteRawInstance ::
     Hs.Struct
  -> Hs.WriteRawInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateWriteRawInstance struct inst mbComment = Instance{
      clss    = Inst.WriteRaw
    , args    = [TCon struct.name]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [(bindgenGlobalTerm WriteRaw_writeRaw, writeRaw)]
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

translatePrimType :: Hs.PrimType -> SType ctx
translatePrimType = \case
    Hs.PrimVoid    -> tBindgenGlobal Void_type
    Hs.PrimUnit    -> TUnit
    Hs.PrimChar    -> tBindgenGlobal Char_type
    Hs.PrimInt     -> tBindgenGlobal Int_type
    Hs.PrimDouble  -> tBindgenGlobal Double_type
    Hs.PrimFloat   -> tBindgenGlobal Float_type
    Hs.PrimBool    -> tBindgenGlobal Bool_type
    Hs.PrimInt8    -> tBindgenGlobal Int8_type
    Hs.PrimInt16   -> tBindgenGlobal Int16_type
    Hs.PrimInt32   -> tBindgenGlobal Int32_type
    Hs.PrimInt64   -> tBindgenGlobal Int64_type
    Hs.PrimWord    -> tBindgenGlobal Word_type
    Hs.PrimWord8   -> tBindgenGlobal Word8_type
    Hs.PrimWord16  -> tBindgenGlobal Word16_type
    Hs.PrimWord32  -> tBindgenGlobal Word32_type
    Hs.PrimWord64  -> tBindgenGlobal Word64_type
    Hs.PrimCChar   -> tBindgenGlobal CChar_type
    Hs.PrimCSChar  -> tBindgenGlobal CSChar_type
    Hs.PrimCUChar  -> tBindgenGlobal CUChar_type
    Hs.PrimCShort  -> tBindgenGlobal CShort_type
    Hs.PrimCUShort -> tBindgenGlobal CUShort_type
    Hs.PrimCInt    -> tBindgenGlobal CInt_type
    Hs.PrimCUInt   -> tBindgenGlobal CUInt_type
    Hs.PrimCLong   -> tBindgenGlobal CLong_type
    Hs.PrimCULong  -> tBindgenGlobal CULong_type
    Hs.PrimCLLong  -> tBindgenGlobal CLLong_type
    Hs.PrimCULLong -> tBindgenGlobal CULLong_type
    Hs.PrimCBool   -> tBindgenGlobal CBool_type
    Hs.PrimCFloat  -> tBindgenGlobal CFloat_type
    Hs.PrimCDouble -> tBindgenGlobal CDouble_type

{-------------------------------------------------------------------------------
  'Storable'
-------------------------------------------------------------------------------}

translateStorableInstance ::
     Hs.Struct
  -> Hs.StorableInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateStorableInstance struct inst mbComment = Instance{
      clss    = Inst.Storable
    , args    = [TCon struct.name]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = map (first bindgenGlobalTerm) [
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
    , decs    = [ ( bindgenGlobalTerm HasCField_offset#
                  , EUnusedLam $ EUnusedLam $ EIntegral o Nothing
                  )
                ]
    }
  where
    parent    = translateType inst.parentType
    fieldLit  = translateType $ Hs.StrLit $ Hs.nameToStr inst.fieldName
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
    , decs    = [ ( bindgenGlobalTerm HasCBitfield_bitfieldOffset#
                  , EUnusedLam $ EUnusedLam $ EIntegral o Nothing
                  )
                , ( bindgenGlobalTerm HasCBitfield_bitfieldWidth#
                  , EUnusedLam $ EUnusedLam $ EIntegral w Nothing
                  )
                ]
    }
  where
    parent    = translateType inst.parentType
    fieldLit  = translateType $ Hs.StrLit $ Hs.nameToStr inst.fieldName
    fieldType = translateType inst.cBitfieldType
    o         = fromIntegral inst.bitOffset
    w         = fromIntegral inst.bitWidth

{-------------------------------------------------------------------------------
  'GHC.Records.HasField'
-------------------------------------------------------------------------------}

translateHasFieldInstance ::
     Hs.HasFieldInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasFieldInstance inst mbComment = Instance{
      clss   = Inst.HasField
    , args    = [fieldLit, parent, tyTypeVar]
    , types   = []
    , comment = mbComment
    , super   = [ TApp (TApp TEq tyTypeVar) field ]
    , decs    = [ ( bindgenGlobalTerm HasField_getField
                  , exprGetter
                  )
                ]
    }
  where
    parent    = translateType inst.parentType
    field     = translateType inst.fieldType
    fieldLit  = translateType $ Hs.StrLit $ Hs.nameToStr inst.fieldName

    -- This is not actually a free type variable.
    tyTypeVar = TFree $ Hs.UnsafeName "ty"

    exprGetter :: SExpr Z
    exprGetter = case inst.impl of
      Hs.HasFieldImplUnion -> eBindgenGlobal ByteArray_getUnionPayload
      Hs.HasFieldImplIndirect {nameTopToAnon, nameAnonToTarget} ->
        let strLitTopToAnon    = translateType (Hs.StrLit (Hs.nameToStr nameTopToAnon))
            strLitAnonToTarget = translateType (Hs.StrLit (Hs.nameToStr nameAnonToTarget)) in
        ELam (NameHint "x") $
          EApp (eBindgenGlobal HasField_getField `ETypeApp` strLitAnonToTarget)
               (eBindgenGlobal HasField_getField `ETypeApp` strLitTopToAnon `EApp` EBound IZ)

{-------------------------------------------------------------------------------
  'GHC.Records.Compat.HasField'
-------------------------------------------------------------------------------}

translateHasFieldCompatInstance ::
     Hs.HasFieldCompatInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasFieldCompatInstance inst mbComment = Instance{
      clss   = Inst.HasFieldCompat
    , args    = [fieldLit, parent, tyTypeVar]
    , types   = []
    , comment = mbComment
    , super   = [ TApp (TApp TEq tyTypeVar) field ]
    , decs    = [ ( bindgenGlobalTerm HasFieldCompat_hasField
                  , ELam (NameHint "x") $ appManyExpr (EBoxedTup $ Plus2 0) [
                        exprSetter
                      , exprGetter
                      ]
                  )
                ]
    }
  where
    parent    = translateType inst.parentType
    field     = translateType inst.fieldType
    fieldLit  = translateType $ Hs.StrLit $ Hs.nameToStr inst.fieldName

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1287>
    -- This is not actually a free type variable.
    tyTypeVar = TFree $ Hs.UnsafeName "ty"

    exprGetter :: SExpr (S Z)
    exprGetter = eBindgenGlobal HasField_getField `ETypeApp` fieldLit `EApp` EBound IZ

    -- For the setter we use total record construction. The advantage with
    -- record fields is that they can be used in any order regardless of the
    -- order in which the fields were defined the datatype. This leads to a
    -- simpler implementation compared to non-record construction. However,
    -- because we use record fields we need total record construction instead of
    -- a partial record update because the latter can cause warnings about
    -- ambiguous field names. The implementation cost for record construction
    -- compared to record update is low.
    exprSetter :: SExpr (S Z)
    exprSetter =
        case inst.impl of
          Hs.HasFieldCompatImplRecord {constr, otherFields} ->
            ELam (NameHint "y") $
              ERecCon constr $ concat [
                  [ FBind (Hs.nameToStr inst.fieldName) (EBound IZ) ]
                , map mkFBindIdentity otherFields
                ]
          Hs.HasFieldCompatImplUnion ->
              eBindgenGlobal ByteArray_setUnionPayload
          Hs.HasFieldCompatImplIndirect {nameTopToAnon, nameAnonToTarget} ->
            let strLitTopToAnon = translateType (Hs.StrLit (Hs.nameToStr nameTopToAnon))
                strLitAnonToTarget = translateType (Hs.StrLit (Hs.nameToStr nameAnonToTarget)) in
            ELam (NameHint "y")
              (
                     (          eBindgenGlobal HasFieldCompat_modifyField
                     `ETypeApp` strLitTopToAnon
                     )
              `EApp` (EBound (IS IZ))
              `EApp` (ELam (NameHint "z")
                        (
                                   eBindgenGlobal HasFieldCompat_setField
                        `ETypeApp` strLitAnonToTarget
                        `EApp`     EBound IZ
                        `EApp`     EBound (IS IZ)
                        )
                      )
              )
      where
        -- An 'FBind' that leaves the original field unchanged
        mkFBindIdentity :: Hs.Name Hs.NsVar -> FBind (S (S n))
        mkFBindIdentity fieldName = FBind (Hs.nameToStr fieldName) $
                      eBindgenGlobal HasField_getField
            `ETypeApp` (translateType (Hs.StrLit (Hs.nameToStr fieldName)))
            `EApp`      EBound (IS IZ)

{-------------------------------------------------------------------------------
  'GHC.Records.HasField' for the pointer manipulation API
-------------------------------------------------------------------------------}

translateHasFieldPtrInstance ::
     Hs.HasFieldPtrInstance
  -> Maybe HsDoc.Comment
  -> Instance
translateHasFieldPtrInstance inst mbComment = Instance{
      clss   = Inst.HasField
    , args    = [fieldLit, parentPtr, tyPtr]
    , types   = []
    , comment = mbComment
    , super   = [ TApp (TApp TEq tyTypeVar) field ]
    , decs    = [ ( bindgenGlobalTerm HasField_getField
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
    fieldLit  = translateType $ Hs.StrLit $ Hs.nameToStr inst.fieldName

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1287>
    -- This is not actually a free type variable.
    tyTypeVar = TFree $ Hs.UnsafeName "ty"

{-------------------------------------------------------------------------------
  Variables
-------------------------------------------------------------------------------}

translateDeclVar :: Hs.Var -> SDecl
translateDeclVar var = DBinding Binding{
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
     Hs.Struct
  -> Hs.Type
  -> Map Integer (NonEmpty String)
  -> Bool
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstance struct fTyp vMap isSequential mbComment = Instance {
      clss    = Inst.CEnum
    , args    = [tcon]
    , super   = []
    , types   = [(bindgenGlobalType CEnumZ_type, [tcon], translateType fTyp)]
    , comment = mbComment
    , decs    = map (first bindgenGlobalTerm) [
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
    dconStrE = EString $ Hs.nameToStr struct.constr

    fname :: Hs.Name Hs.NsVar
    fname = case struct.fields of
        (f : _) -> f.name
        []      -> panicPure "translateCEnumInstance: empty fields"

    fnameStr :: String
    fnameStr = Hs.nameToStr fname

    fromCEnumE :: ClosedExpr
    fromCEnumE = eBindgenGlobal HasField_getField `ETypeApp` translateType (Hs.StrLit fnameStr)

    declaredValuesE :: SExpr ctx
    declaredValuesE = EApp (eBindgenGlobal CEnum_declaredValuesFromList) $ EList [
        appManyExpr (EBoxedTup $ Plus2 0) [
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

    seqDecs :: [(Global LvlTerm, ClosedExpr)]
    seqDecs
      | isSequential = [
            (bindgenGlobalTerm CEnum_isDeclared, eBindgenGlobal CEnum_seqIsDeclared)
          , (bindgenGlobalTerm CEnum_mkDeclared, eBindgenGlobal CEnum_seqMkDeclared)
          ]
      | otherwise = []

translateSequentialCEnum ::
     Hs.Struct
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
          (bindgenGlobalTerm SequentialCEnum_minDeclaredValue, ECon nameMin)
        , (bindgenGlobalTerm SequentialCEnum_maxDeclaredValue, ECon nameMax)
        ]
    }
  where
    tcon :: ClosedType
    tcon = TCon struct.name

translateCEnumInstanceShow ::
     Hs.Struct
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstanceShow struct mbComment = Instance {
      clss    = Inst.Show
    , args    = [tcon]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = [(bindgenGlobalTerm Show_showsPrec, eBindgenGlobal CEnum_showsCEnum)]
    }
  where
    tcon :: ClosedType
    tcon = TCon struct.name

translateCEnumInstanceRead ::
     Hs.Struct
  -> Maybe HsDoc.Comment
  -> Instance
translateCEnumInstanceRead struct mbComment = Instance {
      clss    = Inst.Read
    , args    = [tcon]
    , super   = []
    , types   = []
    , comment = mbComment
    , decs    = map (bimap bindgenGlobalTerm eBindgenGlobal) [
          (Read_readPrec     , CEnum_readPrecCEnum)
        , (Read_readList     , Read_readListDefault)
        , (Read_readListPrec , Read_readListPrecDefault)
        ]
    }
  where
    tcon :: ClosedType
    tcon = TCon struct.name
