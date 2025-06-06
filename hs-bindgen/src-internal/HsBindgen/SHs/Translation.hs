-- | Simplified HS translation (from high level HS)
module HsBindgen.SHs.Translation (
    translateDecls,
    translateType,
) where

-- previously Backend.Common.Translation

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy(..))
import Data.Text qualified as T
import Data.Type.Nat qualified as Fin
import Data.Vec.Lazy qualified as Vec

import HsBindgen.C.Tc.Macro qualified as Macro hiding ( IntegralType )
import HsBindgen.Errors
import HsBindgen.Frontend.Macros.AST.Syntax qualified as C
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Type
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import HsBindgen.NameHint
import HsBindgen.SHs.AST

import DeBruijn (rzeroAdd)
import DeBruijn.Internal.Size (Size(UnsafeSize))
import Witherable (ordNub)

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

translateDecls :: [Hs.Decl] -> [SDecl]
translateDecls decls
    | null csources' =                      concatMap translateDecl decls
    | otherwise      = DCSource csources' : concatMap translateDecl decls
  where
    csources' = csources decls

-- 20250429 this function will change,
-- but for now we find the includes to test addCSource functionality
csources :: [Hs.Decl] -> String
csources decls = unlines $ headers ++ bodies
  where
    headers = ordNub
      [ "#include \"" ++ header ++ "\""
      | Hs.DeclInlineCInclude header <- decls
      ]

    bodies =
      [ body
      | Hs.DeclInlineC body <- decls
      ]

translateDecl :: Hs.Decl -> [SDecl]
translateDecl (Hs.DeclData d) = singleton $ translateDeclData d
translateDecl (Hs.DeclEmpty d) = singleton $ translateDeclEmpty d
translateDecl (Hs.DeclNewtype n) = singleton $ translateNewtype n
translateDecl (Hs.DeclDefineInstance i) = singleton $ translateDefineInstanceDecl i
translateDecl (Hs.DeclDeriveInstance s tc c) = singleton $ translateDeriveInstance s tc c
translateDecl (Hs.DeclVar v) = singleton $ translateVarDecl v
translateDecl (Hs.DeclForeignImport i) = translateForeignImportDecl i
translateDecl (Hs.DeclPatSyn ps) = singleton $ translatePatSyn ps
translateDecl (Hs.DeclUnionGetter u f n) = singleton $ translateUnionGetter u f n
translateDecl (Hs.DeclUnionSetter u f n) = singleton $ translateUnionSetter u f n
translateDecl (Hs.DeclSimple d) = [d]
-- these are processed by 'csources'
translateDecl Hs.DeclInlineCInclude {} = []
translateDecl Hs.DeclInlineC {}        = []

translateDefineInstanceDecl :: Hs.InstanceDecl -> SDecl
translateDefineInstanceDecl (Hs.InstanceStorable struct i) =
    DInst $ translateStorableInstance struct i
translateDefineInstanceDecl (Hs.InstanceHasFLAM struct fty i) =
    DInst Instance
      { instanceClass = HasFlexibleArrayMember_class
      , instanceArgs  = [ translateType fty, TCon $ Hs.structName struct ]
      , instanceTypes = []
      , instanceDecs  = [(HasFlexibleArrayMember_offset, ELam "_ty" $ EIntegral (toInteger i) Nothing)]
      }
translateDefineInstanceDecl (Hs.InstanceCEnum struct fTyp vMap isSequential) =
    DInst $ translateCEnumInstance struct fTyp vMap isSequential
translateDefineInstanceDecl (Hs.InstanceSequentialCEnum struct nameMin nameMax) =
    DInst $ translateSequentialCEnum struct nameMin nameMax
translateDefineInstanceDecl (Hs.InstanceCEnumShow struct) =
    DInst $ translateCEnumInstanceShow struct
translateDefineInstanceDecl (Hs.InstanceCEnumRead struct) =
    DInst $ translateCEnumInstanceRead struct

translateDeclData :: Hs.Struct n -> SDecl
translateDeclData struct = DRecord $ Record
    { dataType = Hs.structName struct
    , dataCon  = Hs.structConstr struct
    , dataFields =
        [ Field {
              fieldName   = Hs.fieldName f
            , fieldType   = translateType $ Hs.fieldType f
            , fieldOrigin = Hs.fieldOrigin f
            }
        | f <- toList $ Hs.structFields struct
        ]
    , dataOrigin =
        case Hs.structOrigin struct of
          Just origin -> origin
          Nothing     -> panicPure "Missing structOrigin"
    }

translateDeclEmpty :: Hs.EmptyData -> SDecl
translateDeclEmpty d = DEmptyData $ EmptyData
    { emptyDataName   = Hs.emptyDataName d
    , emptyDataOrigin = Hs.emptyDataOrigin d
    }

translateNewtype :: Hs.Newtype -> SDecl
translateNewtype n = DNewtype $ Newtype
    { newtypeName   = Hs.newtypeName n
    , newtypeCon    = Hs.newtypeConstr n
    , newtypeField  = Field {
          fieldName   = Hs.fieldName $ Hs.newtypeField n
        , fieldType   = translateType . Hs.fieldType $ Hs.newtypeField n
        , fieldOrigin = Hs.fieldOrigin $ Hs.newtypeField n
        }
    , newtypeOrigin = Hs.newtypeOrigin n
    }

translateDeriveInstance :: Hs.Strategy Hs.HsType -> HsTypeClass -> HsName NsTypeConstr -> SDecl
translateDeriveInstance s tc n = DDerivingInstance (fmap translateType s) $ TApp (translateTypeClass tc) (TCon n)

translateTypeClass :: HsTypeClass -> ClosedType
translateTypeClass Hs.Bits       = TGlobal Bits_class
translateTypeClass Hs.Bounded    = TGlobal Bounded_class
translateTypeClass Hs.Enum       = TGlobal Enum_class
translateTypeClass Hs.Eq         = TGlobal Eq_class
translateTypeClass Hs.FiniteBits = TGlobal FiniteBits_class
translateTypeClass Hs.Floating   = TGlobal Floating_class
translateTypeClass Hs.Fractional = TGlobal Fractional_class
translateTypeClass Hs.Integral   = TGlobal Integral_class
translateTypeClass Hs.Ix         = TGlobal Ix_class
translateTypeClass Hs.Num        = TGlobal Num_class
translateTypeClass Hs.Ord        = TGlobal Ord_class
translateTypeClass Hs.Read       = TGlobal Read_class
translateTypeClass Hs.ReadRaw    = TGlobal ReadRaw_class
translateTypeClass Hs.Real       = TGlobal Real_class
translateTypeClass Hs.RealFloat  = TGlobal RealFloat_class
translateTypeClass Hs.RealFrac   = TGlobal RealFrac_class
translateTypeClass Hs.Show       = TGlobal Show_class
translateTypeClass Hs.StaticSize = TGlobal StaticSize_class
translateTypeClass Hs.Storable   = TGlobal Storable_class
translateTypeClass Hs.WriteRaw   = TGlobal WriteRaw_class

translateVarDecl :: Hs.VarDecl -> SDecl
translateVarDecl Hs.VarDecl {..} = DVar
    varDeclName
    (Just (translateSigma varDeclType))
    (translateBody varDeclBody)

translateForeignImportDecl :: Hs.ForeignImportDecl -> [SDecl]
translateForeignImportDecl Hs.ForeignImportDecl {..} =
    [  DForeignImport ForeignImport
        { foreignImportName     = foreignImportName
        , foreignImportType     = translateType foreignImportType
        , foreignImportOrigName = foreignImportOrigName
        , foreignImportHeader   = foreignImportHeader
        , foreignImportOrigin   = foreignImportDeclOrigin
        }
    ]

translatePatSyn :: Hs.PatSyn -> SDecl
translatePatSyn Hs.PatSyn {..} = DPatternSynonym PatternSynonym
    { patSynName   = patSynName
    , patSynType   = TCon patSynType
    , patSynRHS    = PEApps patSynConstr [PELit patSynValue]
    , patSynOrigin = patSynOrigin
    }

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

translateType :: Hs.HsType -> ClosedType
translateType (Hs.HsPrimType t)     = TGlobal (PrimType t)
translateType (Hs.HsTypRef r)       = TCon r
translateType (Hs.HsPtr t)          = TApp (TGlobal Foreign_Ptr) (translateType t)
translateType (Hs.HsFunPtr t)       = TApp (TGlobal Foreign_FunPtr) (translateType t)
translateType (Hs.HsConstArray n t) = TGlobal ConstantArray `TApp` TLit n `TApp` (translateType t)
translateType (Hs.HsIO t)           = TApp (TGlobal IO_type) (translateType t)
translateType (Hs.HsFun a b)        = TFun (translateType a) (translateType b)
translateType (Hs.HsExtBinding i t) = TExt i t
translateType Hs.HsByteArray        = TGlobal ByteArray_type
translateType (Hs.HsSizedByteArray n m) = TGlobal SizedByteArray_type `TApp` TLit n `TApp` TLit m

{-------------------------------------------------------------------------------
  Sigma/Phi/Tau types
-------------------------------------------------------------------------------}

translateSigma :: Hs.SigmaType -> ClosedType
translateSigma (Hs.ForallTy hints (Hs.QuantTy cts ty)) =
  TForall
    hints
    (rzeroAdd $ UnsafeSize $ length hints)
    (map translatePredTy cts)
    (translateTau ty)

translatePredTy :: Hs.PredType ctx -> SType ctx
translatePredTy (Hs.DictTy (Hs.AClass cls) args) =
  foldl' TApp (tyConGlobal cls) (fmap translateTau args)
translatePredTy (Hs.NomEqTy a b) =
  TGlobal NomEq_class `TApp` translateTau a `TApp` translateTau b

translateTau :: Hs.TauType ctx -> SType ctx
translateTau = \case
  Hs.FunTy a b -> TFun (translateTau a) (translateTau b)
  Hs.TyVarTy x -> TBound x
  Hs.TyConAppTy (Hs.ATyCon tc) args
    | Just ty <- simpleTyConApp tc args
    -> ty
    | otherwise
    -> foldl' TApp (tyConGlobal tc) (fmap translateTau args)

simpleTyConApp :: Macro.TyCon args Macro.Ty -> [Hs.TauType ctx] -> Maybe (SType ctx)
simpleTyConApp
  (Macro.GenerativeTyCon (Macro.DataTyCon Macro.IntLikeTyCon))
  [Hs.TyConAppTy (Hs.ATyCon (Macro.GenerativeTyCon (Macro.DataTyCon (Macro.PrimIntInfoTyCon inty)))) []]
    = Just $ TGlobal $ PrimType $
        case inty of
          Macro.HsIntType -> HsPrimInt
          Macro.CIntegralType primIntTy -> hsPrimIntTy primIntTy
simpleTyConApp
  (Macro.GenerativeTyCon (Macro.DataTyCon Macro.FloatLikeTyCon))
  [Hs.TyConAppTy (Hs.ATyCon (Macro.GenerativeTyCon (Macro.DataTyCon (Macro.PrimFloatInfoTyCon floaty)))) []]
    = Just $ TGlobal $ PrimType $ hsPrimFloatTy floaty
simpleTyConApp _ _ = Nothing

tyConGlobal :: Macro.TyCon args res -> SType ctx
tyConGlobal = \case
  Macro.GenerativeTyCon tc ->
    case tc of
      Macro.DataTyCon dc ->
        case dc of
          Macro.TupleTyCon n ->
            TGlobal $ Tuple_type n
          Macro.VoidTyCon ->
            TGlobal $ PrimType HsPrimVoid
          Macro.IntLikeTyCon   ->
            TGlobal IntLike_tycon
          Macro.FloatLikeTyCon ->
            TGlobal FloatLike_tycon
          Macro.PrimIntInfoTyCon inty ->
            TGlobal $ PrimType $
              case inty of
                Macro.CIntegralType primIntTy -> hsPrimIntTy primIntTy
                Macro.HsIntType -> HsPrimInt
          Macro.PrimFloatInfoTyCon floaty ->
            TGlobal $ PrimType $ hsPrimFloatTy floaty
          Macro.PtrTyCon ->
            TGlobal Foreign_Ptr
          Macro.CharLitTyCon ->
            TGlobal CharValue_tycon
          Macro.PrimTyTyCon ->
            panicPure "tyConGlobal PrimTyTyCon"
          Macro.EmptyTyCon ->
            panicPure "tyConGlobal EmptyTyCon"
      Macro.ClassTyCon cls -> TGlobal $
        case cls of
          Macro.NotTyCon        -> Not_class
          Macro.LogicalTyCon    -> Logical_class
          Macro.RelEqTyCon      -> RelEq_class
          Macro.RelOrdTyCon     -> RelOrd_class
          Macro.PlusTyCon       -> Plus_class
          Macro.MinusTyCon      -> Minus_class
          Macro.AddTyCon        -> Add_class
          Macro.SubTyCon        -> Sub_class
          Macro.MultTyCon       -> Mult_class
          Macro.DivTyCon        -> Div_class
          Macro.RemTyCon        -> Rem_class
          Macro.ComplementTyCon -> Complement_class
          Macro.BitwiseTyCon    -> Bitwise_class
          Macro.ShiftTyCon      -> Shift_class
  Macro.FamilyTyCon tc -> TGlobal $
    case tc of
      Macro.PlusResTyCon       -> Plus_resTyCon
      Macro.MinusResTyCon      -> Minus_resTyCon
      Macro.AddResTyCon        -> Add_resTyCon
      Macro.SubResTyCon        -> Sub_resTyCon
      Macro.MultResTyCon       -> Mult_resTyCon
      Macro.DivResTyCon        -> Div_resTyCon
      Macro.RemResTyCon        -> Rem_resTyCon
      Macro.ComplementResTyCon -> Complement_resTyCon
      Macro.BitsResTyCon       -> Bitwise_resTyCon
      Macro.ShiftResTyCon      -> Shift_resTyCon

mfunGlobal :: C.MFun arity -> Global
mfunGlobal = \case
  C.MUnaryPlus  -> Plus_plus
  C.MUnaryMinus -> Minus_negate
  C.MLogicalNot -> Not_not
  C.MBitwiseNot -> Complement_complement
  C.MMult       -> Mult_mult
  C.MDiv        -> Div_div
  C.MRem        -> Rem_rem
  C.MAdd        -> Add_add
  C.MSub        -> Sub_minus
  C.MShiftLeft  -> Shift_shiftL
  C.MShiftRight -> Shift_shiftR
  C.MRelLT      -> RelOrd_lt
  C.MRelLE      -> RelOrd_le
  C.MRelGT      -> RelOrd_gt
  C.MRelGE      -> RelOrd_ge
  C.MRelEQ      -> RelEq_eq
  C.MRelNE      -> RelEq_uneq
  C.MBitwiseAnd -> Bitwise_and
  C.MBitwiseXor -> Bitwise_xor
  C.MBitwiseOr  -> Bitwise_or
  C.MLogicalAnd -> Logical_and
  C.MLogicalOr  -> Logical_or
  C.MTuple @n   -> Tuple_constructor $ 2 + Fin.reflectToNum @n Proxy

{-------------------------------------------------------------------------------
 VarDeclRHS
-------------------------------------------------------------------------------}

translateBody :: Hs.VarDeclRHS ctx -> SExpr ctx
translateBody (Hs.VarDeclVar x)                     = EBound x
translateBody (Hs.VarDeclFloat f)                   = EFloat f HsPrimCFloat
translateBody (Hs.VarDeclDouble d)                  = EDouble d HsPrimCDouble
translateBody (Hs.VarDeclIntegral i ty)             = EIntegral i (Just ty)
translateBody (Hs.VarDeclChar c)                    = EChar c
translateBody (Hs.VarDeclString s)                  = ECString s
translateBody (Hs.VarDeclLambda (Hs.Lambda hint b)) = ELam hint (translateBody b)
translateBody (Hs.VarDeclApp f as)                  = foldl' EApp (translateAppHead f) (map translateBody as)

translateAppHead :: Hs.VarDeclRHSAppHead -> SExpr ctx
translateAppHead = \case
  Hs.InfixAppHead mfun ->
    EGlobal $ mfunGlobal mfun
  Hs.VarAppHead macroNm -> do
    EFree macroNm

{-------------------------------------------------------------------------------
  'Storable'
-------------------------------------------------------------------------------}

translateStorableInstance :: Hs.Struct n -> Hs.StorableInstance -> Instance
translateStorableInstance struct Hs.StorableInstance{..} = do
    let peek = lambda (idiom structCon translatePeekByteOff) storablePeek
    let poke = lambda (lambda (translateElimStruct (doAll translatePokeByteOff))) storablePoke
    Instance
      { instanceClass = Storable_class
      , instanceArgs  = [TCon $ Hs.structName struct]
      , instanceTypes = []
      , instanceDecs  = [
            (Storable_sizeOf    , EUnusedLam $ EInt storableSizeOf)
          , (Storable_alignment , EUnusedLam $ EInt storableAlignment)
          , (Storable_peek      , peek)
          , (Storable_poke      , poke)
          ]
      }

translatePeekByteOff :: Hs.PeekByteOff ctx -> SExpr ctx
translatePeekByteOff (Hs.PeekByteOff ptr i) = appMany Storable_peekByteOff [EBound ptr, EInt i]
translatePeekByteOff (Hs.PeekBitOffWidth ptr i w) = appMany Bitfield_peekBitOffWidth [EBound ptr, EInt i, EInt w] -- TODO

translatePokeByteOff :: Hs.PokeByteOff ctx -> SExpr ctx
translatePokeByteOff (Hs.PokeByteOff ptr i x) = appMany Storable_pokeByteOff [EBound ptr, EInt i, EBound x]
translatePokeByteOff (Hs.PokeBitOffWidth ptr i w x) = appMany Bitfield_pokeBitOffWidth [EBound ptr, EInt i, EInt w, EBound x] -- TODO

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

translateElimStruct :: (forall ctx'. t ctx' -> SExpr ctx') -> Hs.ElimStruct t ctx -> SExpr ctx
translateElimStruct f (Hs.ElimStruct x struct add k) = ECase
    (EBound x)
    [SAlt (Hs.structConstr struct) add hints (f k)]
  where
    hints = fmap (toNameHint . Hs.fieldName) $ Hs.structFields struct

toNameHint :: HsName 'NsVar -> NameHint
toNameHint (HsName t) = NameHint (T.unpack t)

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

translateUnionGetter :: HsName NsTypeConstr -> HsType -> HsName NsVar -> SDecl
translateUnionGetter u f n = DVar n
    (Just $ TFun (TCon u) (translateType f))
    (EGlobal ByteArray_getUnionPayload)

translateUnionSetter :: HsName NsTypeConstr -> HsType -> HsName NsVar -> SDecl
translateUnionSetter u f n = DVar n
    (Just $ TFun (translateType f) (TCon u))
    (EGlobal ByteArray_setUnionPayload)

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

translateCEnumInstance ::
     Hs.Struct (S Z)
  -> HsType
  -> Map Integer (NonEmpty String)
  -> Bool
  -> Instance
translateCEnumInstance struct fTyp vMap isSequential = Instance {
      instanceClass = CEnum_class
    , instanceArgs  = [tcon]
    , instanceTypes = [(CEnumZ_tycon, tcon, translateType fTyp)]
    , instanceDecs  = [
          (CEnum_toCEnum, ECon (Hs.structConstr struct))
        , (CEnum_fromCEnum, EFree fname)
        , (CEnum_declaredValues, EUnusedLam declaredValuesE)
        , (CEnum_showsUndeclared, EApp (EGlobal CEnum_showsWrappedUndeclared) dconStrE)
        , (CEnum_readPrecUndeclared, EApp (EGlobal CEnum_readPrecWrappedUndeclared) dconStrE)
        ] ++ seqDecs
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

    dconStrE :: SExpr ctx
    dconStrE = EString . T.unpack $ getHsName (Hs.structConstr struct)

    fname :: HsName NsVar
    fname = Hs.fieldName $
      NonEmpty.head (Vec.toNonEmpty (Hs.structFields struct))

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
  -> HsName NsConstr
  -> HsName NsConstr
  -> Instance
translateSequentialCEnum struct nameMin nameMax = Instance {
      instanceClass = SequentialCEnum_class
    , instanceArgs  = [tcon]
    , instanceTypes = []
    , instanceDecs  = [
          (SequentialCEnum_minDeclaredValue, ECon nameMin)
        , (SequentialCEnum_maxDeclaredValue, ECon nameMax)
        ]
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

translateCEnumInstanceShow ::
     Hs.Struct (S Z)
  -> Instance
translateCEnumInstanceShow struct = Instance {
      instanceClass = Show_class
    , instanceArgs  = [tcon]
    , instanceTypes = []
    , instanceDecs  = [
          (Show_showsPrec, EGlobal CEnum_showsCEnum)
        ]
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

translateCEnumInstanceRead ::
     Hs.Struct (S Z)
  -> Instance
translateCEnumInstanceRead struct = Instance {
      instanceClass = Read_class
    , instanceArgs  = [tcon]
    , instanceTypes = []
    , instanceDecs  = [
          (Read_readPrec, EGlobal CEnum_readPrecCEnum)
        , (Read_readList, EGlobal Read_readListDefault)
        , (Read_readListPrec, EGlobal Read_readListPrecDefault)
        ]
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

{-------------------------------------------------------------------------------
  Internal auxiliary: derived functionality
-------------------------------------------------------------------------------}

-- | Apply function to many arguments
appMany :: Global -> [SExpr ctx] -> SExpr ctx
appMany = foldl' EApp . EGlobal

-- | Struct constructor
structCon :: Hs.StructCon ctx -> SExpr ctx
structCon (Hs.StructCon s) = ECon (Hs.structConstr s)

-- | Idiom brackets
idiom :: (pure ctx -> SExpr ctx) -> (xs ctx -> SExpr ctx) -> Hs.Ap pure xs ctx -> SExpr ctx
idiom f g (Hs.Ap p xs) = foldl'
    (\ acc x -> EInfix Applicative_seq acc (g x))
    (EApp (EGlobal Applicative_pure) (f p))
    xs

-- | Translate lambda
lambda :: (t (S ctx) -> SExpr (S ctx)) -> Hs.Lambda t ctx -> SExpr ctx
lambda f (Hs.Lambda hint t) = ELam hint (f t)

-- | Monad sequencing
doAll :: (t ctx -> SExpr ctx) -> Hs.Seq t ctx -> SExpr ctx
doAll _ (Hs.Seq []) = EGlobal Monad_return `EApp` EGlobal (Tuple_constructor 0)
doAll f (Hs.Seq ss) = foldr1 (EInfix Monad_seq) (map f ss)
