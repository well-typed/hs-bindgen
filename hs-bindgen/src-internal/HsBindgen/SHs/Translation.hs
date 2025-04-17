-- | Simplified HS translation (from high level HS)
module HsBindgen.SHs.Translation (
    translateDecl,
) where

-- previously Backend.Common.Translation

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vec.Lazy qualified as Vec

import HsBindgen.C.AST qualified as C (MFun(..))
import HsBindgen.C.Tc.Macro qualified as C hiding ( IntegralType )
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.SHs.AST
import HsBindgen.Errors

import C.Type qualified as C

import DeBruijn (rzeroAdd)
import DeBruijn.Internal.Size (Size(UnsafeSize))
import Data.Type.Nat qualified as Fin
import Data.Proxy (Proxy(..))

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

translateDecl :: Hs.Decl -> SDecl
translateDecl (Hs.DeclData d) = translateDeclData d
translateDecl (Hs.DeclEmpty d) = translateDeclEmpty d
translateDecl (Hs.DeclNewtype n) = translateNewtype n
translateDecl (Hs.DeclDefineInstance i) = translateDefineInstanceDecl i
translateDecl (Hs.DeclDeriveInstance s tc c) = translateDeriveInstance s tc c
translateDecl (Hs.DeclVar v) = translateVarDecl v
translateDecl (Hs.DeclForeignImport i) = translateForeignImportDecl i
translateDecl (Hs.DeclPatSyn ps) = translatePatSyn ps
translateDecl (Hs.DeclUnionGetter u f n) = translateUnionGetter u f n
translateDecl (Hs.DeclUnionSetter u f n) = translateUnionSetter u f n

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
translateDefineInstanceDecl (Hs.InstanceCEnum struct fTyp vMap) =
    DInst $ translateCEnumInstance struct fTyp vMap
translateDefineInstanceDecl (Hs.InstanceSequentialCEnum struct nameMin nameMax) =
    DInst $ translateSequentialCEnum struct nameMin nameMax
translateDefineInstanceDecl (Hs.InstanceCEnumShow struct) =
    DInst $ translateCEnumInstanceShow struct

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
    , dataOrigin = Hs.structOrigin struct
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

translateDeriveInstance :: Hs.Strategy Hs.HsType -> Hs.TypeClass -> HsName NsTypeConstr -> SDecl
translateDeriveInstance s tc n = DDerivingInstance (fmap translateType s) $ TApp (translateTypeClass tc) (TCon n)

translateTypeClass :: Hs.TypeClass -> ClosedType
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
translateTypeClass Hs.Real       = TGlobal Real_class
translateTypeClass Hs.RealFloat  = TGlobal RealFloat_class
translateTypeClass Hs.RealFrac   = TGlobal RealFrac_class
translateTypeClass Hs.Show       = TGlobal Show_class
translateTypeClass Hs.Storable   = TGlobal Storable_class

translateVarDecl :: Hs.VarDecl -> SDecl
translateVarDecl Hs.VarDecl {..} = DVar
    varDeclName
    (Just (translateSigma varDeclType))
    (translateBody varDeclBody)

translateForeignImportDecl :: Hs.ForeignImportDecl -> SDecl
translateForeignImportDecl Hs.ForeignImportDecl {..} = DForeignImport ForeignImport
    { foreignImportName     = foreignImportName
    , foreignImportType     = translateType foreignImportType
    , foreignImportOrigName = foreignImportOrigName
    , foreignImportHeader   = foreignImportHeader
    , foreignImportOrigin   = foreignImportDeclOrigin
    }

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
translateType (Hs.HsType _)         = TGlobal (PrimType HsPrimVoid)
translateType (Hs.HsIO t)           = TApp (TGlobal IO_type) (translateType t)
translateType (Hs.HsFun a b)        = TFun (translateType a) (translateType b)
translateType (Hs.HsExtBinding i)   = TExt i
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

simpleTyConApp :: C.TyCon args C.Ty -> [Hs.TauType ctx] -> Maybe (SType ctx)
simpleTyConApp
  (C.GenerativeTyCon (C.DataTyCon C.IntLikeTyCon))
  [Hs.TyConAppTy (Hs.ATyCon (C.GenerativeTyCon (C.DataTyCon (C.PrimIntInfoTyCon inty)))) []]
    = Just $ TGlobal $ PrimType $
        case inty of
          C.HsIntType -> HsPrimInt
          C.CIntegralType primIntTy -> hsPrimIntTy primIntTy
simpleTyConApp
  (C.GenerativeTyCon (C.DataTyCon C.FloatLikeTyCon))
  [Hs.TyConAppTy (Hs.ATyCon (C.GenerativeTyCon (C.DataTyCon (C.PrimFloatInfoTyCon floaty)))) []]
    = Just $ TGlobal $ PrimType $ hsPrimFloatTy floaty
simpleTyConApp _ _ = Nothing

tyConGlobal :: C.TyCon args res -> SType ctx
tyConGlobal = \case
  C.GenerativeTyCon tc ->
    case tc of
      C.DataTyCon dc ->
        case dc of
          C.TupleTyCon n ->
            TGlobal $ Tuple_type n
          C.VoidTyCon ->
            TGlobal $ PrimType HsPrimVoid
          C.IntLikeTyCon   ->
            TGlobal IntLike_tycon
          C.FloatLikeTyCon ->
            TGlobal FloatLike_tycon
          C.PrimIntInfoTyCon inty ->
            TGlobal $ PrimType $
              case inty of
                C.CIntegralType primIntTy -> hsPrimIntTy primIntTy
                C.HsIntType -> HsPrimInt
          C.PrimFloatInfoTyCon floaty ->
            TGlobal $ PrimType $ hsPrimFloatTy floaty
          C.PtrTyCon ->
            TGlobal Foreign_Ptr
          C.CharLitTyCon ->
            TGlobal CharValue_tycon
          C.PrimTyTyCon ->
            panicPure "tyConGlobal PrimTyTyCon"
          C.EmptyTyCon ->
            panicPure "tyConGlobal EmptyTyCon"
      C.ClassTyCon cls -> TGlobal $
        case cls of
          C.NotTyCon        -> Not_class
          C.LogicalTyCon    -> Logical_class
          C.RelEqTyCon      -> RelEq_class
          C.RelOrdTyCon     -> RelOrd_class
          C.PlusTyCon       -> Plus_class
          C.MinusTyCon      -> Minus_class
          C.AddTyCon        -> Add_class
          C.SubTyCon        -> Sub_class
          C.MultTyCon       -> Mult_class
          C.DivTyCon        -> Div_class
          C.RemTyCon        -> Rem_class
          C.ComplementTyCon -> Complement_class
          C.BitwiseTyCon    -> Bitwise_class
          C.ShiftTyCon      -> Shift_class
  C.FamilyTyCon tc -> TGlobal $
    case tc of
      C.PlusResTyCon       -> Plus_resTyCon
      C.MinusResTyCon      -> Minus_resTyCon
      C.AddResTyCon        -> Add_resTyCon
      C.SubResTyCon        -> Sub_resTyCon
      C.MultResTyCon       -> Mult_resTyCon
      C.DivResTyCon        -> Div_resTyCon
      C.RemResTyCon        -> Rem_resTyCon
      C.ComplementResTyCon -> Complement_resTyCon
      C.BitsResTyCon       -> Bitwise_resTyCon
      C.ShiftResTyCon      -> Shift_resTyCon

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

hsPrimIntTy :: C.IntegralType -> HsPrimType
hsPrimIntTy = \case
  C.Bool -> HsPrimCBool
  C.CharLike c ->
    case c of
      C.Char  -> HsPrimCChar
      C.SChar -> HsPrimCSChar
      C.UChar -> HsPrimCUChar
  C.IntLike i ->
    case i of
      C.Short    s ->
        case s of
          C.Signed   -> HsPrimCShort
          C.Unsigned -> HsPrimCUShort
      C.Int      s ->
        case s of
          C.Signed   -> HsPrimCInt
          C.Unsigned -> HsPrimCUInt
      C.Long     s ->
        case s of
          C.Signed   -> HsPrimCLong
          C.Unsigned -> HsPrimCULong
      C.LongLong s ->
        case s of
          C.Signed   -> HsPrimCLLong
          C.Unsigned -> HsPrimCULLong
      C.PtrDiff    -> HsPrimCPtrDiff
hsPrimFloatTy :: C.FloatingType -> HsPrimType
hsPrimFloatTy = \case
  C.FloatType  -> HsPrimCFloat
  C.DoubleType -> HsPrimCDouble

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
  -> Instance
translateCEnumInstance struct fTyp vMap = Instance {
      instanceClass = CEnum_class
    , instanceArgs  = [tcon]
    , instanceTypes = [(CEnumZ_tycon, tcon, translateType fTyp)]
    , instanceDecs  = [
          (CEnum_fromCEnumZ, ECon (Hs.structConstr struct))
        , (CEnum_toCEnumZ, EFree fname)
        , (CEnum_declaredValues, EUnusedLam vMapE)
        ]
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

    fname :: HsName NsVar
    fname = Hs.fieldName $
      NonEmpty.head (Vec.toNonEmpty (Hs.structFields struct))

    vMapE :: SExpr ctx
    vMapE = EApp (EGlobal Map_fromList) $ EList [
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
          (Show_show, EApp (EGlobal CEnum_showCEnum) dconStrE)
        ]
    }
  where
    tcon :: ClosedType
    tcon = TCon $ Hs.structName struct

    dconStrE :: SExpr ctx
    dconStrE = EString . T.unpack $ getHsName (Hs.structConstr struct)

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
