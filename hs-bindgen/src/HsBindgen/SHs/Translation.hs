-- | Simplified HS translation (from high level HS)
module HsBindgen.SHs.Translation (
    translateDecl,
) where

-- previously Backend.Common.Translation

import Data.Text qualified as T

import HsBindgen.C.AST qualified as C (MFun(..))
import HsBindgen.C.Tc.Macro qualified as C
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Hs.Translation (integralType, floatingType)
import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.SHs.AST

import DeBruijn (rzeroAdd)
import DeBruijn.Internal.Size (Size(UnsafeSize))

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

translateDecl :: Hs.Decl -> SDecl
translateDecl (Hs.DeclData d) = translateDeclData d
translateDecl (Hs.DeclEmpty n) = translateDeclEmpty n
translateDecl (Hs.DeclNewtype n) = translateNewtype n
translateDecl (Hs.DeclInstance i) = translateInstanceDecl i
translateDecl (Hs.DeclNewtypeInstance tc c) = translateNewtypeInstance tc c
translateDecl (Hs.DeclVar v) = translateVarDecl v
translateDecl (Hs.DeclForeignImport i) = translateForeignImportDecl i
translateDecl (Hs.DeclPatSyn ps) = translatePatSyn ps

translateInstanceDecl :: Hs.InstanceDecl -> SDecl
translateInstanceDecl (Hs.InstanceStorable struct i) =
    DInst $ translateStorableInstance struct i

translateDeclData :: Hs.Struct n -> SDecl
translateDeclData struct = DRecord $ Record
    { dataType = Hs.structName struct
    , dataCon  = Hs.structConstr struct
    , dataFields =
        [ (n, translateType t)
        | (n, t) <- toList $ Hs.structFields struct
        ]
    }

translateDeclEmpty :: HsName NsTypeConstr -> SDecl
translateDeclEmpty n = DEmptyData n

translateNewtype :: Hs.Newtype -> SDecl
translateNewtype n = DNewtype $ Newtype
    { newtypeName  = Hs.newtypeName n
    , newtypeCon   = Hs.newtypeConstr n
    , newtypeField = Hs.newtypeField n
    , newtypeType  = translateType (Hs.newtypeType n)
    }

translateNewtypeInstance :: Hs.TypeClass -> HsName NsTypeConstr -> SDecl
translateNewtypeInstance tc n = DDerivingNewtypeInstance $ TApp (translateTypeClass tc) (TCon n)

translateTypeClass :: Hs.TypeClass -> ClosedType
translateTypeClass Hs.Storable = TGlobal Storable_Storable

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
    }

translatePatSyn :: Hs.PatSyn -> SDecl
translatePatSyn Hs.PatSyn {..} = DPatternSynonym PatternSynonym
    { patSynName = patSynName
    , patSynType = TCon patSynType
    , patSynRHS  = EApp (ECon patSynConstr) (EIntegral patSynValue Nothing)
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
  [Hs.TyConAppTy (Hs.ATyCon (C.GenerativeTyCon (C.DataTyCon (C.PrimIntTyCon inty)))) []]
    = Just $ TGlobal $ PrimType $ uncurry integralType inty
simpleTyConApp
  (C.GenerativeTyCon (C.DataTyCon C.FloatLikeTyCon))
  [Hs.TyConAppTy (Hs.ATyCon (C.GenerativeTyCon (C.DataTyCon (C.PrimFloatTyCon floaty)))) []]
    = Just $ TGlobal $ PrimType $ floatingType floaty
simpleTyConApp _ _ = Nothing

tyConGlobal :: C.TyCon args res -> SType ctx
tyConGlobal = \case
  C.GenerativeTyCon tc ->
    case tc of
      C.DataTyCon dc ->
        case dc of
          C.IntLikeTyCon   ->
            TGlobal IntLike_tycon
          C.FloatLikeTyCon ->
            TGlobal FloatLike_tycon
          C.PrimIntTyCon inty ->
            TGlobal $ PrimType $ uncurry integralType inty
          C.PrimFloatTyCon floaty ->
            TGlobal $ PrimType $ floatingType floaty
          C.BoolTyCon ->
            TGlobal $ PrimType $ HsPrimCBool
          C.StringTyCon ->
            TApp (TGlobal Foreign_Ptr) (TGlobal $ PrimType $ HsPrimCChar)
          C.PrimTyTyCon ->
            error "tyConGlobal PrimTyTyCon"
          C.EmptyTyCon ->
            error "tyConGlobal EmptyTyCon"
      C.ClassTyCon cls -> TGlobal $
        case cls of
          C.CNotTyCon        -> CNot_class
          C.CLogicalTyCon    -> CLogical_class
          C.CEqTyCon         -> CEq_class
          C.COrdTyCon        -> COrd_class
          C.CPlusTyCon       -> CPlus_class
          C.CMinusTyCon      -> CMinus_class
          C.CAddTyCon        -> CAdd_class
          C.CSubTyCon        -> CSub_class
          C.CMultTyCon       -> CMult_class
          C.CDivTyCon        -> CDiv_class
          C.CRemTyCon        -> CRem_class
          C.CComplementTyCon -> CComplement_class
          C.CBitsTyCon       -> CBits_class
          C.CShiftTyCon      -> CShift_class
  C.FamilyTyCon tc -> TGlobal $
    case tc of
      C.NotResTyCon        -> CNot_resTyCon
      C.LogicalResTyCon    -> CLogical_resTyCon
      C.PlusResTyCon       -> CPlus_resTyCon
      C.MinusResTyCon      -> CMinus_resTyCon
      C.AddResTyCon        -> CAdd_resTyCon
      C.SubResTyCon        -> CSub_resTyCon
      C.MultResTyCon       -> CMult_resTyCon
      C.DivResTyCon        -> CDiv_resTyCon
      C.RemResTyCon        -> CRem_resTyCon
      C.ComplementResTyCon -> CComplement_resTyCon
      C.BitsResTyCon       -> CBits_resTyCon
      C.ShiftResTyCon      -> CShift_resTyCon

mfunGlobal :: C.MFun arity -> Global
mfunGlobal = \case
  C.MUnaryPlus  -> CPlus_plus
  C.MUnaryMinus -> CMinus_negate
  C.MLogicalNot -> CNot_not
  C.MBitwiseNot -> CComplement_complement
  C.MMult       -> CMult_mult
  C.MDiv        -> CDiv_div
  C.MRem        -> CRem_rem
  C.MAdd        -> CAdd_add
  C.MSub        -> CSub_minus
  C.MShiftLeft  -> CShift_shiftL
  C.MShiftRight -> CShift_shiftR
  C.MRelLT      -> COrd_lt
  C.MRelLE      -> COrd_le
  C.MRelGT      -> COrd_gt
  C.MRelGE      -> COrd_ge
  C.MRelEQ      -> CEq_eq
  C.MRelNE      -> CEq_uneq
  C.MBitwiseAnd -> CBits_and
  C.MBitwiseXor -> CBits_xor
  C.MBitwiseOr  -> CBits_or
  C.MLogicalAnd -> CLogical_and
  C.MLogicalOr  -> CLogical_or

{-------------------------------------------------------------------------------
 VarDeclRHS
-------------------------------------------------------------------------------}

translateBody :: Hs.VarDeclRHS ctx -> SExpr ctx
translateBody (Hs.VarDeclVar x)                     = EBound x
translateBody (Hs.VarDeclFloat f)                   = EFloat f
translateBody (Hs.VarDeclDouble d)                  = EDouble d
translateBody (Hs.VarDeclIntegral i ty)             = EIntegral i (Just ty)
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
      { instanceClass = Storable_Storable
      , instanceType  = Hs.structName struct
      , instanceDecs  = [
            (Storable_sizeOf    , EUnusedLam $ EInt storableSizeOf)
          , (Storable_alignment , EUnusedLam $ EInt storableAlignment)
          , (Storable_peek      , peek)
          , (Storable_poke      , poke)
          ]
      }

translatePeekByteOff :: Hs.PeekByteOff ctx -> SExpr ctx
translatePeekByteOff (Hs.PeekByteOff ptr i) = appMany Storable_peekByteOff [EBound ptr, EInt i]

translatePokeByteOff :: Hs.PokeByteOff ctx -> SExpr ctx
translatePokeByteOff (Hs.PokeByteOff ptr i x) = appMany Storable_pokeByteOff [EBound ptr, EInt i, EBound x]

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

translateElimStruct :: (forall ctx'. t ctx' -> SExpr ctx') -> Hs.ElimStruct t ctx -> SExpr ctx
translateElimStruct f (Hs.ElimStruct x struct add k) = ECase
    (EBound x)
    [SAlt (Hs.structConstr struct) add hints (f k)]
  where
    hints = fmap (toNameHint . fst) $ Hs.structFields struct

toNameHint :: HsName 'NsVar -> NameHint
toNameHint (HsName t) = NameHint (T.unpack t)

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
doAll _ (Hs.Seq []) = EGlobal Monad_return `EApp` EGlobal Unit_constructor
doAll f (Hs.Seq ss) = foldr1 (EInfix Monad_seq) (map f ss)
