module HsBindgen.Backend.SHs.Macro (
    translateMacroExpr
  ) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Type.Nat (SNatI, induction)
import Data.Type.Nat qualified as Fin
import Data.Vec.Lazy qualified as Vec
import DeBruijn.Internal.Size (Size (UnsafeSize))
import GHC.Exts qualified as IsList (IsList (..))

import C.Char qualified as CExpr.Runtime
import C.Type qualified as CExpr.Runtime

import C.Expr.Syntax qualified as CExpr.DSL
import C.Expr.Typecheck.Type qualified as CExpr.DSL
import C.Expr.Util.TestEquality qualified as CExpr.DSL

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.SHs.AST
import HsBindgen.Config.FixCandidate (FixCandidate)
import HsBindgen.Config.FixCandidate qualified as FixCandidate
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

import DeBruijn (Ctx, EmptyCtx, Idx (..), rzeroAdd)

{-------------------------------------------------------------------------------
  External API

  TODO: This module should be cleaned up. In particular, both compositions

  * @translateSigma . quantTyHsTy@
  * @translateBody . macroLamHsExpr@

  should be fused. I think once that is done, 'SigmaType' and co can go.
-------------------------------------------------------------------------------}

translateMacroExpr :: Hs.MacroExpr -> SDecl
translateMacroExpr expr = DVar Var{
      varName    = name
    , varType    = translateSigma . quantTyHsTy $ macroExprType
    , varExpr    = translateBody . macroLamHsExpr $ (macroExprArgs, macroExprBody)
    , varComment = comment
    }
  where
    Hs.MacroExpr{
        macroExprName    = name
      , macroExprBody    = body
      , macroExprComment = comment
      } = expr

    macroExprArgs :: [CExpr.DSL.Name]
    macroExprBody :: CExpr.DSL.MExpr CExpr.DSL.Ps
    macroExprType :: CExpr.DSL.Quant (CExpr.DSL.Type CExpr.DSL.Ty)
    C.CheckedMacroExpr{macroExprArgs, macroExprBody, macroExprType} = body

{-------------------------------------------------------------------------------
  Internal auxiliary: SigmaType
-------------------------------------------------------------------------------}

-- | A σ-type, of the form @forall tvs. ctxt => body@.
type SigmaType :: Star
data SigmaType where
  ForallTy ::
    { forallTyBinders :: Vec n NameHint
    , forallTy        :: PhiType n
    }
    -> SigmaType

deriving stock instance Show SigmaType

-- | A φ-type, of the form @ctxt => body@.
type PhiType :: Ctx -> Star
data PhiType ctx
  = QuantTy
  { quantTyCts  :: [PredType ctx]
  , quantTyBody :: TauType ctx
  }
  deriving stock (Generic, Show)

-- | A τ-type: no quantification or contexts (i.e. no @forall@, no @=>@ arrows).
type TauType :: Ctx -> Star
data TauType ctx
  = FunTy (TauType ctx) (TauType ctx)
  | TyVarTy (Idx ctx)
  | TyConAppTy ATyCon [TauType ctx]
  deriving stock (Eq, Generic, Show)

-- | A predicate/constraint τ-type.
type PredType :: Ctx -> Star
data PredType ctx
  = DictTy AClass [TauType ctx]
  | NomEqTy (TauType ctx) (TauType ctx)
  deriving stock Show

instance Eq (PredType ctx) where
  DictTy cls1 tys1 == DictTy cls2 tys2 =
    cls1 == cls2 && tys1 == tys2
  NomEqTy l1 r1 == NomEqTy l2 r2 =
    l1 == l2 && r1 == r2
  _ == _ = False

data ATyCon where
  ATyCon :: CExpr.DSL.TyCon args CExpr.DSL.Ty -> ATyCon
instance Show ATyCon where
  show ( ATyCon tc ) = show tc
instance Eq ATyCon where
  ATyCon tc1 == ATyCon tc2 =
    isJust $ CExpr.DSL.equals2 tc1 tc2

data AClass where
  AClass :: CExpr.DSL.TyCon args CExpr.DSL.Ct -> AClass
instance Show AClass where
  show ( AClass tc ) = show tc
instance Eq AClass where
  AClass ( CExpr.DSL.GenerativeTyCon ( CExpr.DSL.ClassTyCon cls1 ) )
    ==
    AClass ( CExpr.DSL.GenerativeTyCon ( CExpr.DSL.ClassTyCon cls2 ) ) =
      isJust $ CExpr.DSL.equals1 cls1 cls2

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

quantTyHsTy :: CExpr.DSL.Quant ( CExpr.DSL.Type CExpr.DSL.Ty ) -> SigmaType
quantTyHsTy qty@(CExpr.DSL.Quant @kis _) =
  case CExpr.DSL.mkQuantTyBody qty of
    CExpr.DSL.QuantTyBody { quantTyQuant = cts, quantTyBody = ty } -> do
      goForallTy (CExpr.DSL.tyVarNames @kis) cts ty
  where

    goCt :: Map Text (Idx ctx) -> CExpr.DSL.Type CExpr.DSL.Ct -> PredType ctx
    goCt env (CExpr.DSL.TyConAppTy cls as) =
      DictTy (AClass cls) (goTys env as)
    goCt env (CExpr.DSL.NomEqPred a b) =
      NomEqTy (goTy env a) (goTy env b)

    goTy :: Map Text (Idx ctx) -> CExpr.DSL.Type CExpr.DSL.Ty -> TauType ctx
    goTy env (CExpr.DSL.TyVarTy tv) =
      case Map.lookup (CExpr.DSL.tyVarName tv) env of
        Just hsTv ->
          TyVarTy hsTv
        Nothing ->
          panicPure $ unlines
            [ "quantTyHsTy: unbound type variable " ++ show tv
            , "env: " ++ show env
            , "macro: " ++ show (CExpr.DSL.mkQuantTyBody qty)
            ]
    goTy env (CExpr.DSL.FunTy as r) =
      foldr (FunTy . goTy env) (goTy env r) as
    goTy env (CExpr.DSL.TyConAppTy tc as) =
      TyConAppTy (ATyCon tc) (goTys env as)

    goTys :: Map Text (Idx ctx) -> Vec n ( CExpr.DSL.Type CExpr.DSL.Ty ) -> [ TauType ctx ]
    goTys env as = toList $ fmap (goTy env) as

    goForallTy :: forall n. SNatI n => Vec n (Int, Text) -> [ CExpr.DSL.Type CExpr.DSL.Ct ] -> CExpr.DSL.Type CExpr.DSL.Ty -> SigmaType
    goForallTy args cts body =
        let
          env :: Map Text (Idx n)
          env = Map.fromList $ toList $ Vec.zipWith (,) ( fmap snd args ) qtvs
          qtvs :: Vec n (Idx n)
          qtvs = unU (induction (U VNil) (\(U v) -> U (IZ ::: fmap IS v)))
        in
          ForallTy
            { forallTyBinders = fmap (fromString . T.unpack . snd) args
            , forallTy        = QuantTy
                { quantTyCts  = fmap (goCt env) cts
                , quantTyBody = goTy env body
                }
            }

newtype U n = U { unU :: Vec n (Idx n) }

macroLamHsExpr ::
     ([CExpr.DSL.Name], CExpr.DSL.MExpr p)
  -> Hs.VarDeclRHS EmptyCtx
macroLamHsExpr (macroArgs, expr) =
    makeNames macroArgs Map.empty
  where
    makeNames :: [CExpr.DSL.Name] -> Map CExpr.DSL.Name (Idx ctx) -> Hs.VarDeclRHS ctx
    makeNames []     env = macroExprHsExpr env expr
    makeNames (n:ns) env = Hs.VarDeclLambda . Hs.Lambda (cnameToHint n) $ makeNames ns (Map.insert n IZ (fmap IS env))

cnameToHint :: CExpr.DSL.Name -> NameHint
cnameToHint (CExpr.DSL.Name t) = fromString (T.unpack t)

macroExprHsExpr ::
     Map CExpr.DSL.Name (Idx ctx)
  -> CExpr.DSL.MExpr p
  -> Hs.VarDeclRHS ctx
macroExprHsExpr = goExpr where
    goExpr :: Map CExpr.DSL.Name (Idx ctx) -> CExpr.DSL.MExpr p -> Hs.VarDeclRHS ctx
    goExpr env = \case
      CExpr.DSL.MTerm tm -> goTerm env tm
      CExpr.DSL.MApp _xapp fun args ->
        goApp env (Hs.InfixAppHead fun) (toList args)

    goTerm :: Map CExpr.DSL.Name (Idx ctx) -> CExpr.DSL.MTerm p -> Hs.VarDeclRHS ctx
    goTerm env = \case
      CExpr.DSL.MInt i -> goInt i
      CExpr.DSL.MFloat f -> goFloat f
      CExpr.DSL.MChar c -> goChar c
      CExpr.DSL.MString s -> goString s
      CExpr.DSL.MVar _xvar cname args ->
        --  TODO: removed the macro argument used as a function check.
        case Map.lookup cname env of
          Just i  -> Hs.VarDeclVar i
          Nothing ->
            let hsVar = macroName cname -- mangle nm $ NameVar cname
            in  goApp env (Hs.VarAppHead hsVar) args

    goApp :: Map CExpr.DSL.Name (Idx ctx) -> Hs.VarDeclRHSAppHead -> [CExpr.DSL.MExpr p] -> Hs.VarDeclRHS ctx
    goApp env appHead args =
      let args' = map (goExpr env) args
       in Hs.VarDeclApp appHead args'

    goInt :: CExpr.DSL.IntegerLiteral -> Hs.VarDeclRHS ctx
    goInt (CExpr.DSL.IntegerLiteral { integerLiteralType = intyTy, integerLiteralValue = i }) =
      Hs.VarDeclIntegral i $
        hsPrimIntTy $ CExpr.Runtime.IntLike intyTy

    goChar :: CExpr.DSL.CharLiteral -> Hs.VarDeclRHS ctx
    goChar (CExpr.DSL.CharLiteral { charLiteralValue = c }) =
      Hs.VarDeclChar c

    goString :: CExpr.DSL.StringLiteral -> Hs.VarDeclRHS ctx
    goString (CExpr.DSL.StringLiteral { stringLiteralValue = s }) =
      let bytes = concatMap (IsList.toList . CExpr.Runtime.charValue) s
       in Hs.VarDeclString (IsList.fromList bytes)

    goFloat :: CExpr.DSL.FloatingLiteral -> Hs.VarDeclRHS ctx
    goFloat flt@(CExpr.DSL.FloatingLiteral { floatingLiteralType = fty }) =
      case fty of
        CExpr.Runtime.FloatType  -> Hs.VarDeclFloat (CExpr.DSL.floatingLiteralFloatValue flt)
        CExpr.Runtime.DoubleType -> Hs.VarDeclDouble (CExpr.DSL.floatingLiteralDoubleValue flt)

-- | Construct Haskell name for macro
--
-- TODO: This should be done as part of the NameMangler frontend pass.
macroName :: CExpr.DSL.Name -> Hs.Name Hs.NsVar
macroName (CExpr.DSL.Name cName) =
    case FixCandidate.fixCandidate fix cName of
      Just hsName -> hsName
      Nothing     ->
        panicPure $ "Unable to construct name for macro " ++ show cName
  where
    fix :: FixCandidate Maybe
    fix = FixCandidate.fixCandidateDefault

translateSigma :: SigmaType -> ClosedType
translateSigma (ForallTy hints (QuantTy cts ty)) =
  TForall
    hints
    (rzeroAdd $ UnsafeSize $ length hints)
    (map translatePredTy cts)
    (translateTau ty)

translatePredTy :: PredType ctx -> SType ctx
translatePredTy (DictTy (AClass cls) args) =
  foldl' TApp (tyConGlobal cls) (fmap translateTau args)
translatePredTy (NomEqTy a b) =
  TGlobal NomEq_class `TApp` translateTau a `TApp` translateTau b

translateTau :: TauType ctx -> SType ctx
translateTau = \case
  FunTy a b -> TFun (translateTau a) (translateTau b)
  TyVarTy x -> TBound x
  TyConAppTy (ATyCon tc) args
    | Just ty <- simpleTyConApp tc args
    -> ty
    | otherwise
    -> foldl' TApp (tyConGlobal tc) (fmap translateTau args)

-- | Convert @IntLike inty@ and @FloatLike floaty@ to Haskell types.
--
-- The macro typechecker will never produce a type such as @IntLike a@ for
-- a skolem type variable @a@, because this type has no Haskell counterpart.
--
-- See 'HsBindgen.C.Tc.Macro.isAtomicType'.
simpleTyConApp :: CExpr.DSL.TyCon args CExpr.DSL.Ty -> [TauType ctx] -> Maybe (SType ctx)
simpleTyConApp
  (CExpr.DSL.GenerativeTyCon (CExpr.DSL.DataTyCon CExpr.DSL.IntLikeTyCon))
  [TyConAppTy (ATyCon (CExpr.DSL.GenerativeTyCon (CExpr.DSL.DataTyCon (CExpr.DSL.PrimIntInfoTyCon inty)))) []]
    = Just $ TGlobal $ PrimType $
        case inty of
          CExpr.DSL.HsIntType -> HsPrimInt
          CExpr.DSL.CIntegralType primIntTy -> hsPrimIntTy primIntTy
simpleTyConApp
  (CExpr.DSL.GenerativeTyCon (CExpr.DSL.DataTyCon CExpr.DSL.FloatLikeTyCon))
  [TyConAppTy (ATyCon (CExpr.DSL.GenerativeTyCon (CExpr.DSL.DataTyCon (CExpr.DSL.PrimFloatInfoTyCon floaty)))) []]
    = Just $ TGlobal $ PrimType $ hsPrimFloatTy floaty
simpleTyConApp _ _ = Nothing

tyConGlobal :: CExpr.DSL.TyCon args res -> SType ctx
tyConGlobal = \case
  CExpr.DSL.GenerativeTyCon tc ->
    case tc of
      CExpr.DSL.DataTyCon dc ->
        case dc of
          CExpr.DSL.TupleTyCon n ->
            TGlobal $ Tuple_type n
          CExpr.DSL.VoidTyCon ->
            TGlobal $ PrimType HsPrimVoid
          CExpr.DSL.PrimIntInfoTyCon inty ->
            TGlobal $ PrimType $
              case inty of
                CExpr.DSL.CIntegralType primIntTy -> hsPrimIntTy primIntTy
                CExpr.DSL.HsIntType -> HsPrimInt
          CExpr.DSL.PrimFloatInfoTyCon floaty ->
            TGlobal $ PrimType $ hsPrimFloatTy floaty
          CExpr.DSL.PtrTyCon ->
            TGlobal Foreign_Ptr
          CExpr.DSL.CharLitTyCon ->
            TGlobal CharValue_tycon

          -- These two TyCons are handled by 'simpleTyConApp'.
          CExpr.DSL.IntLikeTyCon   ->
            panicPure "tyConGlobal IntLikeTyCon"
          CExpr.DSL.FloatLikeTyCon ->
            panicPure "tyConGlobal FloatLikeTyCon"

      CExpr.DSL.ClassTyCon cls -> TGlobal $
        case cls of
          CExpr.DSL.NotTyCon        -> Not_class
          CExpr.DSL.LogicalTyCon    -> Logical_class
          CExpr.DSL.RelEqTyCon      -> RelEq_class
          CExpr.DSL.RelOrdTyCon     -> RelOrd_class
          CExpr.DSL.PlusTyCon       -> Plus_class
          CExpr.DSL.MinusTyCon      -> Minus_class
          CExpr.DSL.AddTyCon        -> Add_class
          CExpr.DSL.SubTyCon        -> Sub_class
          CExpr.DSL.MultTyCon       -> Mult_class
          CExpr.DSL.DivTyCon        -> Div_class
          CExpr.DSL.RemTyCon        -> Rem_class
          CExpr.DSL.ComplementTyCon -> Complement_class
          CExpr.DSL.BitwiseTyCon    -> Bitwise_class
          CExpr.DSL.ShiftTyCon      -> Shift_class
  CExpr.DSL.FamilyTyCon tc -> TGlobal $
    case tc of
      CExpr.DSL.PlusResTyCon       -> Plus_resTyCon
      CExpr.DSL.MinusResTyCon      -> Minus_resTyCon
      CExpr.DSL.AddResTyCon        -> Add_resTyCon
      CExpr.DSL.SubResTyCon        -> Sub_resTyCon
      CExpr.DSL.MultResTyCon       -> Mult_resTyCon
      CExpr.DSL.DivResTyCon        -> Div_resTyCon
      CExpr.DSL.RemResTyCon        -> Rem_resTyCon
      CExpr.DSL.ComplementResTyCon -> Complement_resTyCon
      CExpr.DSL.BitsResTyCon       -> Bitwise_resTyCon
      CExpr.DSL.ShiftResTyCon      -> Shift_resTyCon

mfunGlobal :: CExpr.DSL.MFun arity -> Global
mfunGlobal = \case
  CExpr.DSL.MUnaryPlus  -> Plus_plus
  CExpr.DSL.MUnaryMinus -> Minus_negate
  CExpr.DSL.MLogicalNot -> Not_not
  CExpr.DSL.MBitwiseNot -> Complement_complement
  CExpr.DSL.MMult       -> Mult_mult
  CExpr.DSL.MDiv        -> Div_div
  CExpr.DSL.MRem        -> Rem_rem
  CExpr.DSL.MAdd        -> Add_add
  CExpr.DSL.MSub        -> Sub_minus
  CExpr.DSL.MShiftLeft  -> Shift_shiftL
  CExpr.DSL.MShiftRight -> Shift_shiftR
  CExpr.DSL.MRelLT      -> RelOrd_lt
  CExpr.DSL.MRelLE      -> RelOrd_le
  CExpr.DSL.MRelGT      -> RelOrd_gt
  CExpr.DSL.MRelGE      -> RelOrd_ge
  CExpr.DSL.MRelEQ      -> RelEq_eq
  CExpr.DSL.MRelNE      -> RelEq_uneq
  CExpr.DSL.MBitwiseAnd -> Bitwise_and
  CExpr.DSL.MBitwiseXor -> Bitwise_xor
  CExpr.DSL.MBitwiseOr  -> Bitwise_or
  CExpr.DSL.MLogicalAnd -> Logical_and
  CExpr.DSL.MLogicalOr  -> Logical_or
  CExpr.DSL.MTuple @n   -> Tuple_constructor $ 2 + Fin.reflectToNum @n Proxy

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

