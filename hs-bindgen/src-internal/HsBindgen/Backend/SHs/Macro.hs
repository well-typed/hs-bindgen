module HsBindgen.Backend.SHs.Macro (
    translateMacroExpr
  ) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Type.Nat (pattern SS')
import Data.Type.Nat qualified as Fin
import DeBruijn (EmptyCtx, Idx (..), Size (..), rzeroAdd)

import C.Char qualified as Runtime
import C.Type (Sign (Signed, Unsigned))
import C.Type qualified as Runtime

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck.Interface.Value qualified as V
import C.Expr.Typecheck.Type (Kind (Ct, Ty))
import C.Expr.Typecheck.Type qualified as CExpr

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST
import HsBindgen.Config.MangleCandidate (mangleCandidateDefaultFallback)
import HsBindgen.Errors
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

{-------------------------------------------------------------------------------
  External API
-------------------------------------------------------------------------------}

translateMacroExpr :: Hs.MacroExpr -> SDecl
translateMacroExpr macro = DBinding Binding{
      name       = Hs.ExportedName macro.name
    , parameters = []
    , result     = Result (translateType macro.expr.typ) Nothing
    , body       = translateBody macro.expr.args macro.expr.body
    , pragmas    = []
    , comment    = macro.comment
    }

{-------------------------------------------------------------------------------
  Translate type
-------------------------------------------------------------------------------}

translateType :: CExpr.Quant (CExpr.Type Ty) -> ClosedType
translateType qty@(CExpr.Quant @n _) =
    case CExpr.mkQuantTyBody qty of
      CExpr.QuantTyBody{quantTyQuant = cts, quantTyBody = ty} ->
        quantTyBody (snd <$> CExpr.tyVarNames @n) cts ty

quantTyBody :: Vec n Text -> [CExpr.Type Ct] -> CExpr.Type Ty -> ClosedType
quantTyBody args cts body =
    topLevelForall nameHint args $ \env -> (
        map (typeCt env) cts
      , typeTy env body
      )
  where
    -- Use of 'NameHint' is justified because 'CExpr.tyVarNames' generates valid
    -- names.
    nameHint :: Text -> NameHint
    nameHint = NameHint . T.unpack

typeCt :: Map Text (Idx ctx) -> CExpr.Type Ct -> SType ctx
typeCt env = \case
    CExpr.TyConAppTy cls as ->
      tAppN (tyCon cls) (typeTy env <$> as)
    CExpr.NomEqPred a b ->
      tAppN TEq [typeTy env a, typeTy env b]

typeTy :: forall ctx. Map Text (Idx ctx) -> CExpr.Type Ty -> SType ctx
typeTy env = go
  where
    go :: CExpr.Type Ty -> SType ctx
    go (CExpr.TyVarTy tv) =
        case Map.lookup (CExpr.tyVarName tv) env of
          Just n  -> TBound n
          Nothing -> panicPure $ "Unbound type variable " ++ show tv
    go (CExpr.FunTy as r) =
        foldr (TFun . go) (go r) as
    go (CExpr.TyConAppTy tc as) =
        case simpleTyConApp tc as of
          Just ty -> tBindgenGlobal ty
          Nothing -> tAppN (tyCon tc) (go <$> as)

-- | Convert @IntLike t@ and @FloatLike t@ to Haskell types.
--
-- The macro typechecker will never produce a type such as @IntLike a@ for
-- a skolem type variable @a@, because this type has no Haskell counterpart.
--
-- See 'C.Expr.Typecheck.Expr.isAtomicType'.
simpleTyConApp ::
     CExpr.TyCon n Ty
  -> Vec n (CExpr.Type Ty)
  -> Maybe BindgenGlobalType
simpleTyConApp (CExpr.GenerativeTyCon (CExpr.DataTyCon tc)) (arg ::: VNil) =
    case (tc, arg) of

      (   CExpr.IntLikeTyCon
        , CExpr.TyConAppTy
            (CExpr.GenerativeTyCon (CExpr.DataTyCon (CExpr.PrimIntInfoTyCon t)))
            VNil
        ) -> Just $ dslIntegral t

      (     CExpr.FloatLikeTyCon
          , CExpr.TyConAppTy
              (CExpr.GenerativeTyCon (CExpr.DataTyCon (CExpr.PrimFloatInfoTyCon t)))
              VNil
        ) -> Just $ runtimeFloating t

      _otherwise ->
        Nothing

simpleTyConApp _ _ =
    Nothing

{-------------------------------------------------------------------------------
  Translate body
-------------------------------------------------------------------------------}

translateBody ::
     [CExpr.Name]
  -> V.Expr (Id Final)
  -> SExpr EmptyCtx
translateBody macroArgs expr =
    topLevelLambdaN idNameHint macroArgs (flip mexpr expr)

mexpr :: forall ctx.
     Map CExpr.Name (Idx ctx)
  -> V.Expr (Id Final)
  -> SExpr ctx
mexpr env =
    goExpr
  where
    goExpr :: V.Expr (Id Final) -> SExpr ctx
    goExpr = \case
      V.Literal lit ->
        goLiteral lit
      V.LocalArg xId ->
        case Map.lookup xId env of
          Just i  -> EBound i
          -- TODO-D: Try DeBruijn.
          Nothing -> panicPure "unexpected local arg without name"
      V.Var xId args ->
        eAppN (EFree (macroIdToHsName xId)) (goExpr <$> args)
      V.App fun xs ->
        eAppN (mfun fun) (goExpr <$> xs)

    goLiteral :: CExpr.ValueLit -> SExpr ctx
    goLiteral = \case
      -- Literals
      CExpr.ValueInt    x -> integerLiteral  x
      CExpr.ValueFloat  x -> floatingLiteral x
      CExpr.ValueChar   x -> charLiteral     x
      CExpr.ValueString x -> stringLiteral   x

{-------------------------------------------------------------------------------
  Names
-------------------------------------------------------------------------------}

idNameHint :: CExpr.Name -> NameHint
idNameHint xId = toHint $ mangleCandidateDefaultFallback fallback xId.getName
  where
    fallback :: Hs.Name Hs.NsVar
    fallback = Hs.UnsafeName "x"

    toHint :: Hs.Name Hs.NsVar -> NameHint
    toHint x = NameHint $ T.unpack x.text

macroIdToHsName :: Id Final -> Hs.TermName
macroIdToHsName namePair =
    Hs.ExportedName $ Hs.assertNs (Proxy @Hs.NsVar) namePair.hsName

{-------------------------------------------------------------------------------
  Literals
-------------------------------------------------------------------------------}

integerLiteral :: CExpr.IntegerLiteral -> SExpr ctx
integerLiteral lit =
    EIntegral (CExpr.integerLiteralValue lit) $
      Just $ tBindgenGlobal $
        runtimeIntegral $ Runtime.IntLike (CExpr.integerLiteralType lit)

charLiteral :: CExpr.CharLiteral -> SExpr ctx
charLiteral lit =
    EChar $ CExpr.charLiteralValue lit

stringLiteral :: CExpr.StringLiteral -> SExpr ctx
stringLiteral lit =
    ECString $ foldMap Runtime.charValue (CExpr.stringLiteralValue lit)

floatingLiteral :: CExpr.FloatingLiteral -> SExpr ctx
floatingLiteral lit =
    case CExpr.floatingLiteralType lit of
      Runtime.FloatType ->
        EFloat  (CExpr.floatingLiteralFloatValue  lit) (tBindgenGlobal CFloat_type)
      Runtime.DoubleType ->
        EDouble (CExpr.floatingLiteralDoubleValue lit) (tBindgenGlobal CDouble_type)

{-------------------------------------------------------------------------------
  Primitive types
-------------------------------------------------------------------------------}

dslIntegral :: CExpr.IntegralType -> BindgenGlobalType
dslIntegral = \case
    CExpr.CIntegralType primIntTy -> runtimeIntegral primIntTy
    CExpr.HsIntType               -> Int_type

runtimeIntegral :: Runtime.IntegralType -> BindgenGlobalType
runtimeIntegral = \case
    Runtime.Bool       -> CBool_type
    Runtime.CharLike c -> runtimeCharLike c
    Runtime.IntLike i  -> runtimeIntLike i

runtimeCharLike :: Runtime.CharLikeType -> BindgenGlobalType
runtimeCharLike = \case
    Runtime.Char  -> CChar_type
    Runtime.SChar -> CSChar_type
    Runtime.UChar -> CUChar_type

runtimeIntLike :: Runtime.IntLikeType -> BindgenGlobalType
runtimeIntLike = \case
    Runtime.Short    Signed   -> CShort_type
    Runtime.Short    Unsigned -> CUShort_type
    Runtime.Int      Signed   -> CInt_type
    Runtime.Int      Unsigned -> CUInt_type
    Runtime.Long     Signed   -> CLong_type
    Runtime.Long     Unsigned -> CULong_type
    Runtime.LongLong Signed   -> CLLong_type
    Runtime.LongLong Unsigned -> CULLong_type
    Runtime.PtrDiff           -> CPtrdiff_type

runtimeFloating :: Runtime.FloatingType -> BindgenGlobalType
runtimeFloating = \case
    Runtime.FloatType  -> CFloat_type
    Runtime.DoubleType -> CDouble_type

{-------------------------------------------------------------------------------
  Globals
-------------------------------------------------------------------------------}

tyCon :: CExpr.TyCon args res -> SType ctx
tyCon (CExpr.GenerativeTyCon (CExpr.DataTyCon tc))  = dataTyCon   tc
tyCon (CExpr.GenerativeTyCon (CExpr.ClassTyCon tc)) = TGlobal $ cExprGlobalType $ classTyCon  tc
tyCon (CExpr.FamilyTyCon tc)                      = TGlobal $ cExprGlobalType $ familyTyCon tc

dataTyCon :: CExpr.DataTyCon n -> SType ctx
dataTyCon = \case
    CExpr.TupleTyCon (SS' (SS' n)) -> TBoxedTup $ Plus2 $ Fin.snatToNatural n
    CExpr.VoidTyCon                -> tBindgenGlobal Void_type
    CExpr.PrimIntInfoTyCon tc      -> tBindgenGlobal $ dslIntegral tc
    CExpr.PrimFloatInfoTyCon tc    -> tBindgenGlobal $ runtimeFloating tc
    CExpr.PtrTyCon                 -> tBindgenGlobal Foreign_Ptr_type
    CExpr.CharLitTyCon             -> TGlobal $ cExprGlobalType CharValue_type

    -- Handled by 'simpleTyConApp'
    CExpr.IntLikeTyCon   -> panicPure "Should have been handled by simpleTyConApp: IntLikeTyCon"
    CExpr.FloatLikeTyCon -> panicPure "Should have been handled by simpleTyConApp: FloatLikeTyCon"

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1900>
    --
    -- Split the type language into types of types, and types of values.
    CExpr.MacroTypeTyCon -> panicPure "Unexpected type (kind): type"

classTyCon :: CExpr.ClassTyCon args -> CExprGlobalType
classTyCon = \case
    CExpr.NotTyCon        -> Not_class
    CExpr.LogicalTyCon    -> Logical_class
    CExpr.RelEqTyCon      -> RelEq_class
    CExpr.RelOrdTyCon     -> RelOrd_class
    CExpr.PlusTyCon       -> Plus_class
    CExpr.MinusTyCon      -> Minus_class
    CExpr.AddTyCon        -> Add_class
    CExpr.SubTyCon        -> Sub_class
    CExpr.MultTyCon       -> Mult_class
    CExpr.DivTyCon        -> Div_class
    CExpr.RemTyCon        -> Rem_class
    CExpr.ComplementTyCon -> Complement_class
    CExpr.BitwiseTyCon    -> Bitwise_class
    CExpr.ShiftTyCon      -> Shift_class

familyTyCon :: CExpr.FamilyTyCon args -> CExprGlobalType
familyTyCon = \case
    CExpr.PlusResTyCon       -> Plus_resTyCon
    CExpr.MinusResTyCon      -> Minus_resTyCon
    CExpr.AddResTyCon        -> Add_resTyCon
    CExpr.SubResTyCon        -> Sub_resTyCon
    CExpr.MultResTyCon       -> Mult_resTyCon
    CExpr.DivResTyCon        -> Div_resTyCon
    CExpr.RemResTyCon        -> Rem_resTyCon
    CExpr.ComplementResTyCon -> Complement_resTyCon
    CExpr.BitsResTyCon       -> Bitwise_resTyCon
    CExpr.ShiftResTyCon      -> Shift_resTyCon

mfun :: CExpr.VaFun arity -> SExpr ctx
mfun = \case
    CExpr.MUnaryPlus  -> cExpr Plus_plus
    CExpr.MUnaryMinus -> cExpr Minus_negate
    CExpr.MLogicalNot -> cExpr Not_not
    CExpr.MBitwiseNot -> cExpr Complement_complement
    CExpr.MMult       -> cExpr Mult_mult
    CExpr.MDiv        -> cExpr Div_div
    CExpr.MRem        -> cExpr Rem_rem
    CExpr.MAdd        -> cExpr Add_add
    CExpr.MSub        -> cExpr Sub_minus
    CExpr.MShiftLeft  -> cExpr Shift_shiftL
    CExpr.MShiftRight -> cExpr Shift_shiftR
    CExpr.MRelLT      -> cExpr RelOrd_lt
    CExpr.MRelLE      -> cExpr RelOrd_le
    CExpr.MRelGT      -> cExpr RelOrd_gt
    CExpr.MRelGE      -> cExpr RelOrd_ge
    CExpr.MRelEQ      -> cExpr RelEq_eq
    CExpr.MRelNE      -> cExpr RelEq_uneq
    CExpr.MBitwiseAnd -> cExpr Bitwise_and
    CExpr.MBitwiseXor -> cExpr Bitwise_xor
    CExpr.MBitwiseOr  -> cExpr Bitwise_or
    CExpr.MLogicalAnd -> cExpr Logical_and
    CExpr.MLogicalOr  -> cExpr Logical_or
    CExpr.MTuple @n   -> EBoxedTup $ Plus2 $ Fin.reflectToNum @n Proxy
  where
    cExpr = EGlobal . cExprGlobalTerm

{-------------------------------------------------------------------------------
  Auxiliary: AST construction
-------------------------------------------------------------------------------}

-- | Construct n-ary application
eAppN :: Foldable f => SExpr ctx -> f (SExpr ctx) -> SExpr ctx
eAppN = foldl' EApp

-- | Construct n-ary type application
tAppN :: Foldable f => SType ctx -> f (SType ctx) -> SType ctx
tAppN = foldl' TApp

-- | Construct n-ary top-level lambda
topLevelLambdaN :: forall a.
     Ord a
  => (a -> NameHint)
  -> [a]
  -> (forall ctx. Map a (Idx ctx) -> SExpr ctx)
  -> SExpr EmptyCtx
topLevelLambdaN nameHint args mkBody =
    go Map.empty args
  where
    go :: forall ctx. Map a (Idx ctx) -> [a] -> SExpr ctx
    go env []     = mkBody env
    go env (n:ns) = ELam (nameHint n) $ go (Map.insert n IZ (fmap IS env)) ns

-- | Top-level quantified type
topLevelForall :: forall n a.
     Ord a
  => (a -> NameHint)
  -> Vec n a
  -> (Map a (Idx n) -> ([SType n], SType n)) -- ^ Constraints and type
  -> SType EmptyCtx
topLevelForall nameHint args body =
    uncurry (TForall (nameHint <$> args) (rzeroAdd $ vecSize args))$ body env
  where
    qtvs :: Vec n (a, Idx n)
    qtvs = addIndices args

    env :: Map a (Idx n)
    env = Map.fromList $ toList qtvs

{-------------------------------------------------------------------------------
  Auxiliary de Bruijn
-------------------------------------------------------------------------------}

vecSize :: Vec n a -> Size n
vecSize VNil       = SZ
vecSize (_ ::: xs) = SS (vecSize xs)

addIndices :: Vec n a -> Vec n (a, Idx n)
addIndices VNil       = VNil
addIndices (x ::: xs) = (x, IZ) ::: fmap (second IS) (addIndices xs)
