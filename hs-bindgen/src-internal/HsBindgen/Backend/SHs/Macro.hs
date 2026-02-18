module HsBindgen.Backend.SHs.Macro (
    translateMacroExpr
  ) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Type.Nat qualified as Fin
import DeBruijn (EmptyCtx, Idx (..), Size (..), rzeroAdd)

import C.Char qualified as Runtime
import C.Type (Sign (Signed, Unsigned))
import C.Type qualified as Runtime

import C.Expr.Syntax qualified as DSL
import C.Expr.Typecheck.Type (Kind (Ct, Ty))
import C.Expr.Typecheck.Type qualified as DSL

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST
import HsBindgen.Config.FixCandidate (FixCandidate)
import HsBindgen.Config.FixCandidate qualified as FixCandidate
import HsBindgen.Errors
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

{-------------------------------------------------------------------------------
  External API
-------------------------------------------------------------------------------}

translateMacroExpr :: Hs.MacroExpr -> SDecl
translateMacroExpr macro = DBinding Binding{
      name       = macro.name
    , parameters = []
    , result     = Result (translateType macro.expr.typ) Nothing
    , body       = translateBody macro.expr.args macro.expr.body
    , pragmas    = []
    , comment    = macro.comment
    }

{-------------------------------------------------------------------------------
  Translate type
-------------------------------------------------------------------------------}

translateType :: DSL.Quant (DSL.Type Ty) -> ClosedType
translateType qty@(DSL.Quant @n _) =
    case DSL.mkQuantTyBody qty of
      DSL.QuantTyBody{quantTyQuant = cts, quantTyBody = ty} ->
        quantTyBody (snd <$> DSL.tyVarNames @n) cts ty

quantTyBody :: Vec n Text -> [DSL.Type Ct] -> DSL.Type Ty -> ClosedType
quantTyBody args cts body =
    topLevelForall nameHint args $ \env -> (
        map (typeCt env) cts
      , typeTy env body
      )
  where
    nameHint :: Text -> NameHint
    nameHint = fromString . T.unpack

typeCt :: Map Text (Idx ctx) -> DSL.Type Ct -> SType ctx
typeCt env = \case
    DSL.TyConAppTy cls as ->
      tAppN (tyCon cls) (typeTy env <$> as)
    DSL.NomEqPred a b ->
      tAppN TEq [typeTy env a, typeTy env b]

typeTy :: forall ctx. Map Text (Idx ctx) -> DSL.Type Ty -> SType ctx
typeTy env = go
  where
    go :: DSL.Type Ty -> SType ctx
    go (DSL.TyVarTy tv) =
        case Map.lookup (DSL.tyVarName tv) env of
          Just n  -> TBound n
          Nothing -> panicPure $ "typeT: unbound type variable " ++ show tv
    go (DSL.FunTy as r) =
        foldr (TFun . go) (go r) as
    go (DSL.TyConAppTy tc as) =
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
     DSL.TyCon n Ty
  -> Vec n (DSL.Type Ty)
  -> Maybe BindgenGlobalType
simpleTyConApp (DSL.GenerativeTyCon (DSL.DataTyCon tc)) (arg ::: VNil) =
    case (tc, arg) of

      (   DSL.IntLikeTyCon
        , DSL.TyConAppTy
            (DSL.GenerativeTyCon (DSL.DataTyCon (DSL.PrimIntInfoTyCon t)))
            VNil
        ) -> Just $ dslIntegral t

      (     DSL.FloatLikeTyCon
          , DSL.TyConAppTy
              (DSL.GenerativeTyCon (DSL.DataTyCon (DSL.PrimFloatInfoTyCon t)))
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
     [DSL.Name]
  -> DSL.MExpr p
  -> SExpr EmptyCtx
translateBody macroArgs expr =
    topLevelLambdaN cnameToHint macroArgs (flip mexpr expr)

mexpr :: forall ctx p.
     Map DSL.Name (Idx ctx)
  -> DSL.MExpr p
  -> SExpr ctx
mexpr env =
    goExpr
  where
    goExpr :: DSL.MExpr p -> SExpr ctx
    goExpr = \case
        DSL.MTerm t     -> goTerm t
        DSL.MApp _ f xs -> eAppN (mfun f) (goExpr <$> xs)

    goTerm :: DSL.MTerm p -> SExpr ctx
    goTerm = \case
        -- Literals
        DSL.MInt    x -> integerLiteral  x
        DSL.MFloat  x -> floatingLiteral x
        DSL.MChar   x -> charLiteral     x
        DSL.MString x -> stringLiteral   x

        -- Variables
        DSL.MVar _ cname args ->
          case Map.lookup cname env of
            Just i  -> EBound i
            Nothing -> eAppN (EFree $ macroName cname) (goExpr <$> args)

{-------------------------------------------------------------------------------
  Names
-------------------------------------------------------------------------------}

cnameToHint :: DSL.Name -> NameHint
cnameToHint (DSL.Name t) = fromString (T.unpack t)

-- | Construct Haskell name for macro
--
-- TODO: This should be done as part of the NameMangler frontend pass.
macroName :: DSL.Name -> Hs.Name Hs.NsVar
macroName (DSL.Name cName) =
    case FixCandidate.fixCandidate fix cName of
      Just hsName -> Hs.ExportedName hsName
      Nothing     ->
        panicPure $ "Unable to construct name for macro " ++ show cName
  where
    fix :: FixCandidate Maybe
    fix = FixCandidate.fixCandidateDefault

{-------------------------------------------------------------------------------
  Literals
-------------------------------------------------------------------------------}

integerLiteral :: DSL.IntegerLiteral -> SExpr ctx
integerLiteral lit =
    EIntegral (DSL.integerLiteralValue lit) $
      Just $ tBindgenGlobal $
        runtimeIntegral $ Runtime.IntLike (DSL.integerLiteralType lit)

charLiteral :: DSL.CharLiteral -> SExpr ctx
charLiteral lit =
    EChar $ DSL.charLiteralValue lit

stringLiteral :: DSL.StringLiteral -> SExpr ctx
stringLiteral lit =
    ECString $ foldMap Runtime.charValue (DSL.stringLiteralValue lit)

floatingLiteral :: DSL.FloatingLiteral -> SExpr ctx
floatingLiteral lit =
    case DSL.floatingLiteralType lit of
      Runtime.FloatType ->
        EFloat  (DSL.floatingLiteralFloatValue  lit) (tBindgenGlobal CFloat_type)
      Runtime.DoubleType ->
        EDouble (DSL.floatingLiteralDoubleValue lit) (tBindgenGlobal CDouble_type)

{-------------------------------------------------------------------------------
  Primitive types
-------------------------------------------------------------------------------}

dslIntegral :: DSL.IntegralType -> BindgenGlobalType
dslIntegral = \case
    DSL.CIntegralType primIntTy -> runtimeIntegral primIntTy
    DSL.HsIntType               -> Int_type

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

tyCon :: DSL.TyCon args res -> SType ctx
tyCon (DSL.GenerativeTyCon (DSL.DataTyCon tc))  = dataTyCon   tc
tyCon (DSL.GenerativeTyCon (DSL.ClassTyCon tc)) = TGlobal $ cExprGlobalType $ classTyCon  tc
tyCon (DSL.FamilyTyCon tc)                      = TGlobal $ cExprGlobalType $ familyTyCon tc

dataTyCon :: DSL.DataTyCon n -> SType ctx
dataTyCon = \case
    DSL.TupleTyCon n          -> TBoxedOpenTup $ fromIntegral n
    DSL.VoidTyCon             -> tBindgenGlobal Void_type
    DSL.PrimIntInfoTyCon tc   -> tBindgenGlobal $ dslIntegral tc
    DSL.PrimFloatInfoTyCon tc -> tBindgenGlobal $ runtimeFloating tc
    DSL.PtrTyCon              -> tBindgenGlobal Foreign_Ptr_type
    DSL.CharLitTyCon          -> TGlobal $ cExprGlobalType CharValue_type

    -- Handled by 'simpleTyConApp'
    DSL.IntLikeTyCon   -> panicPure "dataTyCon IntLikeTyCon"
    DSL.FloatLikeTyCon -> panicPure "dataTyCon FloatLikeTyCon"

classTyCon :: DSL.ClassTyCon args -> CExprGlobalType
classTyCon = \case
    DSL.NotTyCon        -> Not_class
    DSL.LogicalTyCon    -> Logical_class
    DSL.RelEqTyCon      -> RelEq_class
    DSL.RelOrdTyCon     -> RelOrd_class
    DSL.PlusTyCon       -> Plus_class
    DSL.MinusTyCon      -> Minus_class
    DSL.AddTyCon        -> Add_class
    DSL.SubTyCon        -> Sub_class
    DSL.MultTyCon       -> Mult_class
    DSL.DivTyCon        -> Div_class
    DSL.RemTyCon        -> Rem_class
    DSL.ComplementTyCon -> Complement_class
    DSL.BitwiseTyCon    -> Bitwise_class
    DSL.ShiftTyCon      -> Shift_class

familyTyCon :: DSL.FamilyTyCon args -> CExprGlobalType
familyTyCon = \case
    DSL.PlusResTyCon       -> Plus_resTyCon
    DSL.MinusResTyCon      -> Minus_resTyCon
    DSL.AddResTyCon        -> Add_resTyCon
    DSL.SubResTyCon        -> Sub_resTyCon
    DSL.MultResTyCon       -> Mult_resTyCon
    DSL.DivResTyCon        -> Div_resTyCon
    DSL.RemResTyCon        -> Rem_resTyCon
    DSL.ComplementResTyCon -> Complement_resTyCon
    DSL.BitsResTyCon       -> Bitwise_resTyCon
    DSL.ShiftResTyCon      -> Shift_resTyCon

mfun :: DSL.MFun arity -> SExpr ctx
mfun = \case
    DSL.MUnaryPlus  -> cExpr Plus_plus
    DSL.MUnaryMinus -> cExpr Minus_negate
    DSL.MLogicalNot -> cExpr Not_not
    DSL.MBitwiseNot -> cExpr Complement_complement
    DSL.MMult       -> cExpr Mult_mult
    DSL.MDiv        -> cExpr Div_div
    DSL.MRem        -> cExpr Rem_rem
    DSL.MAdd        -> cExpr Add_add
    DSL.MSub        -> cExpr Sub_minus
    DSL.MShiftLeft  -> cExpr Shift_shiftL
    DSL.MShiftRight -> cExpr Shift_shiftR
    DSL.MRelLT      -> cExpr RelOrd_lt
    DSL.MRelLE      -> cExpr RelOrd_le
    DSL.MRelGT      -> cExpr RelOrd_gt
    DSL.MRelGE      -> cExpr RelOrd_ge
    DSL.MRelEQ      -> cExpr RelEq_eq
    DSL.MRelNE      -> cExpr RelEq_uneq
    DSL.MBitwiseAnd -> cExpr Bitwise_and
    DSL.MBitwiseXor -> cExpr Bitwise_xor
    DSL.MBitwiseOr  -> cExpr Bitwise_or
    DSL.MLogicalAnd -> cExpr Logical_and
    DSL.MLogicalOr  -> cExpr Logical_or
    DSL.MTuple @n   -> EBoxedOpenTup $ 2 + Fin.reflectToNum @n Proxy
  where
    cExpr = EGlobal . cExprGlobalTerm

{-------------------------------------------------------------------------------
  Auxiliary: AST construction

  TODO: Should this live somewhere more general?
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
topLevelLambdaN nameHint args body =
    go Map.empty args
  where
    go :: forall ctx. Map a (Idx ctx) -> [a] -> SExpr ctx
    go env []     = body env
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
