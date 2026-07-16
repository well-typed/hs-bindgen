module HsBindgen.Internal.Macro.Translation.Value (
    translateMacroValue
  ) where

import Data.Text qualified as Text
import Data.Type.Nat qualified as Nat
import Data.Vec.Lazy qualified as Vec
import DeBruijn (EmptyCtx, Idx (..), Size (..), rzeroAdd)

import C.Type qualified as Runtime

import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck
import C.Expr.Typecheck qualified as CExpr
import C.Expr.Typecheck.Interface.Value qualified as V
import C.Expr.Typecheck.Type qualified as CExpr

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST
import HsBindgen.Config.MangleCandidate
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Internal.Macro.CExpr (CExpr)
import HsBindgen.Internal.Macro.CExpr qualified as Macro
import HsBindgen.Internal.Macro.Global
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

translateMacroValue ::
     Hs.Name Hs.NsVar
  -> Macro.TypecheckedValue CExpr Hs.TermName
  -> Maybe HsDoc.Comment
  -> Binding
translateMacroValue name (Macro.TypecheckedValueCExpr expr) comment =
    Binding
      { name       = Hs.ExportedName name
      , parameters = []
      , result     = Result (translateType (fmap snd expr.macroValueType)) Nothing
      , body       = translateBody expr
      , pragmas    = []
      , comment    = comment
      }

{-------------------------------------------------------------------------------
  Translate macro value: type
-------------------------------------------------------------------------------}

translateType :: CExpr.Quant (CExpr.Type CExpr.Ty) -> ClosedType
translateType qty@(CExpr.Quant @n _) =
    case CExpr.mkQuantTyBody qty of
      CExpr.QuantTyBody{quantTyQuant = cts, quantTyBody = ty} ->
        topLevelForall (snd <$> CExpr.tyVarNames @n) cts ty

topLevelForall ::
     Vec n Text
  -> [CExpr.Type CExpr.Ct]
  -> CExpr.Type CExpr.Ty
  -> ClosedType
topLevelForall args cts body =
    TForall
      ((\t -> NameHint (Text.unpack t)) <$> args)
      (rzeroAdd (vecSize args))
      (map (typeCt lookupIdx) cts)
      (typeTy lookupIdx body)
  where
    qtvs    = addIndices args
    lookupIdx name =
      foldr (\(t, i) acc -> if t == name then i else acc)
            (panicPure $
              "translateMacroValue: unbound type variable: "
              <> Text.unpack name)
            qtvs

typeCt :: (Text -> Idx n) -> CExpr.Type CExpr.Ct -> SType n
typeCt env = \case
    CExpr.TyConAppTy cls as ->
      tAppN (tyCon cls) (typeTy env <$> as)
    CExpr.NomEqPred a b ->
      tAppN TEq [typeTy env a, typeTy env b]

typeTy :: forall n. (Text -> Idx n) -> CExpr.Type CExpr.Ty -> SType n
typeTy env = go
  where
    go :: CExpr.Type CExpr.Ty -> SType n
    go CExpr.String =
        tBindgenGlobal ByteString_type
    go (CExpr.TyVarTy tv) =
        TBound (env (CExpr.tyVarName tv))
    go (CExpr.FunTy as r) =
        foldr (TFun . go) (go r) as
    go (CExpr.TyConAppTy tc as) =
        case simpleTyConApp tc as of
          Just ty -> tBindgenGlobal ty
          Nothing -> tAppN (tyCon tc) (go <$> as)

simpleTyConApp ::
     CExpr.TyCon n CExpr.Ty
  -> Vec n (CExpr.Type CExpr.Ty)
  -> Maybe BindgenGlobalType
simpleTyConApp (CExpr.GenerativeTyCon (CExpr.DataTyCon tc)) (arg ::: VNil) =
    case (tc, arg) of
      ( CExpr.IntLikeTyCon
        , CExpr.TyConAppTy
            (CExpr.GenerativeTyCon (CExpr.DataTyCon (CExpr.PrimIntInfoTyCon t)))
            VNil
        ) -> Just $ dslIntegral t
      ( CExpr.FloatLikeTyCon
        , CExpr.TyConAppTy
            (CExpr.GenerativeTyCon (CExpr.DataTyCon (CExpr.PrimFloatInfoTyCon t)))
            VNil
        ) -> Just $ runtimeFloating t
      _ -> Nothing
simpleTyConApp _ _ =
    Nothing

tyCon :: CExpr.TyCon args res -> SType ctx
tyCon (CExpr.GenerativeTyCon (CExpr.DataTyCon tc))  = dataTyCon   tc
tyCon (CExpr.GenerativeTyCon (CExpr.ClassTyCon tc)) = TGlobal $ cExprGlobalType $ classTyCon  tc
tyCon (CExpr.FamilyTyCon tc)                        = TGlobal $ cExprGlobalType $ familyTyCon tc

dataTyCon :: CExpr.DataTyCon n -> SType ctx
dataTyCon = \case
    CExpr.TupleTyCon (Nat.SS' (Nat.SS' n)) -> TBoxedTup $ Plus2 $ Nat.snatToNatural n
    CExpr.VoidTyCon                        -> tBindgenGlobal Void_type
    CExpr.PrimIntInfoTyCon tc              -> tBindgenGlobal $ dslIntegral tc
    CExpr.PrimFloatInfoTyCon tc            -> tBindgenGlobal $ runtimeFloating tc
    CExpr.PtrTyCon                         -> tBindgenGlobal Foreign_Ptr_type
    CExpr.CharLitTyCon                     -> tBindgenGlobal CChar_type
    CExpr.IntLikeTyCon   -> panicPure "translateMacroValue: should have been handled by simpleTyConApp: IntLikeTyCon"
    CExpr.FloatLikeTyCon -> panicPure "translateMacroValue: should have been handled by simpleTyConApp: FloatLikeTyCon"
    CExpr.MacroTypeTyCon -> panicPure "translateMacroValue: unexpected type (kind): type"

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
    Runtime.Short    Runtime.Signed   -> CShort_type
    Runtime.Short    Runtime.Unsigned -> CUShort_type
    Runtime.Int      Runtime.Signed   -> CInt_type
    Runtime.Int      Runtime.Unsigned -> CUInt_type
    Runtime.Long     Runtime.Signed   -> CLong_type
    Runtime.Long     Runtime.Unsigned -> CULong_type
    Runtime.LongLong Runtime.Signed   -> CLLong_type
    Runtime.LongLong Runtime.Unsigned -> CULLong_type
    Runtime.PtrDiff                   -> CPtrdiff_type

runtimeFloating :: Runtime.FloatingType -> BindgenGlobalType
runtimeFloating = \case
    Runtime.FloatType  -> CFloat_type
    Runtime.DoubleType -> CDouble_type

{-------------------------------------------------------------------------------
  Translate macro value: body
-------------------------------------------------------------------------------}

translateBody ::
     CExpr.TypecheckedMacroValueExpr Hs.TermName
  -> ClosedExpr
translateBody (CExpr.TypecheckedMacroValueExpr params body _) =
    lambdaN (Vec.reverse (identifierToHint <$> params)) (mexpr body)

identifierToHint :: CExpr.Identifier -> NameHint
identifierToHint nm =
    toHint $ mangleCandidateDefaultFallback fallback nm.getIdentifier
  where
    fallback :: Hs.Name Hs.NsVar
    fallback = Hs.UnsafeName "x"

    toHint :: Hs.Name Hs.NsVar -> NameHint
    toHint x = NameHint $ Text.unpack x.text

mexpr :: forall ctx. V.Expr ctx Hs.TermName -> SExpr ctx
mexpr = goExpr
  where
    goExpr :: V.Expr ctx Hs.TermName -> SExpr ctx
    goExpr = \case
      V.Literal lit       -> goLiteral lit
      V.LocalParam i      -> EBound i
      V.Var termName args -> eAppN (EFree termName) (map goExpr args)
      V.App fun xs        -> eAppN (mfun fun) (Vec.toList (fmap goExpr xs))

    goLiteral :: CExpr.ValueLit -> SExpr ctx
    goLiteral = \case
      CExpr.ValueInt    x -> integerLiteral  x
      CExpr.ValueFloat  x -> floatingLiteral x
      CExpr.ValueChar   x -> charLiteral     x
      CExpr.ValueString x -> stringLiteral   x

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
    CExpr.MTuple @n   -> EBoxedTup $ Plus2 $ Nat.reflectToNum @n Proxy
  where
    cExpr = EGlobal . cExprGlobalTerm

integerLiteral :: CExpr.IntegerLiteral -> SExpr ctx
integerLiteral lit =
    EIntegral (CExpr.integerLiteralValue lit) $
      Just $ tBindgenGlobal $
        runtimeIntegral $ Runtime.IntLike (CExpr.integerLiteralType lit)

charLiteral :: CExpr.CharLiteral -> SExpr ctx
charLiteral lit = ECChar (CExpr.charLiteralValue lit)

stringLiteral :: CExpr.StringLiteral -> SExpr ctx
stringLiteral lit = ECString (CExpr.stringLiteralValue lit)

floatingLiteral :: CExpr.FloatingLiteral -> SExpr ctx
floatingLiteral lit =
    case CExpr.floatingLiteralType lit of
      Runtime.FloatType  ->
        EFloat
          (CExpr.floatingLiteralFloatValue  lit)
          (tBindgenGlobal CFloat_type)
      Runtime.DoubleType ->
        EDouble
          (CExpr.floatingLiteralDoubleValue lit)
          (tBindgenGlobal CDouble_type)

{-------------------------------------------------------------------------------
  Auxiliary: AST construction
-------------------------------------------------------------------------------}

eAppN :: Foldable f => SExpr ctx -> f (SExpr ctx) -> SExpr ctx
eAppN = foldl' EApp

lambdaN :: Vec ctx NameHint -> SExpr ctx -> SExpr EmptyCtx
lambdaN VNil       expr = expr
lambdaN (n ::: ns) expr = lambdaN ns $ ELam n expr

tAppN :: Foldable f => SType ctx -> f (SType ctx) -> SType ctx
tAppN = foldl' TApp

{-------------------------------------------------------------------------------
  Auxiliary: de Bruijn
-------------------------------------------------------------------------------}

vecSize :: Vec n a -> Size n
vecSize VNil       = SZ
vecSize (_ ::: xs) = SS (vecSize xs)

addIndices :: Vec n a -> Vec n (a, Idx n)
addIndices VNil       = VNil
addIndices (x ::: xs) = (x, IZ) ::: fmap (second IS) (addIndices xs)
