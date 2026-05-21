module HsBindgen.Macro (
    -- * Type
    HasMacroTypes -- opaque
  , CExpr
    -- * Language
  , MacroLang     -- opaque
  , cExprLang
  ) where

import Control.Monad.Except (Except, MonadError (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Type.Nat qualified as Nat
import Data.Vec.Lazy qualified as Vec
import DeBruijn (EmptyCtx, Idx (..), Size (..), rzeroAdd)

import C.Char qualified as CChar
import C.Type qualified as Runtime

import C.Expr.Parse qualified as CExpr
import C.Expr.Syntax qualified as CExpr
import C.Expr.Typecheck qualified as CExpr
import C.Expr.Typecheck.Interface.Type qualified as T
import C.Expr.Typecheck.Interface.Value qualified as V
import C.Expr.Typecheck.Type qualified as CExpr

import Clang.CStandard
import Clang.HighLevel.Types

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Translation.Type qualified as Type
import HsBindgen.Backend.SHs.AST
import HsBindgen.Config.MangleCandidate
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Type
import HsBindgen.Frontend.Naming
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Interface
import HsBindgen.Macro.Type
import HsBindgen.NameHint

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

-- | Tag for the default C macro language, backed by @c-expr-dsl@.
data CExpr

instance HasMacroTypes CExpr where
  newtype ParsedMacroBody CExpr =
    ParsedMacroBodyCExpr CExpr.Macro
  newtype TypecheckedMacroTypeBody CExpr var =
    TypecheckedMacroTypeBodyCExpr  (CExpr.TypecheckedMacroTypeExpr  var)
  newtype TypecheckedMacroValueBody CExpr var =
    TypecheckedMacroValueBodyCExpr (CExpr.TypecheckedMacroValueExpr var)

deriving stock instance Show (ParsedMacroBody CExpr)
deriving stock instance Eq   (ParsedMacroBody CExpr)

deriving stock instance (Show var) => Show (TypecheckedMacroTypeBody CExpr var)
deriving stock instance (Eq var)   => Eq   (TypecheckedMacroTypeBody CExpr var)

deriving stock instance Functor     (TypecheckedMacroTypeBody CExpr)
deriving stock instance Foldable    (TypecheckedMacroTypeBody CExpr)
deriving stock instance Traversable (TypecheckedMacroTypeBody CExpr)

deriving stock instance (Show var) => Show        (TypecheckedMacroValueBody CExpr var)
deriving stock instance (Eq var)   => Eq          (TypecheckedMacroValueBody CExpr var)

deriving stock instance Functor     (TypecheckedMacroValueBody CExpr)
deriving stock instance Foldable    (TypecheckedMacroValueBody CExpr)
deriving stock instance Traversable (TypecheckedMacroValueBody CExpr)

{-------------------------------------------------------------------------------
  Language
-------------------------------------------------------------------------------}

-- | Default macro language implementation backed by @c-expr-dsl@.
--
-- The C standard is fixed at construction time; the resulting 'MacroLang' is
-- configuration-free.
cExprLang :: ClangCStandard -> MacroLang CExpr
cExprLang cStd = MacroLang
  { parseMacroBody           = cExprParseMacroBody cStd
  , parsedMacroDeps          = cExprParsedMacroDeps
  , typecheckMacroBodies     = cExprTypecheckMacroBodies
  , typecheckedMacroTypeDeps = cExprTypecheckedMacroTypeDeps
  , translateMacroType       = cExprTranslateMacroType
  , translateMacroValue      = cExprTranslateMacroValue
  }

cExprParseMacroBody ::
     ClangCStandard
  -> [Token TokenSpelling]
  -> Either MacroLangParseError (ParsedMacroBody CExpr)
cExprParseMacroBody cStd tokens =
    case CExpr.runParser (CExpr.parseMacro cStd) tokens of
      Right macro -> Right (ParsedMacroBodyCExpr macro)
      Left  err   -> Left  (MacroLangParseError err.parseError)

cExprParsedMacroDeps ::
     Set DeclId
  -> ParsedMacroBody CExpr
  -> [(ValOrRef, DeclId)]
cExprParsedMacroDeps declIds (ParsedMacroBodyCExpr macro) =
    case macro of
      CExpr.Macro _ _ _ expr -> goExpr ByValue expr
  where
    goExpr :: ValOrRef -> CExpr.Expr ctx CExpr.Ps -> [(ValOrRef, DeclId)]
    goExpr depTy = \case
      CExpr.Term term               -> goTerm depTy term
      -- Pointer: switch the dependency type to 'ByRef'.
      CExpr.TyApp CExpr.Pointer xs  -> concatMap (goExpr ByRef)  xs
      CExpr.TyApp CExpr.Const   xs  -> concatMap (goExpr depTy)  xs
      CExpr.VaApp _ _ xs            -> concatMap (goExpr depTy)  xs

    goTerm :: ValOrRef -> CExpr.Term ctx CExpr.Ps -> [(ValOrRef, DeclId)]
    goTerm depTy = \case
      CExpr.Literal lit   -> goLit depTy lit
      CExpr.LocalParam{}  -> []
      CExpr.Var _ nm args
        | Just x <- resolveBare nm.getName ->
            (depTy, x) : concatMap (goExpr depTy) args
        | otherwise ->
            concatMap (goExpr depTy) args

    goLit :: ValOrRef -> CExpr.Literal -> [(ValOrRef, DeclId)]
    goLit depTy = \case
      CExpr.TypeTagged tag nm
        | Just a <- resolveTagged (convertTagKind tag) nm.getName -> [(depTy, a)]
        | otherwise -> []
      CExpr.TypeLit{}  -> []
      CExpr.ValueLit{} -> []

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1952>
    --
    -- We should only resolve the declaration IDs of macro dependencies once.
    -- Now we resolve them twice: when we get the dependencies of parsed macros,
    -- and when we typecheck macros.

    -- | Resolves a bare name found in a parsed macro body
    --
    -- Result:
    --
    -- - @'Just' 'CNameKind'@: This bare name refers to another declaration that is in
    --   the 'DeclIndex'. For example, a macro, or a @typedef@.
    --
    -- - @'Nothing'@: This bare name is not in the set of known declarations. It
    --   may simply be non-existent, or refer to a built-in type or a system
    --   @typedef@ not present in the 'DeclIndex'. In this case, we /do not
    --   generate/ a dependency.
    resolveBare :: Text -> Maybe DeclId
    resolveBare nm
      | macroId   `Set.member` declIds = Just macroId
      | typedefId `Set.member` declIds = Just typedefId
      | otherwise                         = Nothing
      where
        macroId, typedefId :: DeclId
        macroId   = DeclId (CDeclName nm CNameKindMacro)    False
        typedefId = DeclId (CDeclName nm CNameKindOrdinary) False

    resolveTagged :: CTagKind -> Text -> Maybe DeclId
    resolveTagged tag nm
      | taggedId `Set.member` declIds = Just taggedId
      | otherwise                        = Nothing
      where
        taggedId :: DeclId
        taggedId = DeclId (CDeclName nm $ CNameKindTagged tag) False

cExprTypecheckMacroBodies ::
     Set DeclId
  -> [ParsedMacroBody CExpr]
  -> Map Text (MacroTypecheckResult CExpr)
cExprTypecheckMacroBodies declsInScope bodies =
    Map.mapKeysMonotonic (.getName) $ fmap convertResult $
      CExpr.tcMacros
        typedefs
        injectTypeName
        injectValueName
        injectNonAnonTaggedTypeName
        [m | ParsedMacroBodyCExpr m <- bodies]
  where
    typedefs :: Set CExpr.Name
    typedefs = Set.fromList $ [ (CExpr.Name declId.name.text)
                              | declId <- Set.toList declsInScope
                              , declId.name.kind == CNameKindOrdinary ]

    convertResult ::
         CExpr.MacroTcResult MacroTypecheckError DeclId
      -> MacroTypecheckResult CExpr
    convertResult = \case
      CExpr.MacroTcTypeExpr x ->
        MacroTypecheckType  (TypecheckedMacroTypeBodyCExpr  x)
      CExpr.MacroTcValueExpr x ->
        MacroTypecheckValue (TypecheckedMacroValueBodyCExpr x)
      CExpr.MacroTcInjectError e ->
        MacroTypecheckError e
      CExpr.MacroTcError err ->
        MacroTypecheckError $
          MacroTypecheckTypecheckError $
            MacroLangTypecheckError (Text.unpack (CExpr.pprMacroTcError err))

    injectTypeName ::
         CExpr.CTypeSource
      -> CExpr.Name
      -> DeclId
    injectTypeName = \case
        CExpr.FromTypedef ->
          \n -> DeclId (CDeclName n.getName CNameKindOrdinary) False
        CExpr.FromMacroType ->
          \n -> DeclId (CDeclName n.getName CNameKindMacro) False

    injectValueName :: CExpr.Name -> DeclId
    injectValueName (CExpr.Name n) =
        let dn = CDeclName{text = n , kind = CNameKindMacro}
        in  DeclId dn False

    injectNonAnonTaggedTypeName ::
         CExpr.TagKind
      -> CExpr.Name
      -> Except MacroTypecheckError DeclId
    injectNonAnonTaggedTypeName k n =
        if Set.member declId declsInScope then
          pure declId
        else
          throwError  $ MacroTypecheckUnresolvedTaggedType declId
      where
        declId :: DeclId
        declId = DeclId{
            name = CDeclName{
              text = n.getName
            , kind = CNameKindTagged (convertTagKind k)
            }
          , isAnon = False
          }

cExprTypecheckedMacroTypeDeps ::
     TypecheckedMacroTypeBody CExpr DeclId -> [(ValOrRef, DeclId)]
cExprTypecheckedMacroTypeDeps (TypecheckedMacroTypeBodyCExpr tcExpr) =
    -- 'T.Expr' is a unary type-application tree, so it carries at most one variable.
    case go ByValue tcExpr.macroTypeBody of
      Nothing -> []
      Just x  -> [x]
  where
    go :: ValOrRef -> T.Expr DeclId -> Maybe (ValOrRef, DeclId)
    go depTy = \case
      T.TypeLit{}       -> Nothing
      T.App T.Pointer e -> go ByRef e
      T.App T.Const   e -> go depTy e
      T.Var v           -> Just (depTy, v)

cExprTranslateMacroType ::
     TypecheckedMacroTypeBody CExpr HsType
  -> HsType
cExprTranslateMacroType (TypecheckedMacroTypeBodyCExpr tcExpr) =
    go tcExpr.macroTypeBody
  where
    go :: T.Expr HsType -> HsType
    go = \case
      T.TypeLit lit     -> HsPrimType (convertLiteral lit)
      T.Var v           -> v
      T.App T.Pointer t -> ptrOf t
      T.App T.Const   t -> go t

    ptrOf :: T.Expr HsType -> HsType
    ptrOf (T.App T.Const t) = HsPtrConst (go t)
    ptrOf t                 = HsPtr (go t)

    convertLiteral :: CExpr.TypeLit -> HsPrimType
    convertLiteral = \case
      CExpr.TypeInt  sign size -> Type.primType $ C.PrimIntegral (convertIntSize size) (convertSign sign)
      CExpr.TypeChar sign      -> Type.primType $ C.PrimChar (convertCharSign sign)
      CExpr.TypeFloat size     -> Type.primType $ C.PrimFloating (convertFloatSize size)
      CExpr.TypeBool           -> Type.primType C.PrimBool
      CExpr.TypeVoid           -> HsPrimVoid

    convertSign :: Maybe CExpr.Sign -> C.PrimSign
    convertSign = \case
      Nothing             -> C.Signed
      Just CExpr.Signed   -> C.Signed
      Just CExpr.Unsigned -> C.Unsigned

    convertCharSign :: Maybe CExpr.Sign -> C.PrimSignChar
    convertCharSign = \case
      Nothing             -> C.PrimSignImplicit Nothing
      Just CExpr.Signed   -> C.PrimSignExplicit C.Signed
      Just CExpr.Unsigned -> C.PrimSignExplicit C.Unsigned

    convertIntSize :: Maybe CExpr.IntSize -> C.PrimIntType
    convertIntSize = \case
      Nothing                 -> C.PrimInt
      Just CExpr.SizeShort    -> C.PrimShort
      Just CExpr.SizeInt      -> C.PrimInt
      Just CExpr.SizeLong     -> C.PrimLong
      Just CExpr.SizeLongLong -> C.PrimLongLong

    convertFloatSize :: CExpr.FloatSize -> C.PrimFloatType
    convertFloatSize = \case
      CExpr.SizeFloat  -> C.PrimFloat
      CExpr.SizeDouble -> C.PrimDouble

cExprTranslateMacroValue ::
     Hs.Name Hs.NsVar
  -> TypecheckedMacroValueBody CExpr Hs.TermName
  -> Maybe HsDoc.Comment
  -> Binding
cExprTranslateMacroValue name (TypecheckedMacroValueBodyCExpr expr) comment =
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
    CExpr.CharLitTyCon                     -> TGlobal $ charLitGlobalType CharValue_type
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
    lambdaN (Vec.reverse (nameToHint <$> params)) (mexpr body)

nameToHint :: CExpr.Name -> NameHint
nameToHint nm =
    toHint $ mangleCandidateDefaultFallback fallback nm.getName
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
charLiteral lit = EChar (CExpr.charLiteralValue lit)

stringLiteral :: CExpr.StringLiteral -> SExpr ctx
stringLiteral lit =
    ECString $ foldMap CChar.charValue (CExpr.stringLiteralValue lit)

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

tAppN :: Foldable f => SType ctx -> f (SType ctx) -> SType ctx
tAppN = foldl' TApp

lambdaN :: Vec ctx NameHint -> SExpr ctx -> SExpr EmptyCtx
lambdaN VNil       expr = expr
lambdaN (n ::: ns) expr = lambdaN ns $ ELam n expr

{-------------------------------------------------------------------------------
  Auxiliary: de Bruijn
-------------------------------------------------------------------------------}

vecSize :: Vec n a -> Size n
vecSize VNil       = SZ
vecSize (_ ::: xs) = SS (vecSize xs)

addIndices :: Vec n a -> Vec n (a, Idx n)
addIndices VNil       = VNil
addIndices (x ::: xs) = (x, IZ) ::: fmap (second IS) (addIndices xs)

convertTagKind :: CExpr.TagKind -> CTagKind
convertTagKind = \case
  CExpr.TagStruct -> CTagKindStruct
  CExpr.TagUnion  -> CTagKindUnion
  CExpr.TagEnum   -> CTagKindEnum
