-- | Translation from the Haskell AST to the backend representation
module HsBindgen.Backend.Common.Translation (toBE) where

import Data.Coerce
import Data.Foldable
import Data.Vec.Lazy (Vec(..))

import HsBindgen.Backend.Common
import HsBindgen.C.AST qualified as C (MFun(..))
import HsBindgen.C.Tc.Macro (DataTyCon(..), ClassTyCon(..))
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Translation.LowLevel
  (integralTyp, floatingTyp)
import HsBindgen.Util.PHOAS
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Translate to backend-specific type
-------------------------------------------------------------------------------}

class Backend be => ToBE be (a :: PHOAS) where
  type Rep be a :: Star
  type Rep be a = Expr be

  toBE :: be -> a (Fresh be) -> M be (Rep be a)

class    (ToBE be a, Rep be a ~ Expr be) => DefToBE be a
instance (ToBE be a, Rep be a ~ Expr be) => DefToBE be a

{-------------------------------------------------------------------------------
  Variable binding
-------------------------------------------------------------------------------}

instance DefToBE be a => ToBE be (Hs.Lambda a) where
  toBE be (Hs.Lambda k) = fresh be "x" $ \x ->
      lambda be (Just x) <$> toBE be (k x)

instance (DefToBE be a, DefToBE be b) => ToBE be (Hs.Ap a b) where
  toBE be (Hs.Ap f stmts) = idiom' be <$> toBE be f <*> mapM (toBE be) stmts

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

instance Backend be => ToBE be Hs.Decl where
  type Rep be Hs.Decl = Decl be
  toBE be (Hs.DeclData d) = mkDecl be <$> toBE be d
  toBE be (Hs.DeclNewtype n) = mkDecl be <$> return (newtypeToBE be n)
  toBE be (Hs.DeclInstance i) = inst be <$> toBE be i
  toBE be (Hs.DeclNewtypeInstance tc c) = mkDecl be <$> return (newtypeInstance be tc c)
  toBE be (Hs.DeclVar v) = var be v

instance Backend be => ToBE be Hs.InstanceDecl where
  type Rep be Hs.InstanceDecl = Instance be
  toBE be (Hs.InstanceStorable i) = toBE be i

instance Backend be => ToBE be (Hs.WithStruct Hs.DataDecl) where
  type Rep be (Hs.WithStruct Hs.DataDecl) = SDecl be

  toBE _be (Hs.WithStruct struct Hs.MkDataDecl) = do
    return $ DRecord $ Record
      { dataType = Hs.structName struct
      , dataCon  = Hs.structConstr struct
      , dataFields =
          [ (n, typeToBE t)
          | (n, t) <- toList $ Hs.structFields struct
          ]
      }

newtypeToBE :: be -> Hs.Newtype -> SDecl be
newtypeToBE _ n =
  DNewtype $ Newtype
    { newtypeName  = Hs.newtypeName n
    , newtypeCon   = Hs.newtypeConstr n
    , newtypeField = Hs.newtypeField n
    , newtypeType  = typeToBE (Hs.newtypeType n)
    }

newtypeInstance :: be -> Hs.TypeClass -> HsName NsTypeConstr -> SDecl be
newtypeInstance be tc n = DDerivingNewtypeInstance $ TApp (typeclass be tc) (TCon n)

typeclass :: be -> Hs.TypeClass -> SType be
typeclass _ Hs.Storable = TGlobal Storable_Storable

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

typeToBE :: Hs.HsType -> SType be
typeToBE (Hs.HsPrimType t)     = TGlobal (PrimType t)
typeToBE (Hs.HsTypRef r)       = TCon r
typeToBE (Hs.HsPtr t)          = TApp (TGlobal Foreign_Ptr) (typeToBE t)
typeToBE (Hs.HsConstArray n t) = TGlobal ConstantArray `TApp` TLit n `TApp` (typeToBE t)
typeToBE (Hs.HsType _)         = TGlobal (PrimType HsPrimVoid)

{-------------------------------------------------------------------------------
  'Storable'
-------------------------------------------------------------------------------}

instance Backend be => ToBE be (Hs.WithStruct Hs.StorableInstance) where
  type Rep be (Hs.WithStruct Hs.StorableInstance) = Instance be

  toBE be (Hs.WithStruct struct Hs.StorableInstance{
          storableSizeOf
        , storableAlignment
        , storablePeek
        , storablePoke
        }) = do
      peek <- toBE be storablePeek
      poke <- toBE be storablePoke
      return $ Instance {
          instanceClass = Storable_Storable
        , instanceType  = Hs.structName struct
        , instanceDecs  = [
              (Storable_sizeOf    , ELam Nothing $ EInt storableSizeOf)
            , (Storable_alignment , ELam Nothing $ EInt storableAlignment)
            , (Storable_peek      , EInj peek)
            , (Storable_poke      , EInj poke)
            ]
        }

instance Backend be => ToBE be Hs.PeekByteOff where
  toBE be (Hs.PeekByteOff ptr i) = return . mkExpr be $
      appMany Storable_peekByteOff [EVar ptr, EInt i]

instance Backend be => ToBE be Hs.PokeByteOff where
  toBE be (Hs.PokeByteOff ptr i x) = return . mkExpr be $
      appMany Storable_pokeByteOff [EVar ptr, EInt i, EVar x]

{-------------------------------------------------------------------------------
  Statements
-------------------------------------------------------------------------------}

instance DefToBE be a => ToBE be (Hs.Seq a) where
  toBE be (Hs.Seq (List stmts)) = doAll be <$> mapM (toBE be) stmts

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

instance Backend be => ToBE be (Hs.IntroStruct n) where
  toBE be (Hs.IntroStruct struct) = return $
      mkExpr be $ ECon $ Hs.structConstr struct

instance DefToBE be a => ToBE be (Hs.ElimStruct n a) where
  toBE be (Hs.ElimStruct struct k) =
      fresh    be "x"        $ \x ->
      freshVec be fieldNames $ \fs -> do
        k' <- toBE be (k fs)
        return $ mkExpr be $ ELam (Just x) $ ECase (EVar x) [
            (Hs.structConstr struct, toList fs, EInj k')
          ]
    where
      fieldNames :: Vec n (HsName NsVar)
      fieldNames = fst <$> Hs.structFields struct

{-------------------------------------------------------------------------------
  Variable declarations
-------------------------------------------------------------------------------}

instance Backend be => ToBE be (Hs.VarDeclRHS) where
  toBE be = fmap (mkExpr be) . varRHS be

{-------------------------------------------------------------------------------
  Internal auxiliary: derived functionality
-------------------------------------------------------------------------------}

-- | Apply function to many arguments
appMany :: Global -> [SExpr be] -> SExpr be
appMany = foldl' EApp . EGlobal

-- | Idiom brackets
idiom :: SExpr be -> [SExpr be] -> SExpr be
idiom f = foldl' (EInfix Applicative_seq) (EApp (EGlobal Applicative_pure) f)

-- | Idiom brackets
idiom' :: forall be. Backend be => be -> Expr be -> [Expr be] -> Expr be
idiom' be f = mkExpr be . idiom (EInj f) . map EInj

-- | Construct simple lambda abstraction
lambda :: Backend be => be -> Maybe (Fresh be Bound) -> Expr be -> Expr be
lambda be x = mkExpr be . ELam x . EInj

-- | Simple instance declaration
inst :: Backend be => be -> Instance be -> Decl be
inst be i = mkDecl be $ DInst i

-- | Simple variable declaration
var :: Backend be => be -> Hs.VarDecl (Fresh be) -> M be (Decl be)
var be (Hs.VarDecl nm ty rhs) = do
  ty'  <- varType be ty
  rhs' <- varRHS be rhs
  return $
    mkDecl be $ DVar nm (Just ty') rhs'

varType :: forall be. Backend be => be -> Hs.SigmaType (Fresh be) -> M be (SType be)
varType be (Hs.ForallTy qtvs (Hs.Forall k)) =
  -- TODO: hacky coerce :: HsName 'NsTypeVar -> HsName 'NsVar
  freshVec be (coerce qtvs) $ \ tvs -> do
    let Hs.QuantTy ctxt body = k tvs
    return $
      TForall (toList $ fmap getFresh tvs) (map goCt ctxt) (goTy body)
  where
    goTy :: Hs.TauType (Fresh be) -> SType be
    goTy = \case
      Hs.FunTy a b -> TFunTy (goTy a) (goTy b)
      Hs.TyVarTy tv -> TTyVar tv
      Hs.TyConAppTy (Hs.TyConApp tc args) ->
        foldl' ( \ a b -> TApp a ( goTy b ) ) (tyConGlobal tc) args
    goCt :: Hs.ClassTy (Fresh be) -> SType be
    goCt (Hs.ClassTy cls args) =
      foldl' ( \ a b -> TApp a ( goTy b ) ) (TGlobal $ classGlobal cls) args

tyConGlobal :: DataTyCon n -> SType be
tyConGlobal = \case
  BoolTyCon             -> mkPrimTy HsPrimCBool
  StringTyCon           -> TApp (TGlobal Foreign_Ptr) (mkPrimTy HsPrimCChar)
  IntLikeTyCon   inty   -> mkPrimTy $ integralTyp inty
  FloatLikeTyCon floaty -> mkPrimTy $ floatingTyp floaty
  PrimTyTyCon           -> error "tyConGlobal PrimTyTyCon"
  EmptyTyCon            -> error "tyConGlobal EmptyTyCon"
  where
    mkPrimTy = TGlobal . PrimType

classGlobal :: ClassTyCon n -> Global
classGlobal = \case
  EqTyCon         -> Eq_class
  OrdTyCon        -> Ord_class
  NumTyCon        -> Num_class
  IntegralTyCon   -> Integral_class
  FractionalTyCon -> Fractional_class
  DivTyCon        -> Div_class
  BitsTyCon       -> Bits_class

varRHS :: Backend be => be -> Hs.VarDeclRHS (Fresh be) -> M be (SExpr be)
varRHS be = \case
  Hs.VarDeclIntegral i ty ->
    return $ EIntegral i ty
  Hs.VarDeclFloat f ->
    return $ EFloat f
  Hs.VarDeclDouble d ->
    return $ EDouble d
  Hs.VarDeclLambda nm (Hs.Lambda k) ->
    fresh be nm $ \ v -> do
      a <- varRHS be $ k v
      return $ ELam (Just v) a
  Hs.VarDeclApp f as ->
    foldl' EApp <$> return (appHead f) <*> traverse (varRHS be) as
  Hs.VarDeclVar varNm ->
    return $ EVar varNm

appHead :: Hs.VarDeclRHSAppHead -> SExpr be
appHead = \case
  Hs.InfixAppHead mfun ->
    EGlobal $ mfunGlobal mfun
  Hs.VarAppHead macroNm -> do
    EFreeVar macroNm

mfunGlobal :: C.MFun arity -> Global
mfunGlobal = \case
  C.MUnaryPlus  -> Base_identity -- TODO: want some function like `numId :: forall a. Num a => a -> a; numId x = x`
  C.MUnaryMinus -> Num_negate
  C.MLogicalNot -> Base_not
  C.MBitwiseNot -> Bits_complement
  C.MMult       -> Num_times
  C.MDiv        -> Div_div
  C.MRem        -> Integral_rem
  C.MAdd        -> Num_add
  C.MSub        -> Num_minus
  C.MShiftLeft  -> Bits_shiftL
  C.MShiftRight -> Bits_shiftR
  C.MRelLT      -> Ord_lt
  C.MRelLE      -> Ord_le
  C.MRelGT      -> Ord_gt
  C.MRelGE      -> Ord_ge
  C.MRelEQ      -> Eq_eq
  C.MRelNE      -> Eq_uneq
  C.MBitwiseAnd -> Bits_and
  C.MBitwiseXor -> Bits_xor
  C.MBitwiseOr  -> Bits_or
  C.MLogicalAnd -> Base_and
  C.MLogicalOr  -> Base_or

-- | Monad sequencing
doAll :: Backend be => be -> [Expr be] -> Expr be
doAll be [] = mkExpr be $ EGlobal Monad_return `EApp` EGlobal Unit_constructor
doAll be ss = mkExpr be $ foldr1 (EInfix Monad_seq) (map EInj ss)

freshVec ::
     Backend be
  => be
  -> Vec n (HsName NsVar)
  -> (Vec n (Fresh be Bound) -> M be a)
  -> M be a
freshVec  _ VNil       k = k VNil
freshVec be (x ::: xs) k = fresh    be x  $ \v ->
                           freshVec be xs $ \vs ->
                           k (v ::: vs)
