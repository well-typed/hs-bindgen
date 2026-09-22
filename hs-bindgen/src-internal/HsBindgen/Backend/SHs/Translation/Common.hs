module HsBindgen.Backend.SHs.Translation.Common (
    translateElimStruct
  , eAppMany
  , appMany
  , appManyExpr
  , structCon
  , idiom
  , lambda
  , doAll
  , asNaryEApp
  , asNaryTApp
    -- * Shift
  , shiftExpr
    -- * DeBruijn utilities
  , reverseEnv
  , idxsEnv
  , idxsPairEnv
  ) where

import Data.Foldable qualified as Foldable
import Data.Type.Equality
import Data.Type.Nat (Plus)
import DeBruijn

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.AST.Expr (FBind (..))

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

translateElimStruct ::
     (forall ctx'. t ctx' -> SExpr ctx')
  -> Hs.ElimStruct t ctx
  -> SExpr ctx
translateElimStruct f (Hs.ElimStruct x constr hints add k) = ECase
    (EBound x)
    [SAlt constr add hints (f k)]

{-------------------------------------------------------------------------------
-  Internal auxiliary: derived functionality
--------------------------------------------------------------------------------}

eAppMany :: SExpr ctx -> [SExpr ctx] -> SExpr ctx
eAppMany = Foldable.foldl' EApp

-- | Apply function to many arguments
appMany :: BindgenGlobalTerm -> [SExpr ctx] -> SExpr ctx
appMany = appManyExpr . eBindgenGlobal

appManyExpr :: SExpr ctx -> [SExpr ctx] -> SExpr ctx
appManyExpr = Foldable.foldl' EApp

-- | Struct constructor
structCon :: Hs.StructCon ctx -> SExpr ctx
structCon (Hs.StructCon s) = ECon s.constr

-- | Idiom brackets
idiom :: (pure ctx -> SExpr ctx) -> (xs ctx -> SExpr ctx) -> Hs.Ap pure xs ctx -> SExpr ctx
idiom f g (Hs.Ap p xs) = Foldable.foldl'
    (\ acc x -> EInfix InfixApplicative_seq acc (g x))
    (EApp (eBindgenGlobal Applicative_pure) (f p))
    xs

-- | Translate lambda
lambda :: (t (S ctx) -> SExpr (S ctx)) -> Hs.Lambda t ctx -> SExpr ctx
lambda f (Hs.Lambda hint t) = ELam hint (f t)

-- | Monad sequencing
doAll :: (t ctx -> SExpr ctx) -> Hs.Seq t ctx -> SExpr ctx
doAll _ (Hs.Seq []) = eBindgenGlobal Monad_return `EApp` EUnit
doAll f (Hs.Seq ss) = foldr1 (EInfix InfixMonad_seq) (map f ss)

-- Recognize n-ary function applications
--
-- Arguments are returned from left to right.
asNaryEApp :: SExpr ctx -> (SExpr ctx, [SExpr ctx])
asNaryEApp = go []
  where
    go acc (EApp f x) = go (x : acc) f
    go acc e          = (e, acc)

-- Recognize n-ary type applications
--
-- Arguments are returned from left to right.
asNaryTApp :: SType ctx -> (SType ctx, [SType ctx])
asNaryTApp = go []
  where
    go acc (TApp f x) = go (x : acc) f
    go acc t          = (t, acc)

{-------------------------------------------------------------------------------
  Shift
--------------------------------------------------------------------------------}

-- | Shift an expression
--
-- Typically an expression needs to be shifted to accomodate newly bound
-- variables. For example, this happens when an expression is moved under a
-- lambda.
shiftExpr :: SExpr ctx -> SExpr (S ctx)
shiftExpr = shiftNExpr (AS AZ)

-- | Shift an expression @n@ times
--
-- Typically an expression needs to be shifted to accomodate newly bound
-- variables. For example, this happens when an expression is moved under a
-- lambda.
shiftNExpr :: Add n ctx ctx' -> SExpr ctx -> SExpr ctx'
shiftNExpr add = \case
    EGlobal g -> EGlobal g
    EBound idx -> EBound (shiftIdxN add idx)
    EFree tn -> EFree tn
    ECon c -> ECon c
    EUnboxedIntegral i -> EUnboxedIntegral i
    EIntegral i mct -> EIntegral i mct
    EFloat f ct -> EFloat f ct
    EDouble d ct -> EDouble d ct
    ECChar c -> ECChar c
    EString s -> EString s
    ECString bs -> ECString bs
    EApp f g -> EApp (shiftNExpr add f) (shiftNExpr add g)
    EInfix io e1 e2 -> EInfix io (shiftNExpr add e1) (shiftNExpr add e2)
    ELam nh e -> ELam nh (shiftNExpr (shiftAdd add) e)
    EUnusedLam e -> EUnusedLam (shiftNExpr add e)
    ECase e as -> ECase (shiftNExpr add e) (map (shiftNAlt add) as)
    EUnit -> EUnit
    EBoxedTup p -> EBoxedTup p
    EUnboxedTup p -> EUnboxedTup p
    EList es -> EList (fmap (shiftNExpr add) es)
    ETypeApp e t -> ETypeApp (shiftNExpr add e) t
    ERecCon n fbs -> ERecCon n (fmap (shiftNFBind add) fbs)

shiftAdd :: Add n ctx ctx' -> Add n (S ctx) (S ctx')
shiftAdd = shiftNAdd (AS AZ) -- equivalent to rsuccAdd

shiftNAdd :: Add n ctx1 ctx2 -> Add m ctx ctx' -> Add m (Plus n ctx) (Plus n ctx')
shiftNAdd = \case
  AZ -> id
  AS x -> rsuccAdd . shiftNAdd x

shiftIdxN :: Add n ctx ctx' -> Idx ctx -> Idx ctx'
shiftIdxN add idx = case add of
    AZ -> idx
    AS add' -> IS (shiftIdxN add' idx)

shiftNFBind :: Add n ctx ctx' -> FBind ctx -> FBind ctx'
shiftNFBind add fb = FBind {
      label = fb.label
    , expr  = shiftNExpr add fb.expr
    }

shiftNAlt :: forall n ctx ctx'. Add n ctx ctx' -> SAlt ctx -> SAlt ctx'
shiftNAlt add = \case
    SAlt @m @_ @ctx'' n (a :: Add m ctx ctx'') nhs (e :: SExpr ctx'') ->
        case eqSymmetry add of
          Refl -> -- ctx' :~: Plus n ctx
            let
              a' :: Add m (Plus n ctx) (Plus n ctx'')
              a' = shiftNAdd add a
              add' :: Add n ctx'' (Plus n ctx'')
              add' = eqReflexivity (addToSize add)
              e' :: SExpr (Plus n ctx'')
              e' = shiftNExpr add' e
            in
              SAlt n a' nhs e'
    SAltNoConstr ns e ->
      SAltNoConstr ns (shiftNExpr (shiftAdd add) e)
    SAltUnboxedTuple a ns e ->
      -- NOTE: same proof as the 'SAlt' case
      case eqSymmetry add of
          Refl ->
            let
              a' = shiftNAdd add a
              add' = eqReflexivity (addToSize add)
              e' = shiftNExpr add' e
            in
              SAltUnboxedTuple a' ns e'

-- | Symmetry of equality (roughly)
--
-- \[
--  forall n m p. n + m = p \implies p = n + m
-- \]
--
eqSymmetry :: Add n m p -> p :~: Plus n m
eqSymmetry = \case
  AZ -> Refl
  AS x -> case eqSymmetry x of
    Refl -> Refl

-- | Reflexivity of equality (roughly)
--
-- \[
--  forall n m. n + m = n + m
-- \]
eqReflexivity :: Size n -> Add n m (Plus n m)
eqReflexivity = \case
  SZ -> AZ
  SS x -> lsuccAdd $ eqReflexivity x

{-------------------------------------------------------------------------------
  DeBruijn utilities
--------------------------------------------------------------------------------}

-- | Reverse an 'Env'
reverseEnv :: forall ctx a. Env ctx a -> Env ctx a
reverseEnv = \env -> go (lzeroAdd (sizeEnv env)) EmptyEnv env
  where
    go ::
         forall ctx1 ctx2 ctx3.
         -- | Proof that the output environment's size is the same as the sum of
         -- the two input environment's sizes
         Add ctx1 ctx2 ctx3
         -- | Accumulator
      -> Env ctx1 a
         -- | Input to reverse
      -> Env ctx2 a
      -> Env ctx3 a
    go proof acc EmptyEnv = case unrzeroAdd proof of
        Refl -> acc
    go proof acc (xs :> x) = go (swapAdd proof) (acc :> x) xs

idxsEnv :: Env ctx a -> Env ctx (Idx ctx)
idxsEnv env = tabulateEnv (sizeEnv env) id

idxsPairEnv :: Env ctx a -> Env ctx (Idx ctx, a)
idxsPairEnv env = zipWithEnv (,) (idxsEnv env) env
