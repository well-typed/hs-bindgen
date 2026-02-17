{-# LANGUAGE MagicHash #-}

-- | Simplified HS abstract syntax tree
module HsBindgen.Backend.SHs.AST (
    ClosedExpr
  , SExpr (..)
  , eBindgenGlobal
  , eInt
  , InfixOp(..)
  , infixOpGlobal
  , SAlt (..)
  , PatExpr (..)
    -- TODO: drop S prefix?
  , SDecl (..)
  , TypeSynonym (..)
  , Pragma (..)
  , ClosedType
  , SType (..)
  , tBindgenGlobal
  , Binding (..)
  , Instance (..)
  , Field (..)
  , Record (..)
  , EmptyData (..)
  , DerivingInstance (..)
  , Newtype (..)
  , ForeignImport (..)
  , Safety (..)
  , Parameter (..)
  , Result (..)
  , PatternSynonym (..)
  ) where

import Data.Type.Nat (Nat1)
import DeBruijn (Add, Ctx, EmptyCtx, Idx)

import C.Char qualified as CExpr.Runtime

import HsBindgen.Backend.Global
import HsBindgen.Backend.Hs.AST.Strategy qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

{-------------------------------------------------------------------------------
  Backend representation
-------------------------------------------------------------------------------}

type ClosedExpr = SExpr EmptyCtx

-- | Simple expressions
type SExpr :: Ctx -> Star
data SExpr ctx =
    EGlobal (Global GExpr)
  | EBound (Idx ctx)
  | EFree (Hs.Name Hs.NsVar)
  | ECon (Hs.Name Hs.NsConstr)
  | EUnboxedIntegral Integer
  | EIntegral Integer (Maybe ClosedType)
  | EFloat Float ClosedType   -- ^ Type annotation to distinguish Float/CFloat
  | EDouble Double ClosedType -- ^ Type annotation to distinguish Double/CDouble
  | EChar CExpr.Runtime.CharValue
  | EString String
  | ECString ByteArray
  | EApp (SExpr ctx) (SExpr ctx)
  | EInfix InfixOp (SExpr ctx) (SExpr ctx)
  | ELam NameHint (SExpr (S ctx))
  | EUnusedLam (SExpr ctx)
  | ECase (SExpr ctx) [SAlt ctx]
  -- TODO https://github.com/well-typed/hs-bindgen/issues/1714.
  | EBoxedOpenTup Natural
  -- TODO https://github.com/well-typed/hs-bindgen/issues/1714.
  | EBoxedClosedTup [SExpr ctx]
  | EUnboxedTup [SExpr ctx]
  | EList [SExpr ctx]
    -- | Type application using \@
  | ETypeApp (SExpr ctx) ClosedType
  deriving stock (Show)

eBindgenGlobal :: BindgenGlobalExpr -> SExpr ctx
eBindgenGlobal = EGlobal . bindgenGlobalExpr

-- | Pattern&Expressions
--
-- There is common sublanguage between term and pattern expressions.
-- This is that common sublanguage.
-- Expressions ('SExpr') and full patterns (may) have more cases,
-- but for bidirectional pattern synonyms only the common bases
-- can be used.
--
-- For now 'PatExpr' is quite small, as we don't need much.
type PatExpr :: Star
data PatExpr
  = PEApps (Hs.Name Hs.NsConstr) [PatExpr] -- head of pattern application cannot be variable.
  | PELit Integer
  deriving stock (Show)

eInt :: Int -> SExpr be
eInt i = EIntegral (fromIntegral i) (Just $ tBindgenGlobal Int_type)

-- | Supported infix operators.
data InfixOp =
    InfixApplicative_seq
  | InfixMonad_seq
  | InfixNonEmpty_constructor
  deriving stock (Show, Eq)

infixOpGlobal :: InfixOp -> Global GExpr
infixOpGlobal = bindgenGlobalExpr . \case
  InfixApplicative_seq      -> Applicative_seq
  InfixMonad_seq            -> Monad_seq
  InfixNonEmpty_constructor -> NonEmpty_constructor

-- | Case alternatives
--
-- Represents different kinds of pattern matching alternatives in case expressions.
--
-- Examples:
--
-- > case x of
-- >   Just y -> ...     -- SAlt with constructor "Just"
-- >   5# -> ...         -- SAltNoConstr for primitive literal
-- >   (# a, b #) -> ... -- SAltUnboxedTuple
--
data SAlt ctx where
    -- | Constructor pattern: @Con x y z -> body@
    --
    -- Pattern matches against a data constructor with fields.
    SAlt
      :: Hs.Name Hs.NsConstr -> Add n ctx ctx' -> Vec n NameHint -> SExpr ctx' -> SAlt ctx
    -- | Non-constructor pattern: @5# -> body@ or @x -> body@
    --
    -- Used for matching primitive values, literals, or catch-all variables.
    -- Always binds exactly one variable (hence Vec Nat1).
    SAltNoConstr
      :: Vec Nat1 NameHint -> SExpr (S ctx) -> SAlt ctx
    -- | Unboxed tuple pattern: @(# x, y #) -> body@
    --
    -- Pattern matches against unboxed tuples.
    SAltUnboxedTuple
      :: Add n ctx ctx' -> Vec n NameHint -> SExpr ctx' -> SAlt ctx

deriving stock instance Show (SAlt ctx)

-- | Simple declarations
data SDecl =
    DTypSyn TypeSynonym
  | DInst Instance
  | DRecord Record
  | DNewtype Newtype
  | DEmptyData EmptyData
  | DDerivingInstance DerivingInstance
  | DForeignImport ForeignImport
  | DBinding Binding
  | DPatternSynonym PatternSynonym
  deriving stock (Show)

type ClosedType = SType EmptyCtx

-- | Simple types
type SType :: Ctx -> Star
data SType ctx =
    TGlobal (Global GTyp)
  | TClass Inst.TypeClass
  | TCon (Hs.Name Hs.NsTypeConstr)
  | TFun (SType ctx) (SType ctx)
  | TLit Natural
  | TStrLit String
  | TExt Hs.ExtRef BindingSpec.CTypeSpec BindingSpec.HsTypeSpec
  | TBound (Idx ctx)
  | TFree (Hs.Name Hs.NsVar)
  | TApp (SType ctx) (SType ctx)
  -- TODO https://github.com/well-typed/hs-bindgen/issues/1714.
  | TBoxedOpenTup Natural
  | TEq
  | forall n ctx'. TForall (Vec n NameHint) (Add n ctx ctx') [SType ctx'] (SType ctx')

tBindgenGlobal :: BindgenGlobalType -> SType ctx
tBindgenGlobal = TGlobal . bindgenGlobalType

data Pragma = NOINLINE
  deriving stock Show

infixl 9 `TApp`

deriving stock instance Show (SType ctx)

data TypeSynonym = TypeSynonym{
      name    :: Hs.Name Hs.NsTypeConstr
    , typ     :: ClosedType
    , origin  :: Origin.Decl Origin.EmptyData
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Instance = Instance{
      clss    :: Inst.TypeClass
    , args    :: [ClosedType]
    , super   :: [ClosedType]
    , types   :: [(Global GTyp, [ClosedType], ClosedType)]
    , decs    :: [(Global GExpr, ClosedExpr)]
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Field = Field{
      name   :: Hs.Name Hs.NsVar
    , typ    :: ClosedType
    , origin :: Origin.Field
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Record = Record{
      typ     :: Hs.Name Hs.NsTypeConstr
    , con     :: Hs.Name Hs.NsConstr
    , fields  :: [Field]
    , origin  :: Origin.Decl Origin.Struct
    , deriv   :: [(Hs.Strategy ClosedType, [Inst.TypeClass])]
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data EmptyData = EmptyData{
      name    :: Hs.Name Hs.NsTypeConstr
    , origin  :: Origin.Decl Origin.EmptyData
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data DerivingInstance = DerivingInstance{
      strategy :: Hs.Strategy ClosedType
    , typ      :: ClosedType
    , comment  :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Newtype = Newtype{
      name    :: Hs.Name Hs.NsTypeConstr
    , con     :: Hs.Name Hs.NsConstr
    , field   :: Field
    , origin  :: Origin.Decl Origin.Newtype
    , deriv   :: [(Hs.Strategy ClosedType, [Inst.TypeClass])]
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

-- | We might want to reconsider the decision of 'foreignImportParameters' as
-- well as 'foreignImportResultType' being 'ClosedType's if we ever want to
-- generate polymorphic type signatures.
--
data ForeignImport = ForeignImport{
      name       :: Hs.Name Hs.NsVar
    , parameters :: [Parameter]
    , result     :: Result
    , origName   :: C.DeclName
    , callConv   :: CallConv
    , origin     :: Origin.ForeignImport
    , comment    :: Maybe HsDoc.Comment
    , safety     :: Safety
    }
  deriving stock (Show, Generic)

-- | Safety of foreign import declarations
data Safety = Safe | Unsafe
  deriving stock (Show, Eq, Generic)

instance Default Safety where
  def = Safe

data Binding = Binding{
      name       :: Hs.Name Hs.NsVar
    , parameters :: [Parameter]
    , result     :: Result
    , body       :: ClosedExpr
    , pragmas    :: [Pragma]
    , comment    :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Parameter = Parameter{
      typ     :: ClosedType
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Result = Result{
      typ     :: ClosedType
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data PatternSynonym = PatternSynonym{
      name    :: Hs.Name Hs.NsConstr
    , typ     :: ClosedType
    , rhs     :: PatExpr
    , origin  :: Origin.PatSyn
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)
